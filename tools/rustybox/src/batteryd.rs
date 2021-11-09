use anyhow::{anyhow, ensure, Context, Result};
use std::{env, fs, io::BufRead, process::Command, thread, time::Duration};

const UEVENT_FILE: &str = "/sys/class/power_supply/BAT1/uevent";

const CHARGE_FULL_KEYS: [&str; 1] = ["POWER_SUPPLY_CHARGE_FULL"];
const CHARGE_NOW_KEYS: [&str; 1] = ["POWER_SUPPLY_CHARGE_NOW"];
const STATUS_KEYS: [&str; 1] = ["POWER_SUPPLY_STATUS"];

pub fn main(_args: env::Args) -> Result<()> {
    monitor_battery()
}

fn monitor_battery() -> Result<()> {
    let timer = Timer::new(Duration::from_secs(30));
    let readings = BatteryReadings::new(Sysfs, timer);
    let notifications = BatteryNotifications::new(readings, vec![5.0, 10.0, 20.0]);

    for event in notifications {
        match event {
            Ok(BatteryEvent::LowBattery(threshold)) => Alerter.alert(threshold)?,
            Err(err) => eprintln!("error: reading battery info: {}", err),
        }
    }

    Ok(())
}

struct Alerter;

impl Alerter {
    fn alert(&self, threshold: f32) -> Result<()> {
        let status = Command::new("notify-send")
            .arg("--urgency=low")
            .arg(format!("--expire-time={}", 5 * 60 * 1000))
            .arg("Low Battery")
            .arg(format!("Battery is at {}%", threshold))
            .status()
            .context("sending notification with notify-send")?;
        ensure!(
            status.success(),
            "notify-send exited with a non-zero exit code"
        );
        Ok(())
    }
}

trait BatteryProvider {
    fn battery_info(&self) -> Result<BatteryReading>;
}

#[derive(Clone)]
struct BatteryReading {
    percent: f32,
    discharging: bool,
}

struct Sysfs;

impl BatteryProvider for Sysfs {
    fn battery_info(&self) -> Result<BatteryReading> {
        let mut battery_full: Option<f32> = None;
        let mut battery_now: Option<f32> = None;
        let mut discharging: Option<bool> = None;

        for line in fs::read(UEVENT_FILE)?.lines() {
            let line = line?;
            let (key, value) = line
                .split_once('=')
                .ok_or_else(|| anyhow!("no `=` found in line in {}", UEVENT_FILE))?;

            if CHARGE_FULL_KEYS.contains(&key) {
                battery_full = Some(value.parse()?)
            } else if CHARGE_NOW_KEYS.contains(&key) {
                battery_now = Some(value.parse()?)
            } else if STATUS_KEYS.contains(&key) {
                discharging = Some(value == "Discharging");
            }
        }

        match (battery_full, battery_now, discharging) {
            (Some(battery_full), Some(battery_now), Some(discharging)) => Ok(BatteryReading {
                percent: 100.0 * battery_now / battery_full,
                discharging,
            }),
            _ => Err(anyhow!(
                "could not find battery information in {}",
                UEVENT_FILE
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum BatteryEvent {
    LowBattery(f32),
}

trait Clock {
    #[must_use]
    fn sleep(&self, dur: Duration) -> Option<()>;
}

#[derive(Debug)]
struct BatteryNotifications<I> {
    high_water_mark: f32,
    readings: I,
    thresholds: Vec<f32>,
}

impl<I> BatteryNotifications<I> {
    fn new<R>(readings: R, thresholds: Vec<f32>) -> Self
    where
        R: IntoIterator<Item = Result<BatteryReading>, IntoIter = I>,
    {
        BatteryNotifications {
            high_water_mark: 100.0,
            readings: readings.into_iter(),
            thresholds,
        }
    }
}

impl<I> Iterator for BatteryNotifications<I>
where
    I: Iterator<Item = Result<BatteryReading>>,
{
    type Item = Result<BatteryEvent>;

    fn next(&mut self) -> Option<Result<BatteryEvent>> {
        loop {
            let battery = match self.readings.next() {
                Some(Ok(battery)) => battery,
                Some(Err(err)) => return Some(Err(err)),
                None => return None,
            };

            let mut event = None;
            if battery.discharging {
                for threshold in self.thresholds.iter().copied() {
                    if battery.percent <= threshold && threshold < self.high_water_mark {
                        event = Some(BatteryEvent::LowBattery(threshold));
                        break;
                    }
                }

                self.high_water_mark = self.high_water_mark.min(battery.percent);

                if let Some(event) = event {
                    return Some(Ok(event));
                }
            } else {
                self.high_water_mark = 100.0;
            }
        }
    }
}

struct Timer {
    interval: Duration,
    started: bool,
}

impl Timer {
    fn new(interval: Duration) -> Timer {
        Timer {
            interval,
            started: false,
        }
    }
}

impl Iterator for Timer {
    type Item = ();

    fn next(&mut self) -> Option<()> {
        if self.started {
            thread::sleep(self.interval);
        } else {
            self.started = true;
        }
        Some(())
    }
}

struct BatteryReadings<P, I> {
    provider: P,
    timer: I,
}

impl<P, I> BatteryReadings<P, I>
where
    P: BatteryProvider,
    I: Iterator<Item = ()>,
{
    fn new(provider: P, timer: I) -> Self {
        Self { provider, timer }
    }
}

impl<P, I> Iterator for BatteryReadings<P, I>
where
    P: BatteryProvider,
    I: Iterator<Item = ()>,
{
    type Item = Result<BatteryReading>;

    fn next(&mut self) -> Option<Result<BatteryReading>> {
        self.timer.next()?;
        Some(self.provider.battery_info())
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use super::*;

    #[test]
    fn test_monitor_battery_full_discharging() {
        run_test(
            vec![BatteryReading {
                discharging: true,
                percent: 100.0,
            }],
            vec![],
        );
    }

    #[test]
    fn test_monitor_battery_low_threshold() {
        run_test(
            vec![
                BatteryReading {
                    discharging: true,
                    percent: 16.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 15.0,
                },
            ],
            vec![BatteryEvent::LowBattery(15.0)],
        );
    }

    #[test]
    fn test_monitor_battery_almost_low_threshold() {
        run_test(
            vec![
                BatteryReading {
                    discharging: true,
                    percent: 17.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 16.0,
                },
            ],
            vec![],
        );
    }

    #[test]
    fn test_monitor_battery_unplugged_below_threshold() {
        run_test(
            vec![
                BatteryReading {
                    discharging: false,
                    percent: 13.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 13.0,
                },
            ],
            vec![BatteryEvent::LowBattery(15.0)],
        );
    }

    #[test]
    fn test_monitor_battery_unplugged_below_lower_threshold() {
        run_test(
            vec![
                BatteryReading {
                    discharging: false,
                    percent: 4.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 4.0,
                },
            ],
            vec![BatteryEvent::LowBattery(5.0)],
        );
    }

    #[test]
    fn test_monitor_battery_both_thresholds() {
        run_test(
            vec![
                BatteryReading {
                    discharging: false,
                    percent: 16.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 13.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 10.0,
                },
                BatteryReading {
                    discharging: true,
                    percent: 4.0,
                },
            ],
            vec![
                BatteryEvent::LowBattery(15.0),
                BatteryEvent::LowBattery(5.0),
            ],
        );
    }

    fn run_test(readings: Vec<BatteryReading>, expected_events: Vec<BatteryEvent>) {
        let thresholds = vec![5.0, 15.0];
        let events = BatteryNotifications::new(readings.into_iter().map(Ok), thresholds)
            .collect::<Result<Vec<_>>>()
            .unwrap();
        assert_eq!(events, expected_events);
    }

    struct MockBatterProvider {
        readings: RefCell<Box<dyn Iterator<Item = BatteryReading>>>,
    }

    impl MockBatterProvider {
        fn new(readings: impl IntoIterator<Item = BatteryReading> + 'static) -> Self {
            Self {
                readings: RefCell::new(Box::new(readings.into_iter())),
            }
        }
    }

    impl BatteryProvider for MockBatterProvider {
        fn battery_info(&self) -> Result<BatteryReading> {
            self.readings
                .borrow_mut()
                .next()
                .ok_or_else(|| anyhow!("no readings left"))
        }
    }

    #[derive(Clone)]
    struct MockClock(Rc<RefCell<MockClockInner>>);

    struct MockClockInner {
        elapsed: Duration,
        ticks: usize,
    }

    impl MockClock {
        fn ticks(ticks: usize) -> Self {
            Self(Rc::new(RefCell::new(MockClockInner {
                elapsed: Default::default(),
                ticks,
            })))
        }
    }

    impl Clock for MockClock {
        fn sleep(&self, dur: Duration) -> Option<()> {
            let mut inner = self.0.borrow_mut();
            if inner.ticks == 0 {
                return None;
            }

            inner.elapsed += dur;
            inner.ticks -= 1;
            Some(())
        }
    }
}
