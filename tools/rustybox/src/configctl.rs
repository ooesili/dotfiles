use anyhow::{anyhow, bail, ensure, Context, Result};
use std::ffi::OsStr;
use std::os::unix;
use std::process::Command;
use std::{
    collections::HashMap,
    env, fs, io,
    path::{Path, PathBuf},
};

pub fn main(mut args: env::Args) -> Result<()> {
    match args.next() {
        Some(arg) => match arg.as_str() {
            "checkout" => checkout(args),
            "diff" => diff(args),
            "init" => init(args),
            "reset" => reset(args),
            "status" => status(args),
            _ => bail!("unrecognized command: {}", arg),
        },
        _ => bail!("no command given"),
    }
}

fn init(_args: env::Args) -> Result<()> {
    let runtime_dir = env::var("XDG_RUNTIME_DIR").context("XDG_RUNTIME_DIR not set")?;
    let link_dir = Path::new(&runtime_dir).join("configctl");

    let config_set = ConfigSet::gather()?;

    for (name, file) in config_set.spec.files.iter() {
        match config_set.status.files.get(name) {
            Some(actual_file) if actual_file == file => {}
            _ => {
                let link_file = link_dir.join(name);
                println!("creating link: {} -> {}", name, link_file.to_string_lossy());
                replace_symlink(file, &link_file)?;
            }
        }
    }

    Ok(())
}

fn checkout(mut args: env::Args) -> Result<()> {
    let name = match args.next() {
        Some(name) => name,
        None => bail!("no config file name given"),
    };
    // let dest = args.next().unwrap_or_else(|| ".".to_string());

    let config_set = ConfigSet::gather()?;

    let source_file = match config_set.spec.files.get(&name) {
        Some(source_file) => source_file,
        None => bail!("no config file named '{}'", name),
    };

    let runtime_dir = env::var("XDG_RUNTIME_DIR").context("XDG_RUNTIME_DIR not set")?;
    let link_dir = Path::new(&runtime_dir).join("configctl");

    let checkout_file = env::current_dir().context("getting pwd")?.join(&name);
    let link_file = link_dir.join(&name);
    replace_symlink(&checkout_file, &link_file)?;

    // Manually copy to use current umask instead of nix store's 0444 permissions.
    let mut source = fs::File::open(&source_file).context("opening source file for reading")?;
    let mut dest = fs::File::create(&checkout_file).context("opening dest file for writing")?;
    io::copy(&mut source, &mut dest).context("copying bytes to dest file")?;

    Ok(())
}

fn replace_symlink(original: &Path, link: &Path) -> Result<()> {
    let dir = link.parent().ok_or_else(|| anyhow!("link has no parent"))?;
    let filename = link
        .file_name()
        .ok_or_else(|| anyhow!("link has no filename"))?;
    let link_filename = filename
        .to_str()
        .ok_or_else(|| anyhow!("path is not utf-8"))?;
    let temp_link = dir.join(format!(".{}.tmp", link_filename));

    unix::fs::symlink(original, &temp_link)
        .with_context(|| format!("overwriting symlink: {}", temp_link.to_string_lossy()))?;
    fs::rename(temp_link, link).context("moving temp link into place")
}

fn reset(mut args: env::Args) -> Result<()> {
    let runtime_dir = env::var("XDG_RUNTIME_DIR").context("XDG_RUNTIME_DIR not set")?;
    let link_dir = Path::new(&runtime_dir).join("configctl");

    let config_set = ConfigSet::gather()?;

    if let Some(name) = args.next() {
        let file = match config_set.spec.files.get(&name) {
            Some(file) => file,
            None => bail!("no config named {}", name),
        };

        match config_set.status.files.get(&name) {
            Some(actual_file) if actual_file == file => {
                println!("already reset");
            }
            _ => {
                println!("resetting {}", name);
                replace_symlink(file, &link_dir.join(name))?
            }
        }

        return Ok(());
    }

    for (name, file) in config_set.spec.files.iter() {
        match config_set.status.files.get(name) {
            Some(actual_file) if actual_file == file => {}
            _ => {
                println!("resetting {}", name);
                replace_symlink(file, &link_dir.join(name))?
            }
        }
    }

    Ok(())
}

fn status(_args: env::Args) -> Result<()> {
    let config_set = ConfigSet::gather()?;

    for (name, file) in config_set.spec.files.iter() {
        match config_set.status.files.get(name) {
            Some(actual_file) if actual_file == file => {
                println!("[ok     ] {name}");
            }
            Some(actual_file) => {
                println!("[dirty  ] {name} -> {}", actual_file.to_string_lossy());
            }
            None => {
                println!("[missing] {name}");
            }
        }
    }

    Ok(())
}

fn diff(_args: env::Args) -> Result<()> {
    let config_set = ConfigSet::gather()?;

    for (name, source_file) in config_set.spec.files.iter() {
        match config_set.status.files.get(name) {
            Some(file) => print_diff(&source_file, &file)?,
            None => eprintln!("warning: file is missing: {}", name),
        }
    }

    Ok(())
}

fn print_diff(file1: &Path, file2: &Path) -> Result<()> {
    Command::new("diff")
        .arg("-u")
        .arg("--")
        .arg(file1)
        .arg(file2)
        .spawn()
        .context("starting diff command")?
        .wait()
        .context("diff command failed")?;
    Ok(())
}

#[derive(Default, Debug)]
struct ConfigSet {
    spec: ConfigSetSpec,
    status: ConfigSetStatus,
}

impl ConfigSet {
    fn gather() -> Result<Self> {
        let runtime_dir = env::var("XDG_RUNTIME_DIR").context("XDG_RUNTIME_DIR not set")?;
        let link_dir = Path::new(&runtime_dir).join("configctl");
        fs::create_dir_all(&link_dir).context("creating runtime link directory")?;

        let spec = ConfigSetSpec::gather().context("gather desired config")?;
        let status = ConfigSetStatus::gather(&link_dir).context("gather actual config")?;
        Ok(Self { spec, status })
    }
}

#[derive(Default, Debug)]
struct ConfigSetSpec {
    files: HashMap<String, PathBuf>,
}

impl ConfigSetSpec {
    fn gather() -> Result<Self> {
        let xdg_config_dirs = match env::var("XDG_CONFIG_DIRS") {
            Ok(dirs) => dirs,
            Err(_) => {
                return Ok(Self {
                    files: HashMap::new(),
                })
            }
        };

        let mut files = HashMap::new();
        for xdg_config_dir in xdg_config_dirs.split(':') {
            // TODO: iterate over all of XDG_CONFIG_DIRS
            let config_dir = Path::new(xdg_config_dir).join("configctl");

            let source_files = match fs::read_dir(&config_dir) {
                Ok(files) => files,
                Err(_) => continue,
            };

            for source_file in source_files {
                let source_file =
                    source_file.with_context(|| format!("opening file in: {:?}", config_dir))?;

                let name = os_to_string(&source_file.file_name())?;

                if files.insert(name, source_file.path()).is_some() {
                    bail!(
                        "detected duplicate file: {}",
                        source_file.file_name().to_string_lossy()
                    );
                }
            }
        }

        Ok(Self { files })
    }
}

#[derive(Default, Debug)]
struct ConfigSetStatus {
    files: HashMap<String, PathBuf>,
}

impl ConfigSetStatus {
    fn gather(link_dir: &Path) -> Result<Self> {
        let mut files = HashMap::new();
        for link_file in fs::read_dir(link_dir)? {
            let link_file = link_file.context("inspecting existing link")?;
            let link_dest = fs::read_link(link_file.path())?;
            files.insert(os_to_string(&link_file.file_name())?, link_dest);
        }

        Ok(Self { files })
    }
}

fn os_to_string(file: &OsStr) -> Result<String> {
    Ok(file
        .to_str()
        .ok_or_else(|| anyhow!("filename is not valid unicode"))?
        .to_string())
}
