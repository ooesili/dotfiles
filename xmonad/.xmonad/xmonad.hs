import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.Gaps
import XMonad.Layout.Renamed
import XMonad.Actions.FloatKeys
import System.Environment (getEnv)

main = do
    home <- io $ getEnv "HOME"
    xmproc <- spawnPipe $ "xmobar " ++ home ++ "/.xmonad/xmobarrc"
    xmonad $ defaultConfig
        { terminal          = myTerminal
        , manageHook        = myManageHook
        , layoutHook        = myLayoutHook
        , logHook           = myLogHook xmproc
        , workspaces        = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = 1
        } `additionalKeysP`
        [ ("M-d",   spawn "firefox")
        , ("M-g",   spawn "gvim")
        , ("M-S-o", spawn "systemctl poweroff")
        , ("M-S-r", spawn "systemctl reboot")
        , ("M-S-s", spawn "systemctl suspend")
        , ("M-z",   spawn "ncmpcpp prev")
        , ("M-x",   spawn "ncmpcpp play")
        , ("M-c",   spawn "ncmpcpp pause")
        , ("M-v",   spawn "ncmpcpp stop")
        , ("M-b",   spawn "ncmpcpp next")
        , ("M-i",   spawn "inithome")
        , ("M-r",   spawn "qjackctl")
        , ("M-a",   spawn "anki")
        , ("M-e",   spawn "easytag")
        , ("M-s",   spawn "scide")
        , ("M-f",   sendMessage ToggleLayout)
        , ("M-p",   spawn "dmenu_run -fn '-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-1' -p 'run>' -nb '#002b36' -nf '#93a1a1' -sb '#dc322f' -sf '#fdf6e3'")
        , ("M-S-a", withFocused $ keysMoveWindowTo (1918,18) (1, 0))
        ---------- TMUX REMAPPINGS ----------
        -- swich panes
        , ("M4-j",   spawn "xdotool keyup j" >> spawn "xdotool key --clearmodifiers ctrl+a o")
        , ("M4-k",   spawn "xdotool keyup k" >> spawn "xdotool key --clearmodifiers ctrl+a O")
        -- move panes
        , ("M4-S-j", spawn "xdotool keyup j" >> spawn "xdotool key --clearmodifiers ctrl+a braceright")
        , ("M4-S-k", spawn "xdotool keyup k" >> spawn "xdotool key --clearmodifiers ctrl+a braceleft")
        -- switch windows
        , ("M4-n",   spawn "xdotool keyup n" >> spawn "xdotool key --clearmodifiers ctrl+a n")
        , ("M4-p",   spawn "xdotool keyup p" >> spawn "xdotool key --clearmodifiers ctrl+a p")
        ---------- VIM REMAPPINGS ----------
        -- switch panes
        , ("M3-f",   spawn "xdotool keyup f" >> spawn "xdotool key --clearmodifiers ctrl+w w")
        , ("M3-d",   spawn "xdotool keyup d" >> spawn "xdotool key --clearmodifiers ctrl+w W")
        -- close
        , ("M3-q",   spawn "xdotool keyup q" >> spawn "xdotool key --clearmodifiers ctrl+w q")
        -- split panes
        , ("M3-s",   spawn "xdotool keyup s" >> spawn "xdotool key --clearmodifiers ctrl+w s")
        , ("M3-v",   spawn "xdotool keyup v" >> spawn "xdotool key --clearmodifiers ctrl+w v")
        ] `additionalKeys`
        [ ((0, 0x1008ff13), spawn "amixer --quiet set Master 1+")
        , ((0, 0x1008ff11), spawn "amixer --quiet set Master 1-")
        , ((0, 0x1008ff12), spawn "amixer --quiet set Master toggle")
        ]

-- solarized colors
data SolarizedPalette = SolarizedPalette
    { base03  :: String
    , base02  :: String
    , base01  :: String
    , base00  :: String
    , base0   :: String
    , base1   :: String
    , base2   :: String
    , base3   :: String
    , yellow  :: String
    , orange  :: String
    , red     :: String
    , magenta :: String
    , violet  :: String
    , blue    :: String
    , cyan    :: String
    , green   :: String
    }

solarP = SolarizedPalette
    { base03 =  "#002b36"
    , base02 =  "#073642"
    , base01 =  "#586e75"
    , base00 =  "#657b83"
    , base0 =   "#839496"
    , base1 =   "#93a1a1"
    , base2 =   "#eee8d5"
    , base3 =   "#fdf6e3"
    , yellow =  "#b58900"
    , orange =  "#cb4b16"
    , red =     "#dc322f"
    , magenta = "#d33682"
    , violet =  "#6c71c4"
    , blue =    "#268bd2"
    , cyan =    "#2aa198"
    , green =   "#859900"
    }


-- simple items

myTerminal = "urxvtc"

myWorkspaces = ["home","web","apps","vim","term","android","misc","mixer","ardour"]

myNormalBorderColor = base02 solarP

myFocusedBorderColor = red solarP

-- layout configuration
myTabbed = noBorders $ tabbed shrinkText solarizedTabs

androidLayout =
    let mainPanes  = TwoPane 0 (719/960)
        littleTerm = gaps [(U,802)] Full
        name = renamed [Replace "Android Layout"]
    in name $ combineTwo mainPanes myTabbed littleTerm

mixerLayout =
    let mainPanes = TwoPane 0 (2/3)
        sidePane = Mirror $ TwoPane 0 (9/10)
        name = renamed [Replace "Mixer Layout"]
        notPatchOrCtl = Not $ Or (Resource "qjackctl") (Title "Audio Connection Manager")
        mixerTabs = tabbed shrinkText solarizedTabs
    in name $ combineTwoP mainPanes mixerTabs sidePane notPatchOrCtl

myLayoutHook =
    let showToolbar = gaps [(U,16)]
        tiled = Tall 1 (3/100) (1/2)
        ardourLayout = renamed [Replace "Ardour Tabs"] (showToolbar myTabbed ||| myTabbed)
    in smartBorders
     . onWorkspace "ardour" ardourLayout
     . toggleLayouts (Full ||| myTabbed)
     . showToolbar
     . onWorkspace "android" androidLayout
     . onWorkspace "mixer" mixerLayout
     $ (tiled ||| Mirror tiled ||| myTabbed)

-- tabbed layout theme
solarizedTabs = defaultTheme
    { activeColor         = base01 solarP
    , inactiveColor       = base02 solarP
    , activeBorderColor   = myFocusedBorderColor
    , inactiveBorderColor = myNormalBorderColor
    , activeTextColor     = base2 solarP
    , inactiveTextColor   = base0 solarP
    , fontName            = "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-1"
    , decoHeight          = 16
    }

-- manageHook configuration
myManageHook = composeOne
    [ isFullscreen			-?> doFullFloat
    , className =? "MPlayer"		-?> doFloat
    , resource =? "ardour_editor"	-?> doShift "ardour"
    , resource =? "ardour_mixer" <||>
        title =? "Audio Connection Manager" <||>
        resource =? "patchage" <||>
        resource =? "qjackctl"		-?> doShift "mixer"
    , resource =? "emulator64-arm"	-?> doFloatAt (3/4) (19/1080)
    , isDialog <||>
        resource =? "gvim" <||>
        resource =? "ardour_preferences" <||>
        resource =? "ardour-3.4"	-?> doCenterFloat
    , return True			-?> manageDocks
    ]

-- logHook configuration
myLogHook input = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn input
    , ppTitle = xmobarColor (green solarP) ""
    , ppLayout = xmobarColor (yellow solarP) ""
    , ppCurrent = xmobarColor (base2 solarP) (base01 solarP) . wrap "[" "]"
    }
