import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
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
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#859900" "" . shorten 75
                        }
        , workspaces        = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeysP`
        [ ("M-d",   spawn "dwb")
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
        , ("M-a",   spawn "urxvt -e yamba")
        , ("M-e",   spawn "easytag")
        , ("M-f",   sendMessage ToggleLayout)
        , ("M-p",   shellPrompt  xoriaXPConfig)
        , ("M-o",   configPrompt home xoriaXPConfig)
        , ("M-S-a", withFocused $ keysMoveWindowTo (1918,18) (1, 0))
        ] `additionalKeys`
        [ ((0, 0x1008ff13), spawn "amixer --quiet set Master 1+")
        , ((0, 0x1008ff11), spawn "amixer --quiet set Master 1-")
        , ((0, 0x1008ff12), spawn "amixer --quiet set Master toggle")
        ]

-- simple items

myTerminal = "urxvtc"

myWorkspaces = ["home","web","apps","vim","term","android","misc","mixer","ardour"]

myNormalBorderColor = "#505050"

myFocusedBorderColor = "#0095b9"

-- layout configuration
myTabbed = noBorders $ tabbed shrinkText myTabTheme

androidLayout =
    let mainPanes  = TwoPane 0 (719/960)
        littleTerm = gaps [(U,802)] Full
        name = renamed [Replace "Android Layout"]
    in name $ combineTwo mainPanes myTabbed littleTerm

mixerLayout r =
    let mainPanes = TwoPane 0 r
        sidePane = Mirror $ TwoPane 0 (9/10)
        name = renamed [Replace "Mixer Layout"]
        notPatchOrCtl = Not $ Or (Resource "qjackctl") (Title "Audio Connection Manager")
        mixerTabs = tabbed shrinkText myTabTheme
    in name $ combineTwoP mainPanes mixerTabs sidePane notPatchOrCtl

myLayoutHook =
    let showToolbar = gaps [(U,18)]
        tiled = Tall 1 (3/100) (1/2)
        ardourLayout = renamed [Replace "Ardour Tabs"] (showToolbar myTabbed ||| myTabbed)
    in smartBorders
     . onWorkspace "ardour" ardourLayout
     . toggleLayouts (Full ||| myTabbed)
     . showToolbar
     . onWorkspace "android" androidLayout
     {-. onWorkspace "mixer" (ResizableTall 1 (3/100) (5/7) [1, 9/5, 1/5])-}
     . onWorkspace "mixer" ((mixerLayout $ 219/320) ||| (mixerLayout $ 629/960))
     $ (tiled ||| Mirror tiled ||| myTabbed)

-- tabbed layout theme
myTabTheme = defaultTheme
    { activeColor         = "#3a3a3a"
    , inactiveColor       = "#1c1c1c"
    , activeBorderColor   = "#0095b9"
    , inactiveBorderColor = "#707070"
    , activeTextColor     = "#ffffff"
    , inactiveTextColor   = "#9e9e9e"
    , fontName            = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"
    , decoHeight          = 18
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
        resource =? "ardour_preferences" <||>
        resource =? "ardour-3.2"	-?> doCenterFloat
    , return True			-?> manageDocks
    ]

-- set the look for the prompts
xoriaXPConfig = defaultXPConfig
    { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"
    , bgColor         = "#1c1c1c"
    , fgColor         = "#d0d0d0"
    , fgHLight        = "#eeeeee"
    , bgHLight        = "#657b83"
    , borderColor     = "#4e4e4e"
    , alwaysHighlight = True
    }

-- config prompt
data Config = Config

instance XPrompt Config where
    showXPrompt Config = "Config: "

configPrompt :: String -> XPConfig -> X ()
configPrompt home c = do
    configMap <- io . configParse $ home ++ "/.xmonad/configs"
    mkXPrompt Config c (mkComplFunFromList $ map fst configMap) (\key ->
        case (lookup key configMap) of (Just file) -> editSpawn file
                                       Nothing     -> return () )
    where editSpawn (':':file) = spawn $ "urxvt -e sh -c 'sudoedit " ++ file ++ "'"
          editSpawn      file  = spawn $ "gvim " ++ file

configParse :: FilePath -> IO [(String, String)]
configParse file = do
    handle <- openFile file ReadMode
    xs <- fmap lines $ hGetContents handle
    let pairs = fmap (fmap tail . span (/=':')) xs
    return pairs
