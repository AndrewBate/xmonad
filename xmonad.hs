
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad hiding (Connection)
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W


main :: IO ()
main = do 
  dbus <- D.connectSession
  getWellKnownName dbus
  spawn "gnome-settings-daemon"
  spawn "gnome-panel"
  xmonad $ gnomeConfig {
    focusedBorderColor = "DarkBlue"
  , borderWidth        = 3
  , manageHook         = manageHook gnomeConfig <+> composeAll managementHooks
  , logHook            = dynamicLogWithPP (myPrettyPrinter dbus)
  , layoutHook         = layoutHook gnomeConfig ||| Accordion ||| Grid
  , modMask            = mod4Mask
  , terminal           = "gnome-terminal"
  }
    -- swap w & e in screen selection as this computer is reversed
    `additionalKeys`     [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
                            | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
                            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    `additionalKeysP` [("M-m",spawn "/home/anbate/remacs.sh")
                      ,("M-i", spawn "google-chrome")
                      ,("M-f", spawn "nautilus")
                      ,("M-n", spawn "thunderbird")
                      ,("M-o", spawn "xrandr -o left")
                      ]



-- -----------------------------------------------------------------------------

myPrettyPrinter :: D.Client -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = dbusOutput dbus
  , ppTitle   = pangoColor "yellow" . shorten 50 . pangoSanitize
  , ppCurrent = pangoColor "red" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "green" . wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppUrgent  = pangoColor "red"
  }

managementHooks :: [ManageHook]
managementHooks = [
    resource  =? "Do"        --> doIgnore
  , className =? "rdesktop"  --> doFloat
  ]

-- -----------------------------------------------------------------------------

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


