import Control.OldException(catchDyn,try)
import XMonad.Util.Run
import Control.Concurrent
import DBus
import DBus.Connection
import DBus.Message
import System.Cmd
import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W


main = withConnection Session $ \ dbus -> do
  getWellKnownName dbus
  xmonad $ gnomeConfig {
    focusedBorderColor = "DarkBlue"
  , borderWidth        = 3
  , manageHook         = manageHook gnomeConfig <+> composeAll managementHooks
  , logHook            = dynamicLogWithPP (myPrettyPrinter dbus)
  , startupHook        = startupHook gnomeConfig >> liftIO startNitrogen
  , layoutHook         = layoutHook gnomeConfig ||| Accordion ||| Grid
  , modMask            = mod4Mask
  , terminal           = "gnome-terminal"
  }
    -- swap w & e in screen selection as this computer is reversed
    `additionalKeys`     [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
                            | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
                            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- `removeKeysP`     ["M-p"]
    `additionalKeysP` [("M-m",spawn "/home/anbate/remacs.sh")
                      ,("M-i",spawn "google-chrome")
                      ,("M-t",spawn "icedove")
                      ]


-- -----------------------------------------------------------------------------

myPrettyPrinter :: Connection -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = pangoColor "#003366" . shorten 50 . pangoSanitize
  , ppCurrent = pangoColor "#006666" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "#663366" . wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppUrgent  = pangoColor "red"
  }

managementHooks :: [ManageHook]
managementHooks = [
    resource  =? "Do"        --> doIgnore
  , className =? "rdesktop"  --> doFloat
  ]

-- -----------------------------------------------------------------------------

data RDesk = RDesk

instance XPrompt RDesk where
  showXPrompt     RDesk = "Remote desktop to:"
  commandToComplete _ c = c
  nextCompletion      _ = getNextCompletion

runRDeskPrompt :: XPConfig -> X ()
runRDeskPrompt c = mkXPrompt RDesk c (mkComplFunFromList targs) run
 where
  targs = ["pane.galois.com","porthole.galois.com"]
  run s = spawn ("/usr/bin/rdesktop -u GALOIS\\\\awick -g 1250x750 " ++ s)

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
 where
  tryGetName = do
    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
    addArgs namereq [String "org.xmonad.Log", Word32 5]
    sendWithReplyAndBlock dbus namereq 0
    return ()

outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
  let str' = "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
  msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
  addArgs msg [String str']
  send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
  return ()

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
 where
  sanitize '>'  acc = "&gt;" ++ acc
  sanitize '<'  acc = "&lt;" ++ acc
  sanitize '\"' acc = "&quot;" ++ acc
  sanitize '&'  acc = "&amp;" ++ acc
  sanitize x    acc = x:acc

startNitrogen :: IO ()
startNitrogen = do
  threadDelay (5 * 1000 * 1000)
  try_ $ rawSystem "nitrogen" ["--restore"]

try_ :: MonadIO m => IO a -> m ()
try_ action = liftIO $ try action >> return ()
