-- imort System.Taffybar.Support.PagerHints (pagerHints)
import XMonad
    ( mod4Mask,
      spawn,
      (|||),
      xmonad,
      Default(def),
      X,
      XConfig(layoutHook, terminal, modMask, borderWidth,
              focusedBorderColor, startupHook),
      Full(Full),
      Tall(Tall) )
import XMonad.Hooks.DynamicLog
    ( shorten,
      wrap,
      xmobarBorder,
      xmobarColor,
      xmobarRaw,
      xmobarStrip,
      PP(ppExtras, ppSep, ppTitleSanitize, ppCurrent, ppHidden,
         ppHiddenNoWindows, ppUrgent, ppOrder) )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Hooks.ManageDocks ( docks )
import XMonad.Hooks.StatusBar
    ( defToggleStrutsKey, statusBarProp, withEasySB )
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.Loggers ( logTitles )
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.Ungrab ()
import Graphics.X11.ExtraTypes.XF86 ()

main :: IO ()
main =
  xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    --    . pagerHints
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig


myConfig =
  def
    { terminal = "konsole",
      modMask = mod4Mask,
      borderWidth = 0,
      focusedBorderColor = "#bd93f9",
      startupHook = myStartupHook,
      layoutHook = myLayoutHook
    }
    `additionalKeysP` myKeys
                      
myKeys :: [(String, X ())]
myKeys = [ ("M-f", spawn "thunar")                             
  , ("M-m", spawn "brave")                              
  , ("M-p", spawn "rofi -show drun") 
  , ("<XF86AudioPlay>", spawn "playerctl --player=spotify_player play-pause")
  , ("<XF86AudioPause>", spawn "playerctl --player=spotify_player play-pause")
  , ("<XF86AudioNext>", spawn "playerctl --player=spotify_player next")
  , ("<XF86AudioPrev>", spawn "playerctl --player=spotify_player previous")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +1.5%")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -1.5%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")                                 
  , ("M-<L>", spawn "spotify_player playback seek -- -10000")
  , ("M-<R>", spawn "spotify_player playback seek -- +10000")
  , ("M-s"  , spawn "~/go/bin/spofi")
  , ("M-t"  , spawn "change_player")
  , ("M-y"  , spawn "playerctl --player=brave play-pause")
  , ("M-i"  , spawn "konsole -e spotify_player")
  ]

myLayoutHook = spacingRaw False (Border 30 0 30 0) True (Border 0 30 0 30) True
  $ Tall 1 (3 / 100) (1 / 2) ||| Full

myStartupHook :: X ()
myStartupHook = do
  --  spawnOnce
  --    "trayer --edge top --align right --SetDockType true \
  --    \--SetPartialStrut true --expand true --width 10 \
  --    \--transparent true --tint 0x5f5f5f --height 18"
  -- spawnOnce "taffybar"
  spawnOnce "feh --bg-fill --no-fehbg ~/.wallpapers/valley.png"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "picom -b --config ~/.config/picom/picom.conf"
  spawnOnce "playerctld daemon"

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . yellow2 . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow . shorten 10

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    yellow2 = xmobarColor "#ffcd3c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
