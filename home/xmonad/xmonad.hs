{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           XMonad                       hiding (Screen)
import           XMonad.Actions.Minimize
import           XMonad.Hooks.DynamicLog      (dynamicLogWithPP, ppOutput,
                                               xmobar, xmonadPropLog)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive    (fadeIn, fadeInactiveLogHook,
                                               fadeOutLogHook)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Circle         (Circle (..))
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           XMonad.Core
import           XMonad.Operations
import           XMonad.StackSet              hiding (workspaces)

import           Control.Applicative          ((<|>))
import           Control.Concurrent
import           Control.Monad                (when)
import qualified Data.List                    as L
import           Data.Maybe

data MyWorkspaces = W0
    | Dev1
    | Dev2
    | Media
    | Chats
    | Etc1
    | Etc2
    | W1
    | W2
    | W3
    | W4
    | W5
    | W6
    | W7
    | W8
    | W9
    deriving (Show, Eq, Bounded, Ord, Enum)

-- Which keys are used for switching to a specific workspace
myWorkspaces :: [(KeySym, String)]
myWorkspaces = zip
  ([xK_b, xK_n, xK_m, xK_semicolon, xK_quoteright, xK_bracketleft, xK_bracketright] ++ [xK_1..xK_9])
  (fmap show [W0 ..])

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((myModMask, key), (windows $ view ws))
  | (key, ws) <- myWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), (windows $ shift ws))
  | (key, ws) <- myWorkspaces
  ] ++
  [ ((myModMask, xK_c), sendMessage Killed >> withFocused killWindow)
  , ((myModMask, xK_Return), spawn "alacritty")
  , ((myModMask, xK_u), windows swapMaster)
  , ((myModMask .|. shiftMask, xK_u), windows focusMaster)

  , ((myModMask, xK_o), sendMessage ToggleStruts)
  , ((myModMask, xK_p), spawn "rofi -show run")
  , ((myModMask .|. shiftMask, xK_p), spawn "rofi -show window")
  , ((myModMask, xK_i), withFocused minimizeWindow)
  , ((myModMask .|. shiftMask, xK_i)
    , withLastMinimized maximizeWindowAndFocus)
  ]


myManageHook :: ManageHook
myManageHook = composeAll $
  [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
  , (className =? "TelegramDesktop" <&&> title =? "Media viewer") --> doFloat
  , (className =? "Anki") --> doFloat
  ] ++
  [ "TelegramDesktop", "discord"] `sendTo` Chats ++
  [ "mpv", "tixati", "Tixati" ] `sendTo` Media
  where
    sendTo :: [String] -> MyWorkspaces -> [ManageHook]
    sendTo names ws = map (\name -> className =? name --> doShift (show ws)) names


-- Slightly configured Tall layout which tracks killed windows
-- and puts boundaries on the number of main windows
newtype MyTall a = MyTall
    { tall :: (Tall a)
    }
    deriving (Show, Read)


data Killed = Killed
    deriving (Typeable)
instance Message Killed

-- Boundaries:
-- 1. There is always at least one master window
-- 2. There can't be more master windows then windows on this workspace
instance LayoutClass MyTall a where
  pureLayout = pureLayout . tall
  handleMessage mt@(MyTall (Tall nmaster delta frac)) m =
      case fromMessage m of
        Just (IncMasterN d) -> Just <$> incmastern d
        Nothing  ->
          case fromMessage m of
            Just Killed -> Just <$> kill
            Nothing     -> pure (pureMessage mt m)
    where
      winNumA = withWindowSet (pure . L.length . integrate' . stack . workspace . current)
      shrink n = max 1 . min n

      kill =
        winNumA >>= (\n -> pure $ MyTall (Tall (shrink (n-1) nmaster) delta frac))

      incmastern d =
        winNumA >>= (\n -> pure $ MyTall (Tall (shrink n (d + nmaster)) delta frac))


  pureMessage (MyTall (Tall nmaster delta frac)) m
      =   resize   <$> fromMessage m
    where
      resize Shrink = MyTall (Tall nmaster delta (max 0 $ frac-delta))
      resize Expand = MyTall (Tall nmaster delta (min 1 $ frac+delta))


type XScreen = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- Show name or the current workspace
myLogHook :: X ()
myLogHook = withWindowSet $ \winSet ->
  let
    csid :: ScreenId
    csid = screen $ current winSet
    ss :: [XScreen]
    ss = L.sortOn screen $ screens winSet
    ws :: [WindowSpace]
    ws = workspace <$> ss

    showName :: WindowSpace -> String
    showName w = postPad (layoutSymbol w) $ pad 6 (tag w)

    showScreenId :: XScreen -> String
    showScreenId s
      | sid /= csid = (\(S i) -> show i) sid
      | otherwise   = (\(S i) -> "[" ++ show i ++ "]") sid
      where
        sid = screen s
  in
    xmonadPropLog $ case ws of
      []  -> error "Impossible"
      [w] -> showName w
      _   -> L.intercalate ", "
        $ zipWith each
          (showScreenId <$> ss)
          (showName <$> ws)
        where
          each n w = n ++ ":" ++ w

-- Visually distinguish between different layouts
layoutSymbol :: WindowSpace -> Maybe Char
layoutSymbol ws
  | description l == "Spacing " <> description Full = Just $ hidden '*'
  | otherwise = Nothing
  where
    l      = layout ws
    winNum = length . integrate' . stack $ ws
    hidden = if winNum `elem` [0, 1] then id else const '!'


-- Add padding to the name
postPad :: Maybe Char -> String -> String
postPad c pStr =
  let
    (p1, str') = L.span (== ' ') pStr
    (str, p2)  = L.span (/= ' ') str'
  in
    p1 ++ str ++ case p2 of
                    (p:ps) -> fromMaybe p c : ps
                    []     -> ""

pad :: Int -> String -> String
pad n = go . L.take n
  where
    go str =
      let
        plen   = n - L.length str
        side   = plen `div` 2
        offset = plen `mod` 2
        padding n = L.take n $ L.repeat ' '
      in
        padding side ++ str ++ padding (side + offset)


type MyLayout =
  (ModifiedLayout SmartBorder
  (ModifiedLayout Spacing
  (ModifiedLayout AvoidStruts (Choose MyTall Full))))

myLayoutHook :: Eq a => MyLayout a
myLayoutHook
  = smartBorders
  $ spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
  $ avoidStruts $ tiled ||| Full
  where
    tiled = MyTall $ Tall nmaster delta ratio
    nmaster = 1
    delta   = 12/100
    ratio   = 1/2

myModMask :: KeyMask
myModMask = mod4Mask

myConfig :: XConfig MyLayout
myConfig = def
  { modMask     = myModMask
  , borderWidth = 2
  , focusedBorderColor = "#e2a478"
  , normalBorderColor = "#1c1c1c"
  , workspaces  = fmap snd myWorkspaces
  , layoutHook  = myLayoutHook
  , logHook     = myLogHook
  , manageHook =
    -- =   insertPosition Below Newer
    myManageHook <+> manageHook def
  , focusFollowsMouse = False
  , terminal    = "alacritty"
  } `additionalKeys` myAdditionalKeys

main :: IO ()
main = do
  -- Ensure that only one instance of XMobar is running
  spawn "killall xmobar"
  threadDelay 100000 -- ugly
  spawn "xmobar"
  xmonad $ ewmh (docks myConfig)

