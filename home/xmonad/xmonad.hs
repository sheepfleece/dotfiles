{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

import           XMonad                       hiding (Screen, focus)
import           XMonad.Actions.Minimize

import           XMonad.Config.Kde
import           XMonad.Config.Prime          (getClassHint, resClass)
import           XMonad.Hooks.DynamicLog      (dynamicLogWithPP, ppOutput,
                                               xmobar, xmonadPropLog)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive    (fadeIn, fadeInactiveLogHook,
                                               fadeOutLogHook)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Circle         (Circle (..))


import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.XUtils

import           XMonad.Core
import           XMonad.Operations            hiding (focus)
import           XMonad.StackSet              hiding (workspaces)

import           Control.Applicative          ((<|>))
import           Control.Concurrent
import           Control.Monad                (when)
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Text.Read                    (readMaybe)

import           Graphics.X11.Xinerama

import           Data.Ratio
import           XMonad.Hooks.ManageHelpers

import           Data.IORef
import           System.IO.Unsafe             (unsafePerformIO)

import           Data.Coerce                  (coerce)
import qualified Data.List                    as L
import           GHC.List                     (lookup)

data MyWorkspaces
  = Dev1  | Dev2
  | Books | Notes
  | Media | Chats | Games
  deriving (Show, Eq, Bounded, Ord, Enum, Read)

{-
  Keys which are used for switching to a specific workspace.
 -}
myWorkspaces :: [(KeySym, String)]
myWorkspaces = (fmap show) <$>
  [ (xK_n, Dev1), (xK_m, Dev2)
  , (xK_bracketleft, Media), (xK_bracketright, Chats)
  , (xK_backslash, Games)
    {-
       Oftentimes I read with only one hand,
       and it is benifitial to have two ways to switch to those Workspaces.
     -}
  , (xK_semicolon, Books), (xK_quoteright, Notes)
  , (xK_v, Books), (xK_b, Notes)
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((myModMask, key), (windows $ greedyView ws))
  | (key, ws) <- myWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), (windows $ shift ws))
  | (key, ws) <- myWorkspaces
  ] ++

  [ ((myModMask, xK_c), killsWindow >> windows focusUp)
  , ((myModMask, xK_z), sendMessage MirrorShrink)
  , ((myModMask, xK_a), sendMessage MirrorExpand)

  , ((myModMask, xK_Return), spawn "alacritty")


  , ((myModMask, xK_space), withFocused (sendMessage . maximizeRestore))

  , ((myModMask, xK_i), windows focusMaster)
  , ((myModMask .|. shiftMask, xK_i), windows swapMaster)

  , ((myModMask, xK_u     ), sendMessage NextLayout)
  , ((myModMask, xK_y     ), sendMessage Toggle)
  , ((myModMask, xK_comma ), sendMessage (IncMasterN ( 1)))
  , ((myModMask, xK_period), sendMessage (IncMasterN (-1)))
  , ((myModMask, xK_equal ), sendMessage (Magnify ( 0.1)))
  , ((myModMask, xK_minus ), sendMessage (Magnify (-0.1)))
  {-
    Hide xmobar. Doesn't work when there are no windows.
   -}
  , ((myModMask, xK_o), sendMessage ToggleStruts)


  {-
    Rofi is used for navigating workspaces and opening applications.
    TODO:
          There is an annoying bug when rofi spawns on the wrong screen if there are no windows open on the focused one.
          What is even worse the order of screens is different from XMobar's.
   -}
  , ((myModMask, xK_p), spawn "rofi -m -4 -show run")
  , ((myModMask .|. shiftMask, xK_p), spawn "rofi -m -4 -show window")
  ]

killsWindow :: X ()
killsWindow = withFocused $ \f -> sendMessage (Killed f) >> withFocused killWindow

myManageHook :: ManageHook
myManageHook = composeAll $
  [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
  , (className =? "TelegramDesktop" <&&> title =? "Media viewer") --> doFloat
  , (className =? "Anki") --> doFloat
  , (className =? "Steam" <&&> title =? "Friends List") --> doRectFloat (RationalRect (3 % 4) (1 % 2) (1 % 4) (1 % 2))

  {-
    KDE4 floating windows.
   -}
  , (title =? "Desktop - Plasma")   --> doFloat
  , (title =? "plasma-desktop")     --> doFloat
  , (title =? "win7")               --> doFloat
  , (className =? "plasmashell")    --> doFloat
  , (className =? "Plasma")         --> doFloat
  , (className =? "krunner")        --> doFloat
  , (className =? "kmix")           --> doFloat
  , (className =? "klipper")        --> doFloat
  , (className =? "Plasmoidviewer") --> doFloat
  , (className =? "kwalletd5")      --> doFloat
  ] ++

  [ "Steam" ] `sendTo` Games ++
  [ "TelegramDesktop", "discord" ] `sendTo` Chats ++
  [ "tixati", "Tixati", "deluge", "Deluge" ] `sendTo` Media
  where
    sendTo :: [String] -> MyWorkspaces -> [ManageHook]
    sendTo names ws = map (\name -> className =? name --> doShift (show ws)) names


{-
  Slightly configured `ResizableTall` layout with
  different handling of master windows.
 -}
newtype MyTall a = MyTall
    { tall :: (ResizableTall a)
    }
    deriving (Show, Read)

withResizable :: (ResizableTall a -> ResizableTall a) -> MyTall a -> MyTall a
withResizable f (MyTall rt) = MyTall $ f rt

data Killed = Killed Window
    deriving (Typeable)
instance Message Killed

{-
  Boundaries:
  1. There is always at least one master window
  2. There can't be more master windows then windows on this workspace
 -}
instance LayoutClass MyTall a where
  doLayout (MyTall rt) rect stack = (fmap . fmap . fmap) MyTall $ doLayout  rt rect stack
  emptyLayout (MyTall rt) rect = (fmap . fmap . fmap) MyTall $ emptyLayout rt rect

  pureMessage :: MyTall a -> SomeMessage -> Maybe (MyTall a)
  pureMessage (MyTall rt) msg = coerce $ pureMessage rt msg

  handleMessage :: MyTall a -> SomeMessage -> X (Maybe (MyTall a))
  handleMessage mt@(MyTall rt) msg
    | Just (IncMasterN d) <- fromMessage msg = Just <$> handleIncMaster mt d
    | Just (Killed win)   <- fromMessage msg = Just <$> handleKilled mt win
    | otherwise                              = fmap MyTall <$> handleMessage rt msg

handleIncMaster :: MyTall a -> Int -> X (MyTall a)
handleIncMaster mt d = withWindowSet $ \winset -> pure $
    withResizable
      (\r -> r { _nmaster = applyBoundaries (length (getWindows winset)) (d + _nmaster r)})
      mt

handleKilled :: MyTall a -> Window -> X (MyTall a)
handleKilled mt win = withWindowSet $ \winset -> pure $
    let
      -- Decrease by one if deleting master window.
      -- When I have more than one master window and want to delete one of them
      -- it is very rarely that I want to keep the same number of master windows
      newMasterNum masterNum = max 1 $ masterNum - fromEnum (isMaster masterNum win (getWindows winset))
    in
      withResizable
        (\r -> r { _nmaster = newMasterNum (_nmaster r) })
        mt

getWindows :: WindowSet -> [Window]
getWindows = integrate' . stack . workspace . current

applyBoundaries :: Int -> Int -> Int
applyBoundaries n = max 1 . min n

isMaster :: Int -> Window -> [Window] -> Bool
isMaster nmaster w ws = fromMaybe False $ fmap (< nmaster) $ lookup' w ws

lookup' :: Eq a => a -> [a] -> Maybe Int
lookup' x xs = lookup x $ zip xs [0..]


{-
  Same changes for `ThreeCol` layout
  TODO: Instead of RPY we can write one LayoutModifier for both of them.
  Maybe will do it later :3
 -}
newtype MyCols a = MyCols
  { cols :: ThreeCol a
  }
  deriving (Show, Read)

withThreeCols :: (ThreeCol a -> ThreeCol a) -> MyCols a -> MyCols a
withThreeCols f (MyCols rt) = MyCols $ f rt

instance LayoutClass MyCols a where
  doLayout (MyCols rt) rect stack = (fmap . fmap . fmap) MyCols $ doLayout  rt rect stack
  emptyLayout (MyCols rt) rect = (fmap . fmap . fmap) MyCols $ emptyLayout rt rect

  pureMessage :: MyCols a -> SomeMessage -> Maybe (MyCols a)
  pureMessage (MyCols rt) msg = coerce $ pureMessage rt msg

  handleMessage :: MyCols a -> SomeMessage -> X (Maybe (MyCols a))
  handleMessage mt@(MyCols rt) msg
    | Just (IncMasterN d) <- fromMessage msg = Just <$> handleIncMaster' mt d
    | Just (Killed win)   <- fromMessage msg = Just <$> handleKilled' mt win
    | otherwise                              = fmap MyCols <$> handleMessage rt msg

handleIncMaster' :: MyCols a -> Int -> X (MyCols a)
handleIncMaster' mt d = withWindowSet $ \winset -> pure $
    withThreeCols
      (\r -> r { threeColNMaster = applyBoundaries (length (getWindows winset)) ((-d) + threeColNMaster r)})
      mt

handleKilled' :: MyCols a -> Window -> X (MyCols a)
handleKilled' mt win = withWindowSet $ \winset -> pure $
    let
      -- Decrease by one if deleting master window.
      -- When I have more than one master window and want to delete one of them
      -- it is very rarely that I want to keep the same number of master windows
      newMasterNum masterNum = max 1 $ masterNum - fromEnum (isMaster masterNum win (getWindows winset))
    in
      withThreeCols
        (\r -> r { threeColNMaster = newMasterNum (threeColNMaster r) })
        mt

type XScreen = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

 {-
   Show the name and used layout of the current workspace.
   1. Names should not be longer than 5 characters for aesthetic reasons.
  -}
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
    showName w = xmobarName 6 (tag w)

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

xmobarName :: Int -> String -> String
xmobarName n = go . L.take n
  where
    go str =
      let
        plen   = n - L.length str
        side   = plen `div` 2
        offset = plen `mod` 2
        padding n = L.take n $ L.repeat ' '
      in
        padding side ++ str ++ padding (side + offset)

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt 0 f (x:xs) = f x : xs
updateAt n f (x:xs) = x : updateAt (n - 1) f xs
updateAt _ _ []     = []

type (<?>) = Choose
infixr 3 <?>

type (!*) = ModifiedLayout
infixr 4 !*

myLayout :: (AvoidStruts !* ((Maximize !* Magnifier !* MyTall) <?> (Maximize !* Magnifier !* MyCols))) Window
myLayout
  = avoidStruts
  $ tiled ||| threeCols

tiled :: (Maximize !* Magnifier !* MyTall) Window
tiled
  = maximizeWithPadding 0
  $ ModifiedLayout (Mag nmaster (Ratio 1.2 1.2) Off All)
  $ MyTall $ ResizableTall nmaster delta ratio []


threeCols :: (Maximize !* Magnifier !* MyCols) Window
threeCols
  = maximizeWithPadding 0
  $ ModifiedLayout (Mag nmaster (Ratio 1.3 1.3) On NoMaster)
  $ MyCols $ ThreeColMid nmaster delta ratio

nmaster :: Int
nmaster = 1

delta, ratio :: Ratio Integer
delta   = 12/100
ratio   = 1/2

myModMask :: KeyMask
myModMask = mod4Mask

-- myConfig :: XConfig DefaultLayout
myConfig = kdeConfig
  { modMask            = myModMask
  , borderWidth        = 3
  , focusedBorderColor = "#e2a478"
  , normalBorderColor  = "#1c1c1c"
  , workspaces         = fmap snd myWorkspaces
  , layoutHook         = myLayout
  , logHook            = myLogHook
  , manageHook         =
    myManageHook <+> manageHook kdeConfig <+> insertPosition Below Newer
  , focusFollowsMouse  = False
  , terminal           = "alacritty"
  } `additionalKeys` myAdditionalKeys

main :: IO ()
main = do
  {-
    Ensure that only one instance of XMobar is running.
   -}
  spawn "killall xmobar" >> threadDelay 100000

  {-
    Spawn XMobar on each screen.
   -}
  dpy <- openDisplay ""
  ns  <- L.length <$> getScreenInfo dpy
  for_ [0 .. ns - 1] $ \n -> spawn $ "xmobar --screen " <> show n

  {-
    Start XMonad.
   -}
  xmonad . ewmh . docks $ myConfig



---------------------------------------
{-# DEPRECATED magnifier "Use magnify instead." #-}
magnifier :: l a -> ModifiedLayout Magnifier l a
magnifier = ModifiedLayout (Mag 1 (Ratio 1.5 1.5) On All)

-- | Change the size of the window that has focus by a custom zoom
{-# DEPRECATED magnifiercz "Use magnify instead." #-}
magnifiercz :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz cz = ModifiedLayout (Mag 1 (Ratio (fromRational cz) (fromRational cz)) On All)

-- | Increase the size of the window that has focus, unless if it is one of the
-- master windows.
{-# DEPRECATED magnifier' "Use magnify instead." #-}
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (Mag 1 (Ratio 1.5 1.5) On NoMaster)

-- | Magnifier that defaults to Off
{-# DEPRECATED magnifierOff "Use magnify instead." #-}
magnifierOff :: l a -> ModifiedLayout Magnifier l a
magnifierOff = ModifiedLayout (Mag 1 (Ratio 1.5 1.5) Off All)

-- | A magnifier that greatly magnifies with defaults to Off
{-# DEPRECATED maxMagnifierOff "Use magnify with FullScreen instead." #-}
maxMagnifierOff :: l a -> ModifiedLayout Magnifier l a
maxMagnifierOff = ModifiedLayout (Mag 1 FullScreen Off All)

-- | Increase the size of the window that has focus by a custom zoom,
-- unless if it is one of the the master windows.
{-# DEPRECATED magnifiercz' "Use magnify instead." #-}
magnifiercz' :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz' cz = ModifiedLayout (Mag 1 (Ratio (fromRational cz) (fromRational cz)) On NoMaster)

-- | A magnifier that greatly magnifies just the vertical direction
{-# DEPRECATED maximizeVertical "Use magnify with Horizontal instead." #-}
maximizeVertical :: l a -> ModifiedLayout Magnifier l a
maximizeVertical = ModifiedLayout (Mag 1 Horizontal Off All)


data Zoom = Ratio !Double !Double | Horizontal | Vertical | FullScreen
  deriving (Read, Show)

data MagnifyMsg
  = Magnify !Double
  | ToggleOn | ToggleOff | Toggle
  | SetZoom Zoom
  deriving (Typeable)
instance Message MagnifyMsg

data Magnifier a = Mag
  { mg_nmaster :: !Int
  , mg_zoom    :: Zoom
  , mg_toggle  :: Toggle
  , mg_master  :: MagnifyMaster
  } deriving (Read, Show)

magnify :: Int -> Zoom -> Toggle -> MagnifyMaster -> l a -> ModifiedLayout Magnifier l a
magnify n z t m = ModifiedLayout $ Mag n z t m

data Toggle = On | Off
  deriving (Read, Show)

data MagnifyMaster = All | NoMaster
  deriving (Read, Show)

instance LayoutModifier Magnifier Window where
    redoLayout  (Mag _ z On All     ) r (Just s) wrs = applyMagnifier z r s wrs
    redoLayout  (Mag n z On NoMaster) r (Just s) wrs = unlessMaster n (applyMagnifier z) r s wrs
    redoLayout  _                     _ _        wrs = pure (wrs, Nothing)

    handleMess mag m
      | Just (IncMasterN d) <- fromMessage m = fmap Just $ withWindowSet $ \winset -> pure $
        mag { mg_nmaster = applyBoundaries (length (getWindows winset)) (mg_nmaster mag - d) }

      | Just (Killed w)     <- fromMessage m = fmap Just $ withWindowSet $ \winset -> pure $
        let
          newMasterNum masterNum =
            max 1 $ masterNum - fromEnum (isMaster masterNum w (getWindows winset))
        in mag { mg_nmaster = newMasterNum (mg_nmaster mag) }
      | otherwise = pure $ pureMess mag m

    pureMess = handlePureMess

    modifierDescription mag =
      case (mg_toggle mag, mg_master mag) of
        (On, All)      -> "Magnifier"
        (On, NoMaster) -> "Magnifier NoMaster"
        (Off, _)       -> "Magnifier (off)"

handlePureMess :: Magnifier a -> SomeMessage -> Maybe (Magnifier a)
handlePureMess mag@(Mag _ (Ratio dx dy) On  _) m
  | Just (Magnify d)    <- fromMessage m = Just mag { mg_zoom = Ratio (d + dx) (d + dy) }

handlePureMess mag@(Mag _ _ On _) m
  | Just ToggleOff      <- fromMessage m = Just mag { mg_toggle = Off }
  | Just Toggle         <- fromMessage m = Just mag { mg_toggle = Off }
  | Just (SetZoom z)    <- fromMessage m = Just mag { mg_zoom = z }

handlePureMess mag@(Mag _ _ Off _) m
  | Just ToggleOn       <- fromMessage m = Just mag { mg_toggle = On }
  | Just Toggle         <- fromMessage m = Just mag { mg_toggle = On }

handlePureMess _ _ = Nothing

type NewLayout a = Rectangle -> Stack a -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe (Magnifier a))

unlessMaster :: Int -> NewLayout a -> NewLayout a
unlessMaster n mainmod r s wrs
  | null (drop (n-1) (up s)) = pure (wrs, Nothing)
  | otherwise                = mainmod r s wrs

applyMagnifier :: Zoom -> Rectangle -> t -> [(Window, Rectangle)]
               -> X ([(Window, Rectangle)], Maybe a)
applyMagnifier z r _ wrs = do
  focused <- withWindowSet (pure . peek)
  let mag (w, wr) ws
        | focused == Just w = ws ++ [(w, magnifyInto z wr r)]
        | otherwise         = (w,wr) : ws

  pure (reverse $ foldr mag [] wrs, Nothing)

magnifyInto :: Zoom -> Rectangle -> Rectangle -> Rectangle
magnifyInto zoom rect@(Rectangle x y w h) screenRect@(Rectangle sx sy sw sh) =
  case zoom of
    FullScreen  -> screenRect
    Vertical    -> Rectangle sx y sw h
    Horizontal  -> Rectangle x sy w sh
    Ratio dx dy -> fit screenRect $ magnifyRect dx dy rect

magnifyRect :: Double -> Double -> Rectangle -> Rectangle
magnifyRect zoomx zoomy (Rectangle x y w h) = Rectangle x' y' w' h'
  where
    x' = x - (fromIntegral (w' - w) `div` 2)
    y' = y - (fromIntegral (h' - h) `div` 2)
    w' = round $ fromIntegral w * zoomx
    h' = round $ fromIntegral h * zoomy

fit :: Rectangle -> Rectangle -> Rectangle
fit (Rectangle sx sy sw sh) (Rectangle x y w h) = Rectangle x' y' w' h'
  where
    x' = max sx (x - max 0 (x + fi w - sx - fi sw))
    y' = max sy (y - max 0 (y + fi h - sy - fi sh))
    w' = min sw w
    h' = min sh h

