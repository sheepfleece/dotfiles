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
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.RefocusLast     (refocusLastLogHook,
                                               refocusLastWhen)
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.Types            (These (..))
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
import           Data.Monoid                  (All)
import           Text.Read                    (readMaybe)

import           Graphics.X11.Xinerama

import           Data.Ratio

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

  , (xK_semicolon, Books), (xK_quoteright, Notes)
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((myModMask, key), (windows $ greedyView ws))
  | (key, ws) <- myWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), (windows $ shift ws))
  | (key, ws) <- myWorkspaces
  ] ++

  [ ((myModMask, xK_c), killsWindow)
  , ((myModMask, xK_z), sendMessage MirrorShrink)
  , ((myModMask, xK_a), sendMessage MirrorExpand)

  , ((myModMask, xK_Return), spawn "alacritty")


  , ((myModMask, xK_space), sendMessage (ToggleMsg 0))

  , ((myModMask, xK_i), windows focusMaster)
  , ((myModMask .|. shiftMask, xK_i), windows swapMaster)

  , ((myModMask, xK_u     ), sendMessage NextLayout)
  , ((myModMask, xK_y     ), sendMessage (ToggleMsg 1))
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



type XScreen = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

 {-
   Show the name of the current workspace.
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

type MyLayout = (AvoidStruts !* (MyTiled <?> MyThreeCols)) Window

myLayout :: MyLayout
myLayout
  = avoidStruts
  $ tiled ||| threeCols

type MyTiled = (SmartMaster !* Magnifier !* Magnifier !* ResizableTall)
tiled :: MyTiled Window
tiled
  = smartMaster nmasters
  $ magnify nmasters FullScreen Off All
  $ magnify nmasters (Ratio 1.2 1.2) Off All
  $ ResizableTall nmasters delta ratio []

type MyThreeCols = (InverseMaster !* SmartMaster !* Magnifier !* Magnifier !* ThreeCol)
threeCols :: MyThreeCols Window
threeCols
  = inverseMaster
  $ smartMaster nmasters
  $ magnify nmasters FullScreen Off All
  $ magnify nmasters (Ratio 1.3 1.3) On NoMaster
  $ ThreeColMid nmasters delta ratio

nmasters :: Int
nmasters = 1

delta, ratio :: Ratio Integer
delta   = 12/100
ratio   = 1/2

myModMask :: KeyMask
myModMask = mod4Mask

myHandleEventHook :: Event -> X All
myHandleEventHook = refocusLastWhen (liftX . pure $ True)

-- myConfig :: XConfig DefaultLayout
myConfig = kde4Config
  { modMask            = myModMask
  , borderWidth        = 3
  , focusedBorderColor = "#e2a478"
  , normalBorderColor  = "#1c1c1c"
  , workspaces         = fmap snd myWorkspaces
  , layoutHook         = myLayout
  , logHook            = refocusLastLogHook <+> myLogHook
  , manageHook         =
    myManageHook <+> manageHook kde4Config <+> insertPosition Below Newer
  , handleEventHook    = myHandleEventHook
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
magnifier = magnify 1 (Ratio 1.5 1.5) On All

-- | Change the size of the window that has focus by a custom zoom
{-# DEPRECATED magnifiercz "Use magnify instead." #-}
magnifiercz :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz cz = magnify 1 (Ratio (fromRational cz) (fromRational cz)) On All

-- | Increase the size of the window that has focus, unless if it is one of the
-- master windows.
{-# DEPRECATED magnifier' "Use magnify instead." #-}
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = magnify 1 (Ratio 1.5 1.5) On NoMaster

-- | Magnifier that defaults to Off
{-# DEPRECATED magnifierOff "Use magnify instead." #-}
magnifierOff :: l a -> ModifiedLayout Magnifier l a
magnifierOff = magnify 1 (Ratio 1.5 1.5) Off All

-- | A magnifier that greatly magnifies with defaults to Off
{-# DEPRECATED maxMagnifierOff "Use magnify with FullScreen instead." #-}
maxMagnifierOff :: l a -> ModifiedLayout Magnifier l a
maxMagnifierOff = magnify 1 FullScreen Off All

-- | Increase the size of the window that has focus by a custom zoom,
-- unless if it is one of the the master windows.
{-# DEPRECATED magnifiercz' "Use magnify instead." #-}
magnifiercz' :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz' cz = magnify 1 (Ratio (fromRational cz) (fromRational cz)) On NoMaster

-- | A magnifier that greatly magnifies just the vertical direction
{-# DEPRECATED maximizeVertical "Use magnify with Horizontal instead." #-}
maximizeVertical :: l a -> ModifiedLayout Magnifier l a
maximizeVertical = magnify 1 Horizontal Off All


data Zoom = Ratio !Double !Double | Horizontal | Vertical | FullScreen
  deriving (Read, Show)

newtype ToggleMsg = ToggleMsg { depth :: Int }
  deriving (Typeable)
instance Message ToggleMsg

data MagnifyMsg
  = Magnify !Double
  | ToggleOn | ToggleOff
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

toggle :: Toggle -> Toggle
toggle On  = Off
toggle Off = On

data MagnifyMaster = All | NoMaster
  deriving (Read, Show)

instance LayoutModifier Magnifier Window where
    redoLayout mag r (Just s) wrs =
      case (mg_toggle mag, mg_master mag) of
        (On, All) -> applyMagnifier (mg_zoom mag) r s wrs
        (On, NoMaster) -> unlessMaster (mg_nmaster mag) (applyMagnifier (mg_zoom mag)) r s wrs
        (Off, _) -> pure (wrs, Nothing)

    redoLayout _ _ _ wrs = pure (wrs, Nothing)

    pureMess = handlePureMess


    handleMessOrInterceptIt mag msg
      | Just (ToggleMsg 0) <- fromMessage msg = pure . Just $ This mag { mg_toggle = toggle $ mg_toggle mag }
      | Just (ToggleMsg n) <- fromMessage msg = pure . Just . That $ SomeMessage (ToggleMsg (n - 1))
      | otherwise = pure $ (\v -> These v msg) <$> pureMess mag msg

    modifierDescription mag =
      case (mg_toggle mag, mg_master mag) of
        (On, All)      -> "Magnifier"
        (On, NoMaster) -> "Magnifier NoMaster"
        (Off, _)       -> "Magnifier (off)"

handlePureMess :: Magnifier a -> SomeMessage -> Maybe (Magnifier a)
handlePureMess mag@(Mag _ (Ratio dx dy) On _) m
  | Just (Magnify d) <- fromMessage m = Just mag { mg_zoom = Ratio (d + dx) (d + dy) }
  | Just (IncMasterN d) <- fromMessage m = Just mag { mg_nmaster = max 0 (mg_nmaster mag + d) }

handlePureMess mag@(Mag _ _ On _) m
  | Just ToggleOff   <- fromMessage m = Just mag { mg_toggle = Off }
  | Just (SetZoom z) <- fromMessage m = Just mag { mg_zoom = z }

handlePureMess mag@(Mag _ _ Off _) m
  | Just ToggleOn    <- fromMessage m = Just mag { mg_toggle = On }


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

smartMaster :: Int -> l a -> ModifiedLayout SmartMaster l a
smartMaster n l = ModifiedLayout (SmartMaster n) l

newtype SmartMaster a = SmartMaster { unSmartMaster :: Int }
  deriving (Show, Read)

instance LayoutModifier SmartMaster Window where
  handleMessOrInterceptIt (SmartMaster nmaster) msg
    | Just (IncMasterN d) <- fromMessage msg = do
        nmaster' <- incMaster nmaster d
        pure $ Just $ These (SmartMaster nmaster') (SomeMessage (IncMasterN (nmaster' - nmaster)))
    | Just (Killed win) <- fromMessage msg = do
        mm <- killedMaster nmaster win
        pure $ case mm of
                  Nothing -> Nothing
                  Just nmaster'  -> Just $ These (SmartMaster nmaster') (SomeMessage (IncMasterN (nmaster' - nmaster)))


    | otherwise = pure Nothing


incMaster :: Int -> Int -> X Int
incMaster nmaster d = withWindowSet $ \winset -> pure $ applyBoundaries (length (getWindows winset)) (d + nmaster)

killedMaster :: Int -> Window -> X (Maybe Int)
killedMaster nmaster win = withWindowSet $ \winset -> pure $
    let
      -- Decrease by one if deleting master window.
      -- When I have more than one master window and want to delete one of them
      -- it is very rarely that I want to keep the same number of master windows
      newMasterNum = max 1 $ nmaster - fromEnum (isMaster nmaster win (getWindows winset))
    in
      if newMasterNum == nmaster
        then Nothing
        else Just newMasterNum

getWindows :: WindowSet -> [Window]
getWindows = integrate' . stack . workspace . current

applyBoundaries :: Int -> Int -> Int
applyBoundaries n = max 1 . min n

isMaster :: Int -> Window -> [Window] -> Bool
isMaster nmaster w ws = fromMaybe False $ fmap (< nmaster) $ lookup' w ws

lookup' :: Eq a => a -> [a] -> Maybe Int
lookup' x xs = lookup x $ zip xs [0..]


data InverseMaster a = InverseMaster
  deriving (Read, Show)

instance LayoutModifier InverseMaster Window where
  handleMessOrInterceptIt _ msg
    | Just (IncMasterN d) <- fromMessage msg = pure . Just . That . SomeMessage $ IncMasterN (-d)
    | otherwise = pure Nothing

inverseMaster :: l a -> ModifiedLayout InverseMaster l a
inverseMaster = ModifiedLayout InverseMaster


data Killed = Killed Window
    deriving (Typeable)
instance Message Killed
