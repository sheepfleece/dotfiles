{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

import           XMonad                       hiding (Screen)
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
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           XMonad.Core
import           XMonad.Operations
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

  [ ((myModMask, xK_c), killsWindow)
  , ((myModMask, xK_z), sendMessage MirrorShrink)
  , ((myModMask, xK_a), sendMessage MirrorExpand)

  , ((myModMask, xK_Return), spawn "alacritty")


  , ((myModMask, xK_space), toggleFull)
  , ((myModMask, xK_i), windows focusMaster)
  , ((myModMask .|. shiftMask, xK_i), windows swapMaster)
  , ((myModMask, xK_u), setLayout (Layout (avoidStruts tiled)))
  , ((myModMask, xK_y), setLayout (Layout (avoidStruts threeCols)))

  , ((myModMask, xK_comma ), sendMessage (IncMasterN ( 1)))
  , ((myModMask, xK_period), sendMessage (IncMasterN (-1)))
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
      (\r -> r { threeColNMaster = applyBoundaries (length (getWindows winset)) (d + threeColNMaster r)})
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
    showName w = xmobarLayout (layoutSymbol w) $ xmobarName 6 (tag w)

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

{-
  Visually distinguish between different layout states with a single character.
    '!' - Full layout with hidden windows underneath.
    '*' - Full layout without any windows.
    '#' - Three columns layout.
 -}
layoutSymbol :: WindowSpace -> Char
layoutSymbol ws
  | description l == description Full = if winNum `elem` [0, 1] then '*' else '!'
  | description l == description threeCols = '#'
  | otherwise = ' '
  where
    l = layout ws
    winNum = length . integrate' . stack $ ws

xmobarLayout :: Char -> String -> String
xmobarLayout symbol name =
  let
    (padl, name') = L.span (== ' ') name
    (name'', padr) = L.span (/= ' ') name'
  in
    padl ++ name'' ++ case padr of
                    (_:padr') -> symbol : padr'
                    []        -> ""

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

{-
   Saved layouts for toggling `Full` layout on each workspace.
   The layout is first saved when `toggleFull` is used.
   That means that initial values are not used.
 -}
savedLayouts :: IORef [Layout Window]
savedLayouts = unsafePerformIO (newIORef $ const (Layout Full) <$> [ minBound .. maxBound :: MyWorkspaces ])
{-# NOINLINE savedLayouts #-}

toggleFull :: X ()
toggleFull = withWindowSet $ \winSet ->
  let
    curlay :: Layout Window
    curlay = layout . workspace . current $ winSet

    mcurwsp :: Maybe MyWorkspaces
    mcurwsp = readMaybe . tag . workspace . current $ winSet
  in
    case mcurwsp of
      Nothing -> pure ()
      Just curwsp ->
        if description curlay /= description Full
        then do
          liftIO (modifyIORef' savedLayouts (updateAt (fromEnum curwsp) (const curlay)))
          setLayout (Layout $ avoidStruts Full)
          sendMessage ToggleStruts
        else do
          ls <- liftIO (readIORef savedLayouts)
          setLayout (ls !! (fromEnum curwsp))

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt 0 f (x:xs) = f x : xs
updateAt n f (x:xs) = x : updateAt (n - 1) f xs
updateAt _ _ []     = []

type (<?>) = Choose
infixr 3 <?>

type (!*) = ModifiedLayout
infixr 4 !*

type MyLayout =
  SmartBorder !* Spacing !* AvoidStruts !* (MyTall <?> MyCols)

myLayoutHook :: Eq a => Integer -> MyLayout a
myLayoutHook n
  = smartBorders
  $ spacingRaw True (Border 0 n n n) True (Border n n n n) True
  $ avoidStruts $ tiled ||| threeCols

tiled :: MyTall a
tiled = MyTall $ ResizableTall nmaster delta ratio []


threeCols :: MyCols a
threeCols = MyCols $ ThreeColMid nmaster delta ratio

nmaster :: Int
nmaster = 1

delta, ratio :: Ratio Integer
delta   = 12/100
ratio   = 1/2

myModMask :: KeyMask
myModMask = mod4Mask

myConfig :: XConfig MyLayout
myConfig = kdeConfig
  { modMask            = myModMask
  , borderWidth        = 3
  , focusedBorderColor = "#e2a478"
  , normalBorderColor  = "#1c1c1c"
  , workspaces         = fmap snd myWorkspaces
  , layoutHook         = myLayoutHook 0
  , logHook            = myLogHook
  , manageHook         =
    {-
       `focusDown` is used for opening files in nvim and staying there.
       I use it very rarely to make it togglable (if it even can be made).
     -}
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

