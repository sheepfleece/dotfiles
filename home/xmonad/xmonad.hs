{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

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
import qualified Data.List                    as L
import           Data.Maybe
import           Text.Read                    (readMaybe)

import           Graphics.X11.Xinerama

import           Data.Ratio
import           XMonad.Hooks.ManageHelpers

import           Data.IORef
import           System.IO.Unsafe             (unsafePerformIO)

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

  [ ((myModMask, xK_c), sendMessage Killed >> withFocused killWindow)
  , ((myModMask, xK_Return), spawn "alacritty")

  , ((myModMask, xK_y), windows focusMaster)
  , ((myModMask .|. shiftMask, xK_y), windows swapMaster)

  , ((myModMask, xK_space), toggleFull)
  , ((myModMask, xK_i), setLayout (Layout (avoidStruts tiled)))
  , ((myModMask, xK_u), setLayout (Layout (avoidStruts threeCols)))

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
  , (className =? ksmserver)        --> scatter notFilledWorkspace
  , (className =? ksmserver)        --> doFloat
  , (className =? "kwalletd5")      --> doFloat
  ] ++
  [ "Steam" ] `sendTo` Games ++
  [ "TelegramDesktop", "discord" ] `sendTo` Chats ++
  [ "tixati", "Tixati", "deluge", "Deluge" ] `sendTo` Media
  where
    sendTo :: [String] -> MyWorkspaces -> [ManageHook]
    sendTo names ws = map (\name -> className =? name --> doShift (show ws)) names

ksmserver :: String
ksmserver = "ksmserver"

scatter :: X (Maybe String) -> ManageHook
scatter action = do
  mws <- liftX action
  case mws of
    Nothing -> doF id
    Just ws -> doShift ws


notFilledWorkspace :: X (Maybe String)
notFilledWorkspace = do
  dsp    <- asks display
  winset <- gets windowset
  let
    loop :: [Screen String l Window sid sd] -> IO (Maybe String)
    loop [] = pure Nothing
    loop (screen:screens) = do
      classNames <-
        let
          getClassName :: Window -> IO String
          getClassName = fmap resClass . getClassHint dsp
          getWindows   = integrate' . stack . workspace
        in sequence $ getClassName <$> (getWindows screen)
      if (any (== ksmserver) classNames)
        then loop screens
        else pure . Just . tag . workspace $ screen
  liftIO $ loop $ screens winset


{-
  Slightly configured Tall layout with
  boundaries on the number of master windows.
 -}
newtype MyTall a = MyTall
    { tall :: (Tall a)
    }
    deriving (Show, Read)


data Killed = Killed
    deriving (Typeable)
instance Message Killed

{-
  Boundaries:
  1. There is always at least one master window
  2. There can't be more master windows then windows on this workspace
 -}
instance LayoutClass MyTall a where
  pureLayout = pureLayout . tall
  handleMessage mt@(MyTall (Tall nmaster delta frac)) msg
    | Just (IncMasterN d) <- fromMessage msg = Just <$> incmastern d
    | Just (Killed)       <- fromMessage msg = Just <$> kill
    | otherwise                              = pure (pureMessage mt msg)
    where
      winNumA = withWindowSet (pure . L.length . integrate' . stack . workspace . current)
      applyBoundaries n = max 1 . min n

      kill =
        winNumA >>= (\n -> pure $ MyTall (Tall (applyBoundaries (n-1) nmaster) delta frac))
      incmastern d =
        winNumA >>= (\n -> pure $ MyTall (Tall (applyBoundaries n (d + nmaster)) delta frac))

  {-
    TODO: I forgot why it is handled like this.
    Seems hacky.
   -}
  pureMessage (MyTall (Tall nmaster delta frac)) m
      = resize <$> fromMessage m
    where
      resize Shrink = MyTall (Tall nmaster delta (max 0 $ frac-delta))
      resize Expand = MyTall (Tall nmaster delta (min 1 $ frac+delta))


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
    '!' - Full layout with hidden windows.
    '*' - Full layout.
    '#' - Three columns layout.
 -}
layoutSymbol :: WindowSpace -> Char
layoutSymbol ws
  | description l == description Full
    = if winNum `elem` [0, 1]
        then '*'
        else '!'
  | description l == description threeCols
    = '#'
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
   Saved layout for each workspace.
   The layout is saved when `toggleFull` is used.
   That means that initial values do not matter.
 -}
savedLayouts :: IORef [Layout Window]
{-# NOINLINE savedLayouts #-}
savedLayouts = unsafePerformIO (newIORef $ const (Layout Full) <$> [ minBound .. maxBound :: MyWorkspaces ])

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

type MyLayout =
  (ModifiedLayout SmartBorder
  (ModifiedLayout Spacing
  (ModifiedLayout AvoidStruts (Choose MyTall (Choose Full ThreeCol)))))

myLayoutHook :: Eq a => Integer -> MyLayout a
myLayoutHook n
  = smartBorders
  $ spacingRaw True (Border 0 n n n) True (Border n n n n) True
  $ avoidStruts $ tiled ||| Full ||| threeCols

tiled :: MyTall a
tiled = MyTall $ Tall nmaster delta ratio

threeCols :: ThreeCol a
threeCols = ThreeColMid nmaster delta ratio

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
       I use it very rarely to make it optional (if it even can be made).
     -}
    myManageHook <+> manageHook kdeConfig -- <+> doF focusDown
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

