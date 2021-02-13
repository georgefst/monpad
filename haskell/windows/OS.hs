module OS (conf) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word64)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pPrint)

import System.Win32.Automation.Input
import System.Win32.Automation.Input.Key
import System.Win32.Automation.Input.Mouse

import Monpad

{-TODO
https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-sendinput
    work out what to do with 'sendInput' return value - throw exception when 0?
    permssions - do we need to run as admin?
-}

conf :: Layout MouseEvent Key -> ServerConfig () () MouseEvent Key
conf _ = mempty
    -- https://docs.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-mouseinput
    { onAxis = \MouseEvent{..} v0 -> do -- note that v0 is always between -1 and 1
        let event = case mouseType of
                Relative ->
                    let dwFlags = 0
                        mouseData = 0
                        v = round $ v0 * multiplier
                        (dx, dy) = case axis of
                            X -> (v, 0)
                            Y -> (0, v)
                     in MOUSEINPUT{..}
                Absolute ->
                    let dwFlags = 0x8000 -- MOUSEEVENTF_ABSOLUTE
                        mouseData = 0
                        v = round $ (v0 + 1) * multiplier / 2
                        (dx, dy) = case axis of
                            X -> (v, 0)
                            Y -> (0, v)
                     in MOUSEINPUT{..}
                Wheel ->
                    let dwFlags = case axis of
                            X -> 0x01000 -- MOUSEEVENTF_HWHEEL
                            Y -> 0x0800 -- MOUSEEVENTF_WHEEL
                        v = round $ v0 * multiplier
                        mouseData = v
                        (dx, dy) = (0, 0)
                     in MOUSEINPUT{..}
            dwExtraInfo = CUIntPtr 0
            time = 0 -- system will provide its own
        liftIO $ pPrint =<< sendInput [Mouse event]
     -- https://docs.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-keybdinput
    , onButton = \wVk up -> do
        let dwFlags = if up then 2 else 0
            wScan = 0
            dwExtraInfo = CUIntPtr 0
            time = 0 -- system will provide its own
        liftIO $ pPrint =<< sendInput [Keyboard KEYBDINPUT{..}]
    }

--TODO use an enum type, as with evdev
type Key = Word16 -- https://docs.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
data MouseEvent = MouseEvent
    { axis :: Axis
    , mouseType :: MouseEventType
    , multiplier :: Double
    }
    deriving (Generic, FromDhall)
data Axis = X | Y
    deriving (Generic, FromDhall)
data MouseEventType
    = Relative
    | Absolute
    | Wheel
    deriving (Generic, FromDhall)
