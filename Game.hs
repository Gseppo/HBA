module Game (
  runGame,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.&.), (.|.))
import Data.ByteString as BS
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (nullPtr)
import qualified CPU
import qualified RAM
import SDL (($=))
import qualified SDL
import qualified SDL.Raw.Enum as SDLEnum
import qualified SDL.Vect as SDLVect

-- | Initial program counter address
initialPC :: Word32
initialPC = 0x08000000

-- | GBA screen dimensions
screenWidth :: SDL.Vect.V2 Int
screenWidth = SDL.Vect.V2 240 160

-- | Function to run the game loop (with minimal graphics and controls)
runGame :: FilePath -> IO ()
runGame romPath = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  window <- SDL.createWindow "Barely Running GBA" SDL.defaultWindow { SDL.windowInitialSize = screenWidth }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  romData <- BS.readFile romPath
  memory <- RAM.newMemory romData
  initialCPUState <- CPU.newCPUState initialPC

  eventQueue <- SDL.createEventChannel

  gameLoop initialCPUState memory renderer eventQueue

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- | A slightly less basic game loop with rudimentary timing, graphics, and controls
gameLoop :: CPU.CPUState mode isa -> RAM.Memory -> SDL.Renderer -> SDL.EventChannel -> IO ()
gameLoop cpuState memory renderer eventQueue = do
  -- Handle events (very basic input)
  SDL.pollEvents eventQueue >>= mapM_ handleEvent

  -- **EXTREMELY SIMPLIFIED "GRAPHICS"**:
  -- We'll just draw a single color based on a memory location (very naive)
  vramAddress :: Word32
  vramAddress = 0x06000000 -- Start of VRAM

  -- Read a byte from VRAM and use it to determine the color (very arbitrary)
  colorByte <- RAM.readByte memory vramAddress
  let color = SDL.V4 (fromIntegral colorByte) (fromIntegral (colorByte `shiftL` 2)) (fromIntegral (colorByte `shiftL` 4)) 0xFF

  SDL.rendererDrawColor $= color
  SDL.clear renderer
  SDL.present renderer

  nextCPUState <- CPU.stepCPU cpuState memory

  -- Introduce a small delay
  threadDelay 16000

  gameLoop nextCPUState memory renderer eventQueue

-- | Very basic event handling (just prints key presses)
handleEvent :: SDL.Event -> IO ()
handleEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyboardEvent ->
    case SDL.keyboardEventKeyMotion keyboardEvent of
      SDL.Pressed -> do
        let keysym = SDL.keyboardEventKeysym keyboardEvent
        putStrLn $ "Key Pressed: " ++ show (SDL.keysymName keysym)
      SDL.Released -> return ()
  _ -> return ()

-- Helper to convert String to ByteString (efficiently now)
fromString :: String -> ByteString
fromString = BS.pack . map (fromIntegral . fromEnum)
