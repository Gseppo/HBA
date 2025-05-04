module PPU (
  PPUState (..),
  newPPUState,
  stepPPU,
  renderScanline,
) where

import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr)

-- | Placeholder for PPU state
data PPUState = PPUState
  { -- | Registers (very incomplete)
    control :: !Word16
  , status :: !Word16
  , vcount :: !Word16
    -- | VRAM (placeholder - needs proper structure)
  , vram :: !([Word8])
    -- | Palettes (placeholder)
  , paletteRAM :: !([Word16])
  }

-- | Creates a new initial PPU state (very basic)
newPPUState :: IO PPUState
newPPUState = return $ PPUState
  { control = 0
  , status = 0
  , vcount = 0
  , vram = replicate (96 * 1024) 0 -- 96KB VRAM
  , paletteRAM = replicate 512 0   -- 512 colors
  }

-- | Steps the PPU by one (conceptual) step
-- In a real emulator, this would be tied to clock cycles and scanline rendering
stepPPU :: PPUState -> IO PPUState
stepPPU ppuState = do
  -- Placeholder: Increment vertical counter (very simplistic)
  let nextVCount = (vcount ppuState + 1) `mod` 228 -- GBA has 228 scanlines total
  return ppuState { vcount = nextVCount }

-- | Renders a single scanline to a pixel buffer (very incomplete)
renderScanline :: PPUState -> Int -> Ptr Word32 -> IO ()
renderScanline ppuState scanline buffer = do
  -- Placeholder: Just draw a simple color based on the scanline number
  let color = 0x00FF00 * fromIntegral scanline -- Green gradient (very arbitrary)
  pokeElemOff (castPtr buffer) 0 color -- Draw to the first pixel (incorrect)

-- | Placeholder for reading PPU registers
readPPU :: Word32 -> PPUState -> Word16
readPPU addr ppuState =
  case addr of
    0x04000000 -> control ppuState
    0x04000002 -> status ppuState
    0x04000004 -> vcount ppuState
    _ -> 0 -- Unknown register

-- | Placeholder for writing to PPU registers
writePPU :: Word32 -> Word16 -> PPUState -> PPUState
writePPU addr value ppuState =
  case addr of
    0x04000000 -> ppuState { control = value }
    0x04000002 -> ppuState { status = value } -- In reality, some bits are read-only
    _ -> ppuState

-- | Placeholder for accessing VRAM
readVRAM :: Word32 -> PPUState -> Word8
readVRAM addr ppuState =
  let offset = fromIntegral (addr .&. 0x1FFFF) -- VRAM is 0x06000000 - 0x0601FFFF
  in if offset < length (vram ppuState)
     then (vram ppuState) !! offset
     else 0

-- | Placeholder for writing to VRAM
writeVRAM :: Word32 -> Word8 -> PPUState -> PPUState
writeVRAM addr value ppuState =
  let offset = fromIntegral (addr .&. 0x1FFFF)
  in if offset < length (vram ppuState)
     then ppuState { vram = updateAt (vram ppuState) offset value }
     else ppuState

-- | Placeholder for accessing Palette RAM
readPaletteRAM :: Word32 -> PPUState -> Word16
readPaletteRAM addr ppuState =
  let offset = fromIntegral (addr .&. 0x3FE) `div` 2 -- Palette RAM is 0x05000000 - 0x050003FE (16-bit words)
  in if offset < length (paletteRAM ppuState)
     then (paletteRAM ppuState) !! offset
     else 0

-- | Placeholder for writing to Palette RAM
writePaletteRAM :: Word32 -> Word16 -> PPUState -> PPUState
writePaletteRAM addr value ppuState =
  let offset = fromIntegral (addr .&. 0x3FE) `div` 2
  in if offset < length (paletteRAM ppuState)
     then ppuState { paletteRAM = updateAt (paletteRAM ppuState) offset value }
     else ppuState

-- Helper function to update an element in a list
updateAt :: [a] -> Int -> a -> [a]
updateAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

foreign import ccall "pokeElemOff" pokeElemOff :: Ptr a -> Int -> a -> IO ()
foreign import ccall "castPtr" castPtr :: Ptr a -> Ptr b
