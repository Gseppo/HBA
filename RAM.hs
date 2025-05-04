— Memory Management written before Repo was      
—made should maybe work. not the most refined —but not horrible.

module RAM (
  Address,
  Memory,
  newMemory,
  readByte,
  writeByte,
  readHalfword,
  writeHalfword,
  readWord,
  writeWord,
  -- Exporting memory regions for potential direct access (with caution)
  wramRegion,
  ewramRegion,
  romRegion,
  paletteRAMRegion,
  vramRegion,
  oamRegion,
  ioRegistersRegion,
) where

import Data.ByteString as BS
import Data.ByteString.Mutable as MBS
import Data.Word
import Data.Bits
import Control.Monad.IO.Class (MonadIO)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV

-- | Represents a memory address (32-bit unsigned integer for GBA)
type Address = Word32

-- | Represents a specific memory region with its bounds
data MemoryRegion = MemoryRegion
  { startAddress :: Address
  , size :: Int
  , mutable :: Bool
  , name :: String
  }

-- | Definition of the GBA's memory map
memoryMap :: [MemoryRegion]
memoryMap =
  [ MemoryRegion 0x00000000 0x00004000 False "BIOS ROM" -- 16 KB
  , MemoryRegion 0x02000000 0x00040000 True "WRAM (Onboard)" -- 256 KB
  , MemoryRegion 0x03000000 0x00040000 True "EWRAM (External)" -- 256 KB
  , MemoryRegion 0x04000000 0x00000400 True "IO Registers" -- 1 KB
  , MemoryRegion 0x05000000 0x00000400 True "Palette RAM" -- 1 KB
  , MemoryRegion 0x06000000 0x00018000 True "VRAM" -- 96 KB
  , MemoryRegion 0x07000000 0x00000400 True "OAM" -- 1 KB
  , MemoryRegion 0x08000000 0x02000000 False "Game Pak ROM" -- Up to 32 MB
  , MemoryRegion 0x0A000000 0x00008000 False "Game Pak SRAM" -- Up to 32 KB (typically) - Mutable in reality, handled separately later
  , MemoryRegion 0x0E000000 0x00000200 False "Game Pak Backup (Flash/EEPROM)" -- Handled separately
  ]

-- | Represents the entire GBA memory using mutable vectors for RAM regions
data Memory = Memory
  { wramRegion :: IOVector Word8
  , ewramRegion :: IOVector Word8
  , romRegion :: BS.ByteString
  , paletteRAMRegion :: IOVector Word8
  , vramRegion :: IOVector Word8
  , oamRegion :: IOVector Word8
  , ioRegistersRegion :: IOVector Word8
  }

-- | Creates a new initialized memory structure
newMemory :: BS.ByteString -> IO Memory
newMemory gameRom = do
  wram <- MV.new $ sizeForRegion "WRAM (Onboard)"
  ewram <- MV.new $ sizeForRegion "EWRAM (External)"
  paletteRAM <- MV.new $ sizeForRegion "Palette RAM"
  vram <- MV.new $ sizeForRegion "VRAM"
  oam <- MV.new $ sizeForRegion "OAM"
  ioRegisters <- MV.new $ sizeForRegion "IO Registers"
  return $ Memory wram ewram gameRom paletteRAM vram oam ioRegisters
  where
    sizeForRegion name' = case findRegionByName name' of
      Just region -> size region
      Nothing -> error $ "Unknown memory region: " ++ name'

    findRegionByName n = find (\r -> name r == n) memoryMap

-- | Helper function to get the base address and the actual memory block for a given address
getAddressAndMemory :: Memory -> Address -> Maybe (IOVector Word8, Int)
getAddressAndMemory mem addr
  | addr >= 0x02000000 && addr < 0x02040000 = Just (wramRegion mem, fromIntegral $ addr - 0x02000000)
  | addr >= 0x03000000 && addr < 0x03040000 = Just (ewramRegion mem, fromIntegral $ addr - 0x03000000)
  | addr >= 0x05000000 && addr < 0x05000400 = Just (paletteRAMRegion mem, fromIntegral $ addr - 0x05000000)
  | addr >= 0x06000000 && addr < 0x06018000 = Just (vramRegion mem, fromIntegral $ addr - 0x06000000)
  | addr >= 0x07000000 && addr < 0x07000400 = Just (oamRegion mem, fromIntegral $ addr - 0x07000000)
  | addr >= 0x04000000 && addr < 0x04000400 = Just (ioRegistersRegion mem, fromIntegral $ addr - 0x04000000)
  | addr >= 0x08000000 && addr < 0x0A000000 = Nothing -- ROM is handled separately
  | otherwise = Nothing

-- | Reads a byte from the specified address
readByte :: Memory -> Address -> IO Word8
readByte mem addr
  | addr >= 0x08000000 && addr < 0x0A000000 =
    if fromIntegral (addr - 0x08000000) < BS.length (romRegion mem)
      then return $ BS.index (romRegion mem) (fromIntegral $ addr - 0x08000000)
      else return 0 -- Reading out of bounds of ROM returns 0
  | otherwise = case getAddressAndMemory mem addr of
    Just (region, offset) -> MV.read region offset
    Nothing -> return 0 -- Access to undefined memory returns 0

-- | Writes a byte to the specified address
writeByte :: Memory -> Address -> Word8 -> IO ()
writeByte mem addr
  | otherwise = case getAddressAndMemory mem addr of
    Just (region, offset) -> MV.write region offset val
    Nothing -> return () -- Writes to undefined or read-only memory are ignored
  where
    -- Need to be careful about writing to ROM or other read-only areas
    val = if addr >= 0x08000000 && addr < 0x0A000000
            then error "Attempted write to ROM" -- Or just ignore, depending on desired behavior
            else val

-- | Reads a halfword (16 bits) from the specified address
readHalfword :: Memory -> Address -> IO Word16
readHalfword mem addr = do
  -- Ensure alignment for halfword reads
  if addr `mod` 2 /= 0
    then return 0 -- Or throw an error for misaligned access
    else do
      byte1 <- readByte mem addr
      byte2 <- readByte mem (addr + 1)
      return $ (fromIntegral byte2 `shiftL` 8) .|. fromIntegral byte1

-- | Writes a halfword (16 bits) to the specified address
writeHalfword :: Memory -> Address -> Word16 -> IO ()
writeHalfword mem addr val = do
  -- Ensure alignment for halfword writes
  if addr `mod` 2 /= 0
    then return () -- Or throw an error
    else do
      writeByte mem addr (fromIntegral $ val .&. 0xFF)
      writeByte mem (addr + 1) (fromIntegral $ (val `shiftR` 8) .&. 0xFF)

-- | Reads a word (32 bits) from the specified address
readWord :: Memory -> Address -> IO Word32
readWord mem addr = do
  -- Ensure alignment for word reads
  if addr `mod` 4 /= 0
    then return 0 -- Or throw an error
    else do
      byte1 <- readByte mem addr
      byte2 <- readByte mem (addr + 1)
      byte3 <- readByte mem (addr + 2)
      byte4 <- readByte mem (addr + 3)
      return $ (fromIntegral byte4 `shiftL` 24)
               .|. (fromIntegral byte3 `shiftL` 16)
               .|. (fromIntegral byte2 `shiftL` 8)
               .|. fromIntegral byte1

-- | Writes a word (32 bits) to the specified address
writeWord :: Memory -> Address -> Word32 -> IO ()
writeWord mem addr val = do
  -- Ensure alignment for word writes
  if addr `mod` 4 /= 0
    then return () -- Or throw an error
    else do
      writeByte mem addr (fromIntegral $ val .&. 0xFF)
      writeByte mem (addr + 1) (fromIntegral $ (val `shiftR` 8) .&. 0xFF)
      writeByte mem (addr + 2) (fromIntegral $ (val `shiftR` 16) .&. 0xFF)
      writeByte mem (addr + 3) (fromIntegral $ (val `shiftR` 24) .&. 0xFF)
