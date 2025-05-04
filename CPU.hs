--Might work might not idk

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module CPU (
  CPUState (..),
  newCPUState,
  stepCPU,
  registers,
  pc,
  cpsr,
  getFlag,
) where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, testBit)
import Data.Kind (Type)
import Data.Word (Word16, Word32, Word8)
import qualified RAM
import Numeric (showHex)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (Void)
import Prelude hiding (Bool, Char, Eq, Functor, IO, Monad, Show, fromIntegral, not, or)
import qualified Prelude as P

-- Re-establish necessary Prelude types and classes
type Bool = P.Bool
type Char = P.Char
class Eq a where (==) :: a -> a -> Bool; (/=) :: a -> a -> Bool
instance Eq Bool where True == True = True; False == False = True; _ == _ = False; a /= b = not (a == b)
class Show a where show :: a -> P.String
instance Show Word32 where show = P.show
instance Show Word16 where show = P.show
instance Show Word8 where show = P.show
not :: Bool -> Bool; not True = False; not False = True
or :: Bool -> Bool -> Bool; True `or` _ = True; _ `or` True = True; False `or` False = False
fromIntegral :: (P.Integral a, Num b) => a -> b
class Functor f where fmap :: (a -> b) -> f a -> f b
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
instance Monad IO where
  (>>=) = (P.=<<)
  return = P.return

-- | Phantom types to represent CPU modes at the type level
data Mode = User | FIQ | IRQ | Supervisor | Abort | Undefined | System

-- | Phantom types to represent CPU state (ARM/Thumb)
data ISA = ARM | ThumbISA

-- | GADT to represent different CPU states, indexed by mode and ISA
data CPUState (mode :: Mode) (isa :: ISA) = CPUState
  { registers :: !(RegFile mode)
  , pc :: !Word32
  , cpsr :: !(CPSR mode isa)
  , fetchedInstruction :: !Word32
  , decodedInstruction :: !(Instruction isa)
  }

-- | Type family for register file, specialized by CPU mode (for banked registers)
type family RegFile (mode :: Mode) :: Type where
  RegFile User = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14
  RegFile FIQ = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14 (FIQ banks R8-R14)
  RegFile IRQ = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14 (IRQ banks R13-R14)
  RegFile Supervisor = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14 (SVC banks R13-R14)
  RegFile Abort = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14 (ABT banks R13-R14)
  RegFile Undefined = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14 (UND banks R13-R14)
  RegFile System = !(Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -- R0-R14

-- | GADT for CPSR, including type-level tracking of ISA
data CPSR (mode :: Mode) (isa :: ISA) where
  ARM_CPSR :: { nFlag :: !Bool, zFlag :: !Bool, cFlag :: !Bool, vFlag :: !Bool, iFlag :: !Bool, fFlag :: !Bool, tFlag :: !(isa :~: ARM) -> Void, modeBits :: !Word8 } -> CPSR mode ARM
  Thumb_CPSR :: { nFlag :: !Bool, zFlag :: !Bool, cFlag :: !Bool, vFlag :: !Bool, iFlag :: !Bool, fFlag :: !Bool, tFlag :: !(isa :~: ThumbISA) -> Void, modeBits :: !Word8 } -> CPSR mode ThumbISA

-- | Type family to get the Word size of an instruction based on ISA
type family InstructionWord (isa :: ISA) :: Type where
  InstructionWord ARM = Word32
  InstructionWord ThumbISA = Word16

-- | GADT for instructions, parameterized by ISA
data Instruction (isa :: ISA) where
  ARMInstruction :: !ARMInstruction' -> Instruction ARM
  ThumbInstruction :: !ThumbInstruction' -> Instruction ThumbISA
  UndefinedInstruction :: !(InstructionWord isa) -> Instruction isa

-- | Data type for specific ARM instructions (more detailed)
data ARMInstruction' =
    ARMDataProcessing
      { dpOpcode :: !Word8
      , rn :: !Word8
      , rd :: !Word8
      , operand2 :: !ARMOperand2
      , sBit :: !Bool
      }
  | ARMBranch'
      { linkFlag :: !Bool
      , offset :: !Word32 -- Signed 24-bit offset
      }
  | ARMLoadStoreImmOffset
      { loadFlag :: !Bool
      , byteFlag :: !Bool
      , writebackFlag :: !Bool
      , preIndexFlag :: !Bool
      , upFlag :: !Bool
      , rn :: !Word8
      , rd :: !Word8
      , offset :: !Word16 -- 12-bit immediate offset
      }
  -- More ARM instructions
  | UndefinedARM Word32
  deriving (Show)

-- | Data type for ARM Operand 2 (immediate or shifted register)
data ARMOperand2 =
    ARMImmediateOperand !Word32
  | ARMShiftedRegisterOperand !Word8 !ShiftType !Word8
  deriving (Show)

-- | Data type for shift types
data ShiftType = LSL' | LSR' | ASR' | ROR' deriving (Show)

-- | Data type for specific Thumb instructions (more detailed)
data ThumbInstruction' =
    ThumbMoveShiftedReg
      { shiftOpcode :: !Word8
      , rd :: !Word8
      , rs :: !Word8
      , offset5 :: !Word8
      }
  | ThumbAddSubReg
      { op :: !Word8 -- 0 for ADD, 1 for SUB
      , rd :: !Word8
      , rn :: !Word8
      , rs :: !Word8
      }
  | ThumbAddSubImm3
      { op :: !Word8
      , rd :: !Word8
      , rn :: !Word8
      , imm3 :: !Word8
      }
  | ThumbAddSubImm8
      { op :: !Word8
      , rd :: !Word8
      , imm8 :: !Word8
      }
  | ThumbMoveCompAddSubImm8
      { opcode :: !Word8 -- 00 for MOV, 01 for CMP, 10 for ADD, 11 for SUB
      , rd :: !Word8
      , imm8 :: !Word8
      }
  | ThumbBranch'
      { cond :: !Word8
      , offset :: !Word16 -- Signed 8-bit offset, shifted left by 1
      }
  -- More Thumb instructions
  | UndefinedThumb Word16
  deriving (Show)

-- | Creates a new initial CPU state (still needs proper initial values)
newCPUState :: Word32 -> IO (CPUState 'User 'ARM)
newCPUState initialPC = return $ CPUState
  { registers = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x03007F00, 0)
  , pc = initialPC
  , cpsr = ARM_CPSR { nFlag = False, zFlag = False, cFlag = False, vFlag = False, iFlag = False, fFlag = False, tFlag = Refl, modeBits = 0b10000 } -- User mode, ARM state (tFlag Refl proves isa is ARM)
  , fetchedInstruction = 0
  , decodedInstruction = UndefinedInstruction 0
  }

-- | Steps the CPU by one instruction
stepCPU :: CPUState mode isa -> RAM.Memory -> IO (CPUState mode isa') where
  stepCPU cpuState memory = do
    let currentPC = pc cpuState
    instructionWord <- case cpsr cpuState of
      ARM_CPSR {} -> RAM.readWord memory currentPC
      Thumb_CPSR {} -> fmap fromIntegral (RAM.readHalfword memory currentPC)

    let nextPC = currentPC + (case cpsr cpuState of ARM_CPSR {} -> 4; Thumb_CPSR {} -> 2)
    let newCPUStateWithFetch = cpuState { fetchedInstruction = instructionWord, pc = nextPC }

    -- Decode the instruction based on the current ISA
    (decodedInstr, newISA) <- case cpsr cpuState of
      armCPSR@ARM_CPSR{} -> return (ARM (decodeARMInstruction instructionWord), ARM)
      thumbCPSR@Thumb_CPSR{} -> return (Thumb (decodeThumbInstruction (fromIntegral instructionWord)), ThumbISA)

    let newCPSR = case newISA of
          ARM -> case cpsr cpuState of armCPSR -> armCPSR { tFlag = Refl }
          ThumbISA -> case cpsr cpuState of _ -> Thumb_CPSR { nFlag = nFlag (cpsr cpuState), zFlag = zFlag (cpsr cpuState), cFlag = cFlag (cpsr cpuState), vFlag = vFlag (cpsr cpuState), iFlag = iFlag (cpsr cpuState), fFlag = fFlag (cpsr cpuState), tFlag = Refl, modeBits = modeBits (cpsr cpuState) }

    let newCPUStateWithDecode = newCPUStateWithFetch { decodedInstruction = decodedInstr, cpsr = newCPSR }

    -- Execute the instruction
    executeInstruction newCPUStateWithDecode memory

-- | Decodes an ARM instruction (more comprehensive but still incomplete)
decodeARMInstruction :: Word32 -> ARMInstruction'
decodeARMInstruction instr =
  let cond = (instr `shiftR` 28) .&. 0xF
      opCodeGroup = (instr `shiftR` 26) .&. 0x3
      opCodeMisc = (instr `shiftR` 21) .&. 0xF
      rn' = fromIntegral $ (instr `shiftR` 16) .&. 0xF
      rd' = fromIntegral $ (instr `shiftR` 12) .&. 0xF
      sBit' = (instr `testBit` 20)
  in case opCodeGroup of
    0b00 -> case opCodeMisc of
      0b0 -> ARMDataProcessing { dpOpcode = fromIntegral ((instr `shiftR` 21) .&. 0xF)
                               , rn = rn'
                               , rd = rd'
                               , operand2 = decodeARMOperand2 instr
                               , sBit = sBit'
                               }
      0b1 -> UndefinedARM instr -- Single Data Transfer (shifted register offset)
      0b2 -> ARMLoadStoreImmOffset { loadFlag = instr `testBit` 20
                                   , byteFlag = instr `testBit` 22
                                   , writebackFlag = instr `testBit` 21
                                   , preIndexFlag = instr `testBit` 24
                                   , upFlag = instr `testBit` 23
                                   , rn = rn'
                                   , rd = rd'
                                   , offset = fromIntegral (instr .&. 0xFFF)
                                   }
      -- More cases for opcode 0
      _ -> UndefinedARM instr
    0b10 -> ARMBranch' { linkFlag = instr `testBit` 24
                        , offset = instr .&. 0x00FFFFFF
                        }
    -- More opcode groups
    _ -> UndefinedARM instr

-- | Decodes ARM Operand 2
decodeARMOperand2 :: Word32 -> ARMOperand2
decodeARMOperand2 instr
  | not (instr `testBit` 25) = -- Immediate operand
    let rotateAmount = fromIntegral $ (instr `shiftR` 8) .&. 0xF
        imm = instr .&. 0xFF
    in ARMImmediateOperand $ P.rotateR imm (rotateAmount * 2)
  | otherwise = -- Shifted register operand
    let shiftAmount = fromIntegral $ (instr `shiftR` 7) .&. 0x1F
        shiftTypeBits = (instr `shiftR` 5) .&. 0x3
        rm = fromIntegral $ instr .&. 0xF
        shiftType = case shiftTypeBits of
          0b00 -> LSL'
          0b01 -> LSR'
          0b10 -> ASR'
          0b11 -> ROR'
          _ -> error "Invalid shift type"
    in ARMShiftedRegisterOperand shiftAmount shiftType rm

-- | Decodes a Thumb instruction (more comprehensive but still incomplete)
decodeThumbInstruction :: Word16 -> ThumbInstruction'
decodeThumbInstruction instr =
  let opGroup = (instr `shiftR` 13) .&. 0x7
  in case opGroup of
    0b000 -> let subOp = (instr `shiftR` 11) .&. 0x3
                 rd' = fromIntegral $ (instr `shiftR` 0) .&. 0x7
                 rs' = fromIntegral $ (instr `shiftR` 3) .&. 0x7
                 offset5' = fromIntegral $ (instr `shiftR` 6) .&. 0x1F
             in case subOp of
                  0b00 -> ThumbMoveShiftedReg { shiftOpcode = 0, rd = rd', rs = rs', offset5 = offset5' } -- LSL
                  0b01 -> ThumbMoveShiftedReg { shiftOpcode = 1, rd = rd', rs = rs', offset5 = offset5' } -- LSR
                  0b10 -> ThumbMoveShiftedReg { shiftOpcode = 2, rd = rd', rs = rs', offset5 = offset5' } -- ASR
                  0b11 -> let opcode' = (instr `shiftR` 9) .&. 0x3
                              rn' = fromIntegral $ (instr `shiftR` 6) .&. 0x7
                          in case opcode' of
                               0b00 -> ThumbAddSubReg { op = 0, rd = rd', rn = rn', rs = rs' } -- ADD reg
                               0b01 -> ThumbAddSubReg { op = 1, rd = rd', rn = rn', rs = rs' } -- SUB reg
                               0b10 -> ThumbAddSubImm3 { op = 0, rd = rd', rn = rn', imm3 = offset5' } -- ADD imm3 (using offset5 field)
                               0b11 -> ThumbAddSubImm3 { op = 1, rd = rd', rn = rn', imm3 = offset5' } -- SUB imm3 (using offset5 field)
                               _ -> UndefinedThumb instr
                  _ -> Und
