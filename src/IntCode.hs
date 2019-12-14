module IntCode (
    execute,
    executeWithIo,
    loadMemory,
    dumpMemory,
    Error
) where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (iterateWhile)
import Data.Sort (sortOn)


type Addr = Int

type Memory = M.Map Addr Int

data Error = OutOfBounds Addr Int | BadOpCode Addr Int deriving (Eq)

instance Show Error where
    show (OutOfBounds pc value) = "Out of bounds: " ++ (show value) ++ " at " ++ (show pc)
    show (BadOpCode pc value) = "Bad opcode: " ++ (show value) ++ " at " ++ (show pc)

data MachineState = MS {
    getMemory :: Memory,
    getPc :: Addr,
    getInput :: [Int],
    getOutput :: [Int]
}


type Machine a = StateT MachineState (Except Error) a

data Status = Continue | Halt

data Mode = Position | Immediate

data OpCode = 
    Add (Mode, Mode) | 
    Mul (Mode, Mode) | 
    Hlt | 
    Inp | 
    Out Mode |
    Jz (Mode, Mode) |
    Jnz (Mode, Mode) |
    Lt (Mode, Mode) |
    Eql (Mode, Mode)


isContinue :: Status -> Bool
isContinue Continue = True
isContinue _ = False


load :: Addr -> Machine Int
load addr = do
    memory <- getMemory <$> get
    pc <- getPc <$> get
    case M.lookup addr memory of
        Nothing -> throwError $ OutOfBounds pc addr
        Just value -> return value


store :: Addr -> Int -> Machine ()
store addr value = 
    modify (\ ms -> ms { getMemory = M.insert addr value (getMemory ms)})


operand :: Mode -> Addr -> Machine Int
operand Position addr = load =<< load addr
operand Immediate addr = load addr


comparison :: (Int -> Int -> Bool) -> Int -> Int -> Int
comparison op left right = if op left right then 1 else 0


performOperation :: (Mode, Mode) -> (Int -> Int -> Int) -> Machine ()
performOperation (leftMode, rightMode) operation = do
    pc <- getPc <$> get
    left <- operand leftMode (pc + 1)
    right <- operand rightMode (pc + 2)
    destination <- load (pc + 3)

    store destination (operation left right)
    advanceOperation 4


performInput :: Machine ()
performInput = do
    pc <- getPc <$> get
    dest <- load (pc + 1)
    input <- getInput <$> get

    store dest (head input)
    modify (\ms -> ms { getInput = tail input})
    advanceOperation 2


performOutput :: Mode -> Machine ()
performOutput mode = do
    pc <- getPc <$> get
    source <- operand mode (pc + 1)
    
    modify (\ms -> ms { getOutput = source : (getOutput ms)})
    advanceOperation 2


performJump :: (Mode, Mode) -> (Int -> Bool) -> Machine ()
performJump (comparisandMode, destMode) pred = do
    pc <- getPc <$> get
    comparisand <- operand comparisandMode (pc + 1)
    dest <- operand destMode (pc + 2)
    
    if (pred comparisand) then 
        modify (\ms -> ms { getPc = dest })
    else
        advanceOperation 3


advanceOperation :: Int -> Machine ()
advanceOperation size = modify (\ ms -> ms { getPc = (getPc ms) + size })


mode :: Int -> Int -> Mode
mode position instruction = case (instruction `div` (10 ^ (2 + position))) `mod` 10 of
    0 -> Position
    1 -> Immediate


decode :: Int -> Maybe OpCode
decode instruction = case instruction `mod` 100 of
    1 -> Just (Add (mode 0 instruction, mode 1 instruction))
    2 -> Just (Mul (mode 0 instruction, mode 1 instruction))
    3 -> Just Inp
    4 -> Just (Out (mode 0 instruction))
    5 -> Just (Jnz (mode 0 instruction, mode 1 instruction))
    6 -> Just (Jz (mode 0 instruction, mode 1 instruction))
    7 -> Just (Lt (mode 0 instruction, mode 1 instruction))
    8 -> Just (Eql (mode 0 instruction, mode 1 instruction))
    99 -> Just Hlt
    _ -> Nothing


executeOperation :: Machine Status
executeOperation = do
    pc <- getPc <$> get
    opcode <- load pc

    op <- maybe (throwError $ BadOpCode pc opcode) return (decode opcode) 

    result <- case op of
            Add modes -> performOperation modes (+) >> return Continue
            Mul modes -> performOperation modes (*) >> return Continue
            Inp -> performInput >> return Continue
            Out mode -> performOutput mode >> return Continue
            Jnz modes -> performJump modes (/= 0) >> return Continue
            Jz modes -> performJump modes (== 0) >> return Continue
            Lt modes -> performOperation modes (comparison (<)) >> return Continue
            Eql modes -> performOperation modes (comparison (==)) >> return Continue
            Hlt -> return Halt
    
    return result


dumpMemory :: Memory -> [Int]
dumpMemory memory = map snd . sortOn fst $ M.toList memory 


loadMemory :: [Int] -> Memory
loadMemory source = M.fromList $ zip [0..] source 
    

execute' :: Machine ()
execute' = do 
    iterateWhile isContinue executeOperation
    return ()


runMachine :: [Int] -> [Int] -> Either Error MachineState
runMachine program input = runExcept $ execStateT execute' (MS (loadMemory program) 0 input [])


execute :: [Int] -> Either Error [Int]
execute program = dumpMemory <$> getMemory <$> runMachine program []


executeWithIo :: [Int] -> [Int] -> Either Error [Int]
executeWithIo program input = reverse <$> getOutput <$> runMachine program input