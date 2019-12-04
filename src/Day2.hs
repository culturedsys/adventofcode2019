module Day2 (
    parser,
    execute,
    result,
    result2,
    loadMemory,
    dumpMemory
) where

import qualified Data.Map.Lazy as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops
import Data.Sort (sortOn)
import Debug.Trace
import Data.List.Split (splitOn)

type Addr = Int

type Memory = M.Map Addr Int

data Error = OutOfBounds Addr Int | BadOpCode Addr Int deriving (Eq)

instance Show Error where
    show (OutOfBounds pc value) = "Out of bounds: " ++ (show value) ++ " at " ++ (show pc)
    show (BadOpCode pc value) = "Bad opcode: " ++ (show value) ++ " at " ++ (show pc)

type Machine a = StateT (Memory, Int) (Except Error) a

data Status = Continue | Halt


isContinue :: Status -> Bool
isContinue Continue = True
isContinue _ = False


load :: Addr -> Machine Int
load addr = do
    (memory, pc) <- get
    case M.lookup addr memory of
        Nothing -> throwError $ OutOfBounds pc addr
        Just value -> return value


store :: Addr -> Int -> Machine ()
store addr value= do
    (memory, pc) <- get
    put (M.insert addr value memory, pc)    


performOperation :: (Int -> Int -> Int) -> Machine ()
performOperation operation = do
    (_, pc) <- get
    left <- load =<< load (pc + 1)
    right <- load =<< load (pc + 2)
    destination <- load (pc + 3)

    store destination (operation left right)

    
advanceOperation :: Machine ()
advanceOperation = modify (\ (memory, pc) -> (memory, pc + 4))


executeOperation :: Machine Status
executeOperation = do
    (_, pc) <- get
    opcode <- load pc

    result <- case opcode of
            1 -> performOperation (+) >> return Continue
            2 -> performOperation (*) >> return Continue
            99 -> return Halt
            _ -> throwError $ BadOpCode pc opcode
    
    advanceOperation
    return result


dumpMemory :: Memory -> [Int]
dumpMemory memory = map snd . sortOn fst $ M.toList memory 


loadMemory :: [Int] -> Memory
loadMemory source = M.fromList $ zip [0..] source 
    

execute' :: Machine ()
execute' = do 
    iterateWhile isContinue executeOperation
    return ()


execute :: [Int] -> Either Error [Int]
execute input = 
    case runExcept $ execStateT execute' (loadMemory input, 0) of
        Right (finalMemory, _) -> Right $ dumpMemory finalMemory
        Left error -> Left error


result :: [Int] -> Either Error Int
result input = executeProgram input 12 2


executeProgram :: [Int] -> Int -> Int -> Either Error Int
executeProgram input noun verb =
    let program = (head input) : [noun, verb] ++ (drop 3 input) in
        case execute program of
            Left error -> Left error
            Right finalState -> Right $ head finalState


searchPrograms :: [Int] -> Int -> [(Int, Int)]
searchPrograms input target = do
    noun <- [0..99]
    verb <- [0..99]
    case executeProgram input noun verb of
        Left error -> []
        Right result -> if result == target then [(noun, verb)] else []


result2 :: Int -> [Int] -> (Int, Int)
result2 target input = head $ searchPrograms input target

parser :: String -> [Int]
parser = map read . (splitOn ",")