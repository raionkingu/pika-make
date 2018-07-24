module Routines ( die
                , getFirstExistingAmong
                , parseCheck
                , getAllSimpleInstructionUses, getAllMultiInstructionUses
                , fillInSimples, fillInMultis
                , shouldGetExecuted, execWhileOK
                ) where

import System.Exit (exitFailure)
import System.Directory (doesFileExist, getModificationTime)
import System.Process (system)

import Data.List (find, nub)
import Data.Either (partitionEithers)

import qualified InstructionDefinition as InsDef
import qualified InstructionUse as InsUse

import qualified Parser
import qualified Checking

import qualified RuleProcessing as RP
import qualified InstructionFilling as IF

die msg = do putStrLn $ "error:\t" ++ msg
             exitFailure

getFirstExistingAmong :: [String] -> IO (Maybe String)
getFirstExistingAmong candidates =
  do bools <- mapM doesFileExist candidates
     return $ let zips = zip candidates bools
              in fmap fst $ find snd zips

parseCheck :: String -> Either String Parser.Pikafile
parseCheck raw =
  case Parser.parse raw of
    Left (line, msg) -> Left $ msg ++ " (line " ++ show line ++ ")"
    Right parsed ->
      case Checking.check parsed of
        Nothing -> Right parsed
        Just msg -> Left msg

getAllSimpleInstructionUses :: [String] -> Parser.Pikafile -> [(String, (InsDef.InstructionDefinition, InsUse.InstructionUse))]
getAllSimpleInstructionUses todo pikadata =
  let withDeps = let merge = nub . concat
                 in merge $ map (RP.getDependenciesFrom pikadata) todo
      aux dep = zip (repeat dep) $ RP.getSimpleUsesFrom pikadata dep
  in concat $ map aux withDeps

getAllMultiInstructionUses :: [String] -> Parser.Pikafile -> [(String, (InsDef.InstructionDefinition, InsUse.InstructionUse))]
getAllMultiInstructionUses todo pikadata =
  let withDeps = let merge = nub . concat
                 in merge $ map (RP.getDependenciesFrom pikadata) todo
      aux dep = zip (repeat dep) $ RP.getMultiUsesFrom pikadata dep
  in concat $ map aux withDeps

fillInSimples :: [(String, (InsDef.InstructionDefinition, InsUse.InstructionUse))] -> IO (Either String [IF.FilledIn])
fillInSimples simpleUses =
  do filledin <- let aux (rule, (insdef, insuse)) =
                       let dirs = InsUse.arguments insuse
                       in IF.fillInInstruction rule dirs [] insdef
                 in mapM aux simpleUses
     case partitionEithers filledin of
       ([], goods) -> return (Right goods)
       (fails, _) -> return (Left $ head fails)

fillInMultis :: [((String, (InsDef.InstructionDefinition, InsUse.InstructionUse)), [(String, String)])] -> IO (Either String [IF.FilledIn])
fillInMultis pairs =
  do filledin <- let aux ((rule, (insdef, insuse)), deps) =
                       let dirs = InsUse.arguments insuse
                       in IF.fillInInstruction rule dirs deps insdef
                 in mapM aux pairs
     case partitionEithers filledin of
       ([], goods) -> return (Right goods)
       (fails, _) -> return (Left $ head fails)

shouldGetExecuted :: IF.FilledIn -> IO Bool
shouldGetExecuted fi =
  do let out_names = map IF.filledIn $ IF.outputs fi
     existing <- mapM doesFileExist out_names
     if any not existing
     then return True
     else do let in_names = map IF.filledIn $ IF.inputs fi
             in_times <- mapM getModificationTime in_names
             out_times <- mapM getModificationTime out_names
             case compare (maximum in_times) (minimum out_times) of
               GT -> return True
               _ -> return False

execWhileOK :: [(String, String, [String])] -> IO Bool
execWhileOK [] = return True
execWhileOK ((msg, instruction, outs):xs) =
  do putStrLn msg
     system instruction
     existing <- mapM doesFileExist outs
     if any not existing
     then return False
     else execWhileOK xs
