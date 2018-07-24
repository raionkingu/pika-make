module RuleProcessing ( getDependenciesFrom, getUsesFrom
                      , getSimpleUsesFrom, getMultiUsesFrom
                      ) where

import qualified InstructionDefinition as InsDef
import qualified RuleDefinition as RuleDef
import qualified InstructionUse as InsUse
import qualified Parser

import Data.List (find)

--  This here supposes that all logic checks from Checking.hs have been done

isRule :: String -> (RuleDef.RuleDefinition, [InsUse.InstructionUse]) -> Bool
isRule rule (rdef, _) = RuleDef.name rdef == rule

isInsDef :: String -> InsDef.InstructionDefinition -> Bool
isInsDef name insdef = InsDef.name insdef == name

getDirectDependenciesFrom :: String -> Parser.Pikafile -> [String]
getDirectDependenciesFrom rule = dependencies . find (isRule rule) . Parser.rules
  where dependencies (Just (rdef, _)) = RuleDef.dependencies rdef
        dependencies Nothing = error $ "undefined rule '" ++ rule ++ "'"

getDependenciesFrom :: Parser.Pikafile -> String -> [String]
getDependenciesFrom pikafile rule =
  let rec [] results = results
      rec todo@(x:xs) done
        | x `elem` done = rec xs done
        | otherwise = let getdeps = flip getDirectDependenciesFrom pikafile
                          notdone = filter (not . flip elem done)
                          notxs = filter (not . flip elem xs)
                          aux = notxs . notdone . getdeps
                      in rec (xs ++ aux x) (x : done)
  in rec [rule] []

getUsesFrom :: Parser.Pikafile -> String -> [(InsDef.InstructionDefinition, InsUse.InstructionUse)]
getUsesFrom pikafile rule =
  case find (isRule rule) $ Parser.rules pikafile of
    Nothing -> error $ "undefined rule '" ++ rule ++ "'"
    Just (_, uses) -> map aux uses
  where insdefs = Parser.insdefs pikafile
        aux insuse = let name = InsUse.name insuse
                     in case find (isInsDef name) insdefs of
                          Nothing -> error $ "rule '" ++ rule ++ "' tries to use " ++
                                             "undefined instruction '" ++ name ++ "'"
                          Just insdef -> (insdef, insuse)

getSimpleUsesFrom :: Parser.Pikafile -> String -> [(InsDef.InstructionDefinition, InsUse.InstructionUse)]
getSimpleUsesFrom pikafile = filter isSimple . getUsesFrom pikafile
  where isSimple = not . InsDef.isMultiInstruction . fst

getMultiUsesFrom :: Parser.Pikafile -> String -> [(InsDef.InstructionDefinition, InsUse.InstructionUse)]
getMultiUsesFrom pikafile = filter isMulti . getUsesFrom pikafile
  where isMulti = InsDef.isMultiInstruction . fst
