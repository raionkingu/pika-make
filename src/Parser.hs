module Parser ( Pikafile(..)
              , parse
              ) where

import Data.Char (isSpace, isAlphaNum)
import Data.List (partition)
import Data.Either (partitionEithers)

import Utils (isLeft, fromRight
             , rtrim
             , map1, map2, mapR)
import qualified Commons

import qualified InstructionDefinition as InsDef
import qualified RuleDefinition as RuleDef
import qualified InstructionUse as InsUse

data Pikafile = Pikafile { insdefs :: [InsDef.InstructionDefinition]
                         , rules :: [(RuleDef.RuleDefinition, [InsUse.InstructionUse])]
                         } deriving (Show, Eq)

--  from an array of lines, make the (harmless) transformations for the parsing
prepare :: [String] -> [(Int, String)]
prepare = let uncomment = takeWhile (/= '#')
              notnull = not . null . snd
          in filter notnull . zip [1..] . map (uncomment . rtrim)

--  classify each line
data LineType = INSDEF | RULEDEF | INSUSE deriving (Show, Eq)

classifyLine :: String -> Maybe LineType
classifyLine ('\t':_) = Just INSUSE
classifyLine line =
  case removeName line of
    '=':_ -> Just INSDEF
    ':':_ -> Just RULEDEF
    _ -> Nothing
  where removeName = dropWhile isSpace . dropWhile Commons.isNameChar

classifyNonEmpty :: (Int, String) -> Either (Int, String) (LineType, (Int, String))
classifyNonEmpty (line, text) =
  case classifyLine text of
    Nothing -> Left (line, "cannot classify line '" ++ text ++ "'")
    Just lt -> Right (lt, (line, text))

--  actual parsing
groupRules [] = Right []
groupRules ((INSUSE, (line, _)):_) = Left (line, "instruction use without rule")
groupRules ((RULEDEF, (line, text)):xs) =
  let (insuses, after) = map1 (map snd) $ span ((== INSUSE) . fst) xs
  in case RuleDef.parse text of
       Left msg -> Left (line, msg)
       Right ruledef ->
         let mkInsUse (l, t) = case InsUse.parse t of
                                 Left msg -> Left (l, msg)
                                 Right insuse -> Right insuse
         in case partitionEithers $ map mkInsUse insuses of
              ([], goods) -> mapR ((:) (ruledef, goods)) $ groupRules after
              (fails, _) -> Left $ head fails

parse :: String -> Either (Int, String) Pikafile
parse input =
  case partitionEithers $ classify input of
    ([], goods) ->
      let (insdefs', rulegroups) = let (ids, ngs) = partition ((== INSDEF) . fst) goods
                                   in (map (map2 InsDef.parse . snd) ids, groupRules ngs)
          insdefs'' = case partition (isLeft . snd) insdefs' of
                        ([], rights) -> Right $ map (fromRight . snd) rights
                        (lefts, _) -> let (line, Left l) = head lefts in Left (line, l)
      in case insdefs'' of
           Left pair -> Left pair
           Right ids ->
             case rulegroups of
               Left pair -> Left pair
               Right rgs -> Right Pikafile { insdefs = ids
                                           , rules = rgs
                                           }
    (fails, _) -> Left $ head fails
  where classify = map classifyNonEmpty . prepare . lines
