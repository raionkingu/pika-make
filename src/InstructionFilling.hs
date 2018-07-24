module InstructionFilling ( FilledInTemplate(..)
                          , InstructionType(..)
                          , FilledIn(..)
                          , fillInInputParameter, fillInOutputParameter, fillInMultiParameter
                          , fillInInstruction
                          ) where

import qualified InstructionDefinition as InsDef

import Data.List (lookup, intercalate)
import Data.Maybe (fromJust)

import System.Directory (doesFileExist)

data FilledInTemplate = FilledInTemplate { original :: InsDef.Template
                                         , ruleName :: String
                                         , directoryName :: String
                                         , filledIn :: String
                                         } deriving (Show, Eq)

data InstructionType = SIMPLE | MULTI deriving (Show, Eq)

data FilledIn = FilledIn { instructionName :: String
                         , rule :: String
                         , itype :: InstructionType
                         , asString :: String
                         , inputs :: [FilledInTemplate]
                         , outputs :: [FilledInTemplate]
                         } deriving (Show, Eq)

type Directory = String
type RuleName = String

addDirectory :: Directory -> String -> String
addDirectory "./" str = str
addDirectory dir str = dir ++ str

fillInTemplate :: RuleName -> InsDef.Template -> String
fillInTemplate _ [] = ""
fillInTemplate rname (InsDef.Joker:xs) = rname ++ fillInTemplate rname xs
fillInTemplate rname ((InsDef.TextPart text):xs) = text ++ fillInTemplate rname xs

fillInInputParameter :: (Directory, RuleName) -> InsDef.InstructionPart -> IO (Either String FilledInTemplate)
fillInInputParameter (directory, rname) parameter =
  let names = map (addDirectory directory . fillInTemplate rname) $ InsDef.templates parameter
      pairs = zip names $ InsDef.templates parameter
      join [x] = "'" ++ x ++ "'"
      join [x, y] = "'" ++ x ++ "' and '" ++ y ++ "'"
      join xs = let (y, ys) = (last xs, init xs)
                in "'" ++ intercalate "', '" ys ++ "' and '" ++ y ++ "'"
  in do bools <- mapM doesFileExist names
        return $ let existing = filter snd $ zip names bools
                 in case map fst existing of
                      [] -> if length names == 1
                            then Left $ "could not find file " ++ join names
                            else Left $ "could not find any existing file among " ++ join names
                      [one] -> Right FilledInTemplate { original = fromJust $ lookup one pairs
                                                      , ruleName = rname
                                                      , directoryName = directory
                                                      , filledIn = one
                                                      }
                      more -> Left $ "don't know which file to use, among " ++ join more

fillInOutputParameter :: (Directory, RuleName) -> InsDef.InstructionPart -> [FilledInTemplate]
fillInOutputParameter (directory, rname) parameter =
  let names = map (addDirectory directory . fillInTemplate rname) $ InsDef.templates parameter
      pairs = zip names $ InsDef.templates parameter
      toFilledIn (a, b) = FilledInTemplate { original = b
                                           , ruleName = rname
                                           , directoryName = directory
                                           , filledIn = a
                                           }
  in map toFilledIn pairs

--  ugly
fillInMultiParameter :: [(Directory, RuleName)] -> InsDef.InstructionPart -> IO (Either String [FilledInTemplate])
fillInMultiParameter inputs parameter =
  --  somewhat hacky, but does the trick : for each input pair, get the filled-in templates
  let candidates = map (flip fillInOutputParameter parameter) inputs
      join [x] = "'" ++ x ++ "'"
      join [x, y] = "'" ++ x ++ "' and '" ++ y ++ "'"
      join xs = let (y, ys) = (last xs, init xs)
                in "'" ++ intercalate "', '" ys ++ "' and '" ++ y ++ "'"
      aux filledins =
        do bools <- mapM doesFileExist $ map filledIn filledins
           return $ let existing = filter snd $ zip filledins bools
                    in case map fst existing of
                         [] -> if length filledins == 1
                               then Left $ "could not find file " ++
                                           join (map filledIn filledins)
                               else Left $ "could not find any existing file among " ++
                                           join (map filledIn filledins)
                         [one] -> Right one
                         more -> Left $ "don't know which file to use, among " ++
                                        join (map filledIn more)
      rec [] = return $ Right []
      rec (filledins:fs) =
        do recursed <- rec fs
           case recursed of
             Left msg -> return $ Left msg
             Right others ->
               do result <- aux filledins
                  case result of
                    Left msg -> return $ Left msg
                    Right template -> return $ Right (template : others)
  in rec candidates

--  super ugly
fillInInstruction :: RuleName -> [Directory] -> [(Directory, RuleName)] -> InsDef.InstructionDefinition -> IO (Either String FilledIn)
fillInInstruction rname arguments multis instruction =
  let rec _ [] = return $ Right FilledIn { instructionName = InsDef.name instruction
                                         , rule = rname
                                         , itype = if null multis then SIMPLE else MULTI
                                         , asString = ""
                                         , inputs = []
                                         , outputs = []
                                         }
      rec arguments ((InsDef.Text text):ps) =
        do others <- rec arguments ps
           case others of
             Left msg -> return $ Left msg
             Right recursed ->
               return $ Right FilledIn { instructionName = instructionName recursed
                                       , rule = rule recursed
                                       , itype = itype recursed
                                       , asString = text ++ asString recursed
                                       , inputs = inputs recursed
                                       , outputs = outputs recursed
                                       }
      rec (arg:as) (param@(InsDef.Parameter InsDef.IN _):ps) =
        do current <- fillInInputParameter (arg, rname) param
           case current of
             Left msg -> return $ Left msg
             Right filledin ->
               do others <- rec as ps
                  case others of
                    Left msg -> return $ Left msg
                    Right recursed ->
                      return $ Right FilledIn { instructionName = instructionName recursed
                                              , rule = rule recursed
                                              , itype = itype recursed
                                              , asString = filledIn filledin ++ asString recursed
                                              , inputs = filledin : inputs recursed
                                              , outputs = outputs recursed
                                              }
      rec (arg:as) (param@(InsDef.Parameter InsDef.OUT _):ps) =
        let filledin = fillInOutputParameter (arg, rname) param
        in do others <- rec as ps
              case others of
                Left msg -> return $ Left msg
                Right recursed ->
                  return $ Right FilledIn { instructionName = instructionName recursed
                                          , rule = rule recursed
                                          , itype = itype recursed
                                          , asString = filledIn (head filledin) ++ asString recursed
                                          , inputs = inputs recursed
                                          , outputs = filledin ++ outputs recursed
                                          }
      rec arguments (param@(InsDef.Parameter InsDef.MULTI _):ps) =
        do current <- fillInMultiParameter multis param
           case current of
             Left msg -> return $ Left msg
             Right filledin ->
               do others <- rec arguments ps
                  case others of
                    Left msg -> return $ Left msg
                    Right recursed ->
                      return $ Right FilledIn { instructionName = instructionName recursed
                                              , rule = rule recursed
                                              , itype = itype recursed
                                              , asString = unwords (map filledIn filledin) ++ asString recursed
                                              , inputs = filledin ++ inputs recursed
                                              , outputs = outputs recursed
                                              }
  in rec arguments $ InsDef.parts instruction
