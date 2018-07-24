module Checking ( isRuleDefined
                , check
                ) where

import qualified InstructionDefinition as InsDef
import qualified RuleDefinition as RuleDef
import qualified InstructionUse as InsUse
import qualified Parser

import Data.List (find, lookup)
import Data.Maybe (isJust, fromJust)

getDuplicate :: Eq b => (a -> b) -> [a] -> Maybe (a, a)
getDuplicate _ [] = Nothing
getDuplicate f (x:xs) = let y = f x
                            isSame = (== y) . f
                        in case find isSame xs of
                             Nothing -> getDuplicate f xs
                             Just z -> Just (x, z)

getInstructionDefinitionDuplicate :: Parser.Pikafile -> Maybe String
getInstructionDefinitionDuplicate = let f = InsDef.name in fmap (f . fst) . getDuplicate f . Parser.insdefs

getRuleDefinitionDuplicate :: Parser.Pikafile -> Maybe String
getRuleDefinitionDuplicate = let f = RuleDef.name in fmap (f . fst) . getDuplicate f . map fst . Parser.rules

--  this might be changed if needed
getInstructionUseDuplicate :: Parser.Pikafile -> Maybe (String, String)
getInstructionUseDuplicate = rec . Parser.rules
  where rec [] = Nothing
        rec ((rdef, uses):xs) =
          case getDuplicate InsUse.name uses of
            Nothing -> rec xs
            Just (name, _) -> Just (RuleDef.name rdef, InsUse.name name)

getInstructionDefinitionOutputDuplicate :: Parser.Pikafile -> Maybe String
getInstructionDefinitionOutputDuplicate = rec . Parser.insdefs
  where rec [] = Nothing
        rec (x:xs) =
          let outparts = let getOutParts [] = []
                             getOutParts ((InsDef.Parameter InsDef.OUT ts):ys) = ts ++ getOutParts ys
                             getOutParts (_:ys) = getOutParts ys
                         in getOutParts $ InsDef.parts x
          in case getDuplicate id outparts of
               Nothing -> rec xs
               Just _ -> Just $ InsDef.name x

alignOutsToArgs :: [InsDef.InstructionPart] -> [String] -> [(String, InsDef.Template)]
alignOutsToArgs _ [] = []
alignOutsToArgs (x:xs) args@(arg:ys) =
  case x of
    InsDef.Parameter InsDef.IN _ -> alignOutsToArgs xs ys
    InsDef.Parameter InsDef.OUT templates ->
      let zipped = zip (repeat arg) templates
      in zipped ++ alignOutsToArgs xs ys
    _ -> alignOutsToArgs xs args

getInstructionUsesOutputDuplicate :: Parser.Pikafile -> Maybe (String, String, String)
getInstructionUsesOutputDuplicate pikafile = rec $ Parser.rules pikafile
  where rec [] = Nothing
        rec ((rdef, uses):xs) =
          let getUsesOuts use = let getDefFrom name = let isSame def = InsDef.name def == name
                                                          Just found = find isSame $ Parser.insdefs pikafile
                                                      in found
                                    args = InsUse.arguments use
                                    parts = InsDef.parts $ getDefFrom (InsUse.name use)
                                in alignOutsToArgs parts args
              mzip use = zip (repeat $ InsUse.name use) (getUsesOuts use)
              combine = concat . map mzip
          in case getDuplicate snd $ combine uses of
               Nothing -> rec xs
               Just (use1, use2) -> let rule = RuleDef.name rdef
                                    in Just (rule, fst use1, fst use2)

getUnsatisfiedMultiDependency :: Parser.Pikafile -> Maybe (String, String)
getUnsatisfiedMultiDependency pikafile = rec insdefs
  where insdefs = Parser.insdefs pikafile
        all_outs = let getOuts [] = []
                       getOuts ((InsDef.Parameter InsDef.OUT templates):xs) = templates ++ getOuts xs
                       getOuts (_:xs) = getOuts xs
                       getOutsFrom insdef = let name = InsDef.name insdef
                                                parts = InsDef.parts insdef
                                            in zip (repeat name) $ getOuts parts
                   in concat $ map getOutsFrom insdefs
        rec [] = Nothing
        rec (x:xs) =
          let isMulti (InsDef.Parameter InsDef.MULTI _) = True
              isMulti _ = False
          in case filter isMulti $ InsDef.parts x of
               [] -> rec xs
               multis ->
                 let rec' [] = Nothing
                     rec' (y:ys) =
                       let inOuts template = any ((== template) . snd) all_outs
                       in find (not . inOuts) y
                     untemplate [] = ""
                     untemplate (InsDef.Joker:ys) = '*' : untemplate ys
                     untemplate ((InsDef.TextPart text):ys) = text ++ untemplate ys
                 in case rec' $ map InsDef.templates multis of
                      Nothing -> rec xs
                      Just template -> Just (InsDef.name x, untemplate template)

isRuleDefined :: Parser.Pikafile -> String -> Bool
isRuleDefined pikafile rule = rule `elem` (getRuleNames pikafile)
  where getRuleNames = map (RuleDef.name . fst) . Parser.rules

getUnsatisfiedDependency :: Parser.Pikafile -> Maybe (String, String)
getUnsatisfiedDependency pikafile =
  let rules = let aux rule = (RuleDef.name rule, RuleDef.dependencies rule)
              in map (aux . fst) $ Parser.rules pikafile
      rec [] = Nothing
      rec ((name, dependencies):rs) =
        let rec' [] = Nothing
            rec' (dep:ds)
              | isRuleDefined pikafile dep = rec' ds
              | otherwise = Just (name, dep)
        in case rec' dependencies of
             Nothing -> rec rs
             Just pair -> Just pair
  in rec rules

getUnsatisfiedInstructionUse :: Parser.Pikafile -> Maybe (String, String)
getUnsatisfiedInstructionUse pikafile =
  let idefs = map InsDef.name $ Parser.insdefs pikafile
      rdefs = let aux (x, y) = (RuleDef.name x, map InsUse.name y)
              in map aux $ Parser.rules pikafile
      rec [] = Nothing
      rec ((rule, instructions):rs) =
        let rec' [] = Nothing
            rec' (i:is) =
              case find (== i) idefs of
                Nothing -> Just (rule, i)
                Just _ -> rec' is
        in case rec' instructions of
             Nothing -> rec rs
             Just pair -> Just pair
  in rec rdefs

getSelfDependency :: Parser.Pikafile -> Maybe String
getSelfDependency =
  let rules = let aux x = (RuleDef.name x, RuleDef.dependencies x)
              in map (aux . fst) . Parser.rules
      hasSelfDependency (rule, deps) =
        case find (== rule) deps of
          Nothing -> False
          Just _ -> True
  in fmap fst . find hasSelfDependency . rules

getBadParamNumber :: Parser.Pikafile -> Maybe (String, Int, String, Int)
getBadParamNumber pikafile =
  let idefs = let getParamNumber [] = 0
                  getParamNumber ((InsDef.Text _):xs) = getParamNumber xs
                  getParamNumber ((InsDef.Parameter InsDef.MULTI _):xs) = getParamNumber xs
              --  from here on, either an IN- or OUT-parameter
                  getParamNumber (_:xs) = 1 + getParamNumber xs
                  aux x = (InsDef.name x, getParamNumber $ InsDef.parts x)
              in map aux $ Parser.insdefs pikafile
      rdefs = let processUse x = (InsUse.name x, length $ InsUse.arguments x)
                  aux (x, y) = (RuleDef.name x, map processUse y)
              in map aux $ Parser.rules pikafile
      rec [] = Nothing
      rec ((rule, instructions):rs) =
        let rec' [] = Nothing
            rec' ((iname, icount):is) =
              let icount' = fromJust $ lookup iname idefs
              in if icount == icount'
                 then rec' is
                 else Just (iname, icount', rule, icount)
        in case rec' instructions of
             Just triple -> Just triple
             _ -> rec rs
  in rec rdefs

check :: Parser.Pikafile -> Maybe String
check pikafile
  | isJust a = let Just x = a in Just $ "multiple definitions of instruction '" ++ x ++ "'"
  | isJust b = let Just x = b in Just $ "multiple definitions of rule '" ++ x ++ "'"
  | isJust c = Just $ let Just (x, y) = c
                      in "multiple usage of instruction '" ++ y ++ "' by rule '" ++ x ++ "'"
  | isJust d = Just $ let Just (x, y) = d
                      in "unsatisfied dependency '" ++ y ++ "' for rule '" ++ x ++ "'"
  | isJust e = Just $ let Just (x, y) = e
                      in "rule '" ++ x ++ "' uses undefined instruction '" ++ y ++ "'"
  | isJust f = let Just x = f in Just $ "rule '" ++ x ++ "' depends on itself"
  | isJust g = Just $ let Just (iname, icount, rname, rcount) = g
                      in "instruction '" ++ iname ++ "' requires " ++ show icount ++
                         " arguments, but in rule '" ++ rname ++ "', " ++
                         "only " ++ show rcount ++ " are provided"
  | isJust h = let Just x = h in Just $ "instruction '" ++ x ++ "' outputs the same file several times"
  | isJust i = Just $ let Just (x, y, z) = i
                      in "rule '" ++ x ++ "' has two instruction uses '" ++
                         y ++ "' and '" ++ z ++ "' that produce the same output"
  | isJust j = Just $ let Just (x, y) = j
                      in "instruction '" ++ x ++ "' has multi-parameter with template '" ++
                         y ++ "' but no instruction produces such output"
  | otherwise = Nothing
  where a = getInstructionDefinitionDuplicate pikafile
        b = getRuleDefinitionDuplicate pikafile
        c = getInstructionUseDuplicate pikafile
        d = getUnsatisfiedDependency pikafile
        e = getUnsatisfiedInstructionUse pikafile
        f = getSelfDependency pikafile
        g = getBadParamNumber pikafile
        h = getInstructionDefinitionOutputDuplicate pikafile
        i = getInstructionUsesOutputDuplicate pikafile
        j = getUnsatisfiedMultiDependency pikafile
