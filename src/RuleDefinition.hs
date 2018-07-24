module RuleDefinition ( RuleDefinition(..)
                      , parse
                      ) where

import Data.Char (isSpace)

import qualified Commons

data RuleDefinition = RuleDefinition { name :: String
                                     , dependencies :: [String]
                                     } deriving (Show, Eq)

parse :: String -> Either String RuleDefinition
--  logic error : upstream line classification MUST prevent this case
parse "" = error "cannot parse empty string : not a valid rule definition line"
parse line =
  case span Commons.isNameChar line of
    ("", _) -> Left "a rule must have a name"
--  logic error : upstream line classification MUST prevent this case
    (_, "") -> error wtf
    (nimi, notname) ->
      case dropWhile isSpace notname of
        ':':withSpacesInFront ->
          let names = words withSpacesInFront
              verifySyntax [] = Nothing
              verifySyntax (x:xs)
                | all Commons.isNameChar x = verifySyntax xs
                | otherwise = Nothing
          in case verifySyntax names of
               Just msg -> Left msg
               Nothing -> Right RuleDefinition { name = nimi
                                               , dependencies = names
                                               }
    --  logic error : upstream line classification MUST prevent this case
        _ -> error wtf
  where wtf = "cannot parse fake rule definition line '" ++ line ++ "'"
