module InstructionUse ( InstructionUse(..)
                      , parse
                      ) where

import Data.Char (isSpace)
import System.FilePath.Posix (normalise)

import qualified Commons

data InstructionUse = InstructionUse { name :: String
                                     , arguments :: [String]
                                     } deriving (Show, Eq)

parse :: String -> Either String InstructionUse
--  logic error : upstream line classification MUST prevent this case
parse "" = error "cannot parse empty string : not a valid instruction use line"
parse ('\t':line) =
  case span Commons.isNameChar line of
    ("", _) -> Left "cannot use an instruction if no name provided"
    (nimi, "") -> Left $ "no argument provided to instruction '" ++ nimi ++ "'"
    (nimi, withSpacesInFront) ->
      let paths = let aux path
                        | last path == '/' = path
                        | otherwise = path ++ "/"
                  in map (aux . normalise) $ words withSpacesInFront
      in Right InstructionUse { name = nimi
                              , arguments = paths
                              }
--  logic error : upstream line classification MUST prevent this case
parse line = error $ "cannot parse fake instruction use line '\t" ++ line ++ "'"
