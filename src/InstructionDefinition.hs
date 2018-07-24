module InstructionDefinition ( ParameterType(..)
                             , TemplatePart(..)
                             , Template
                             , InstructionPart(..)
                             , InstructionDefinition(..)
                             , parse
                             , isMultiInstruction
                             ) where

import Utils (isLeft, rtrim, map1, mapR)
import qualified Commons

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf)

data ParameterType = IN | OUT | MULTI deriving (Show, Eq)

data TemplatePart = TextPart String | Joker deriving (Show, Eq)
type Template = [TemplatePart]

data InstructionPart = Text String
                     | Parameter { paramType :: ParameterType
                                 , templates :: [Template]
                                 }
                     deriving (Show, Eq)

data InstructionDefinition = InstructionDefinition { name :: String
                                                   , parts :: [InstructionPart]
                                                   } deriving (Show, Eq)

--  text with some jokers in the middle
parseTemplate :: String -> Template
parseTemplate [] = []
parseTemplate ('*':xs) = Joker : parseTemplate xs
parseTemplate str = let (text, xs) = span (/= '*') str
                    in TextPart text : parseTemplate xs

parseTemplates :: String -> Either String [Template]
parseTemplates [] = Left "empty template list"
parseTemplates csv =
  let rec [] = Right []
      rec str
        | null text = Left errmsg
        | all isSpace text = Left errmsg
        | null xs = Right [parseTemplate text]
        | otherwise = case rec (tail xs) of
                        Left msg -> Left msg
                        Right ys -> Right $ parseTemplate text : ys
        where (text, xs) = map1 rtrim $ span (/= ',') str
              errmsg = "empty template (are the commas correct ?)"
  in rec csv

parameterHelper ptype (':':maybeWithSpacesInFront) =
  case dropWhile isSpace maybeWithSpacesInFront of
--  logic error : this function MUST be called by the one below
    "" -> error "non-closed parameter provided to parameterHelper"
    noSpacesInFront ->
  --  init as to get rid of the closing parenthesis
      case parseTemplates $ init noSpacesInFront of
        Left msg -> Left msg
        Right ts -> Right Parameter { paramType = ptype
                                    , templates = ts
                                    }
parameterHelper _ _ = Left "'in'/'ou' in a parameter must be followed by a colon"

parseParameter :: String -> Either String InstructionPart
parseParameter whole@('$':'(':xs) =
  case dropWhile isSpace xs of
    'i':'n':ys -> parameterHelper IN $ dropWhile isSpace ys
    'o':'u':'t':ys -> parameterHelper OUT $ dropWhile isSpace ys
    _ -> Left $ let end = " (in '" ++ whole ++ "')"
                in "a parameter must be either 'in' or 'out'" ++ end
parseParameter whole@('$':'[':xs) =
  case dropWhile isSpace xs of
    'i':'n':ys -> parameterHelper MULTI $ dropWhile isSpace ys
    'o':'u':'t':ys -> Left "a multi parameter can only be 'in' (not 'out')"
    _ -> Left $ let end = " (in '" ++ whole ++ "')"
                in "a parameter must be either 'in' or 'out'" ++ end
--  logic error : this function MUST be called only for parameters
parseParameter whole = error $ "non-parameter '" ++ whole ++ "' provided to parseParameter"

handleParameter :: Char -> Char -> String -> Either String [InstructionPart]
handleParameter opening closing input =
  case span (/= closing) input of
    (_, "") -> Left $ "unmatched '" ++ opening : "' parenthesis"
    ("", _) -> Left "empty parameter"
    (paramtext, (_:nonparam)) ->
      let str = '$' : [opening] ++ paramtext ++ [closing]
      in case parseParameter str of
           Left msg -> Left msg
           Right param -> if null nonparam
                          then Right [param]
                          else case separateIntoParts nonparam of
                                 Left msg -> Left msg
                                 Right others -> Right $ param : others

separateIntoParts :: String -> Either String [InstructionPart]
--  logic error : this function MUST be called by the one below
separateIntoParts "" = error "empty string provided to separateIntoParts"
separateIntoParts ('$':'(':after) = handleParameter '(' ')' after
separateIntoParts ('$':'[':after) = handleParameter '[' ']' after
separateIntoParts nonparam =
  let spanNotParam ('$':'(':xs) = ("", "$(" ++ xs)
      spanNotParam ('$':'[':xs) = ("", "$[" ++ xs)
      spanNotParam (x:xs) = map1 ((:) x) $ spanNotParam xs
      spanNotParam "" = ("", "")
  in case spanNotParam nonparam of
       (text, "") -> Right [Text text]
       (text, after) -> case separateIntoParts after of
                          Left msg -> Left msg
                          Right others -> Right $ Text text : others

parse :: String -> Either String InstructionDefinition
--  logic error : upstream line classification MUST prevent this case
parse "" = error "cannot parse empty string : not a valid instruction definition line"
parse line =
  case span Commons.isNameChar line of
    ("", _) -> Left "an instruction must have a name"
--  logic error : upstream line classification MUST prevent this case
    (_, "") -> error wtf
    (nimi, notname) ->
      case dropWhile isSpace notname of
        '=':withSpacesInFront ->
          case dropWhile isSpace withSpacesInFront of
            "" -> Left $ "instruction '" ++ nimi ++ "' is empty"
            rawinsdefs ->
              case separateIntoParts rawinsdefs of
                Left msg -> Left msg
                Right osat -> let isText (Text _) = True
                                  isText _ = False
                              in if all isText osat
                                 then Left $ "instruction '" ++ nimi ++ "' has no parameter"
                                 else Right InstructionDefinition { name = nimi
                                                                  , parts = osat
                                                                  }
    --  logic error : upstream line classification MUST prevent this case
        _ -> error wtf
  where wtf = "cannot parse fake instruction definition line  '" ++ line ++ "'"

isMultiInstruction :: InstructionDefinition -> Bool
isMultiInstruction = any isMulti . parts
  where isMulti (Parameter MULTI _) = True
        isMulti _ = False
