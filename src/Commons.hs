module Commons ( isNameChar
               ) where

import Data.Char (isAlphaNum)

isNameChar c = isAlphaNum c || c `elem` "-_+./"
