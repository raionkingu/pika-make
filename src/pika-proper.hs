import System.Environment (getArgs)
import System.Directory (doesFileExist, removeFile)

import Utils (map2)

import qualified InstructionDefinition as InsDef
import qualified InstructionUse as InsUse
import Routines

main = do args <- getArgs
          let todo = if null args then ["default-rule"] else args

          maybePikafile <- getFirstExistingAmong ["pikafile", "Pikafile"]
          case maybePikafile of
            Nothing -> die "no pikafile found in this directory"
            _ -> return ()

          raw_pikadata <- let Just pikafile = maybePikafile
                          in readFile pikafile
          let eitherErrorOrParsed = parseCheck raw_pikadata
          case eitherErrorOrParsed of
            Left msg -> die msg
            _ -> return ()
          let Right pikadata = eitherErrorOrParsed
              simpleUses = getAllSimpleInstructionUses todo pikadata
              multiUses = getAllMultiInstructionUses todo pikadata

      --  beginning of exclusive part
          let getTransformedOuts = let prepare (a, (b, c)) = (a, (InsDef.parts b, InsUse.arguments c))
                                       align (_, []) = []
                                       align (((InsDef.Text _):ids), ius) = align (ids, ius)
                                       align ((param:ids), ius@(i:is))
                                         | InsDef.paramType param == InsDef.MULTI = align (ids, ius)
                                         | otherwise = (param, i) : align (ids, is)
                                       isOut (InsDef.Parameter InsDef.OUT _) = True
                                       isOut _ = False
                                       getOutParams = map2 (filter (isOut . fst) . align)
                                       transform (_, []) = []
                                       transform (r, ((p, d):xs)) =
                                         let templates = InsDef.templates p
                                             aux [] = ""
                                             aux (InsDef.Joker:ys) = r ++ aux ys
                                             aux ((InsDef.TextPart text):ys) = text ++ aux ys
                                         in map ((++) d . aux) templates
                                   in concat . map (transform . getOutParams . prepare)
              allSimpleOuts = getTransformedOuts simpleUses
              allMultiOuts = getTransformedOuts multiUses

          simpleBools <- mapM doesFileExist allSimpleOuts
          multiBools <- mapM doesFileExist allMultiOuts
          let whichToRemove outs bools = map fst . filter snd $ zip outs bools
      --  the choice is made to let exceptions handle the failure of remove a file
          case (whichToRemove allSimpleOuts simpleBools, whichToRemove allMultiOuts multiBools) of
            ([], []) -> putStrLn "nothing to do"
            (simples, multis) ->
              let rm files
                    | null files = return ()
                    | otherwise = do putStrLn $ unwords ("rm" : files)
                                     mapM_ removeFile files
              in do rm simples
                    rm multis
