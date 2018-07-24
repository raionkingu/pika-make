import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Control.Monad (when)
import Data.List (find)

import qualified InstructionDefinition as InsDef
import qualified RuleProcessing as RP
import qualified InstructionFilling as IF
import Checking (isRuleDefined)
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

          let undefineds = zip todo $ map (not . isRuleDefined pikadata) todo
          case find snd undefineds of
            Just (name, _) -> die $ "undefined rule '" ++ name ++ "'"
            _ -> return ()

          let simpleUses = getAllSimpleInstructionUses todo pikadata
              multiUses = getAllMultiInstructionUses todo pikadata

          eitherErrorOrFilledIn <- fillInSimples simpleUses
          case eitherErrorOrFilledIn of
            Left msg -> die msg
            _ -> return ()
          let Right simpleFilledIns = eitherErrorOrFilledIn

          to_exec <- mapM shouldGetExecuted simpleFilledIns
          let simpleToExecs = let transform fi = let msg = unwords [IF.instructionName fi, IF.rule fi]
                                                     outs = map IF.filledIn $ IF.outputs fi
                                                 in (msg, IF.asString fi, outs)
                                  aux = map snd . filter fst . zip to_exec
                              in map transform $ aux simpleFilledIns

          success <- execWhileOK simpleToExecs
          when (not success) exitFailure

          let getOutputsForMulti = let getOutputFrom rule =
                                         case find ((== rule) . IF.rule) simpleFilledIns of
                                           Just fi -> IF.outputs fi
                                   in concat . map getOutputFrom . RP.getDependenciesFrom pikadata . fst
              getTemplatesOfMulti = let collect [] = []
                                        collect ((InsDef.Parameter InsDef.MULTI ts):xs) = ts ++ collect xs
                                        collect (_:xs) = collect xs
                                    in collect . InsDef.parts . fst . snd
              keepOnlyRelevants (templates, outputs) = let inTemplates = flip elem templates
                                                       in filter (inTemplates . IF.original) outputs
              multisWithDeps = let mzip m = zip (map getTemplatesOfMulti m) (map getOutputsForMulti m)
                                   genDep fi = (IF.directoryName fi, IF.ruleName fi)
                               --  I say map map,
                               --  in the dep,
                                   transform = map (map genDep . keepOnlyRelevants) . mzip
                               in zip multiUses $ transform multiUses
          eitherErrorOrFilledIn <- fillInMultis multisWithDeps
          case eitherErrorOrFilledIn of
            Left msg -> die msg
            _ -> return ()
          let Right multiFilledIns = eitherErrorOrFilledIn

          to_exec <- mapM shouldGetExecuted multiFilledIns
          let multiToExecs = let transform fi = let msg = unwords [IF.instructionName fi, IF.rule fi]
                                                    outs = map IF.filledIn $ IF.outputs fi
                                                in (msg, IF.asString fi, outs)
                                 aux = map snd . filter fst . zip to_exec
                             in map transform $ aux multiFilledIns

          success <- execWhileOK multiToExecs
          if success then exitSuccess else exitFailure
