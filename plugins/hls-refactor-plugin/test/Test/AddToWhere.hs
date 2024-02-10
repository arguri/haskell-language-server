{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.AddToWhere (tests) where

import Development.IDE
import Test.Tasty
import qualified Data.Text as T
import qualified Development.IDE.Plugin.CodeAction        as Refactor
import Test.Hls

import           Data.List.Extra
import System.FilePath

tests = testGroup "add to where" [
        mkGoldenAddArgTest "InsertNewWhere" (R 0 0 0 50),
        mkGoldenAddArgTest "PrependWhereDecls" (R 0 0 0 50),
        mkGoldenAddArgTest "PrependWhereDeclsComplex" (R 0 0 0 50),
        mkGoldenAddArgTest "PrependWhereDeclsComplex" (R 6 0 6 50)
  ]

mkGoldenAddArgTest :: FilePath -> Range -> TestTree
mkGoldenAddArgTest testFileName range@(Range (Position sl sc) (Position el ec)) = do
    let action docB = do
          _ <- waitForDiagnostics
          actions <- getCodeActions docB range 
          case filter (\(InR CodeAction {_title = x}) -> "Add to" `isPrefixOf` T.unpack x) actions of 
            InR action@CodeAction {_title = actionTitle} : _ -> do 
              liftIO $ actionTitle @?= "Add to where ‘new_def’"
              executeCodeAction action
            _ -> liftIO $ assertFailure $ "could not find code action in " <> show actions 

          -- InR action@CodeAction {_title = actionTitle} : _ <-
          --   filter (\(InR CodeAction {_title = x}) -> "Add to" `isPrefixOf` T.unpack x) actions 
              
          
        rangeName = show sl <> "_" <> show sc <> "_" <> show el <> "_" <> show ec
    goldenWithHaskellDoc 
      def 
      (mkPluginTestDescriptor Refactor.bindingsPluginDescriptor "ghcide-code-actions-bindings") 
      (testFileName <> " " <> rangeName <> " (golden)") 
      "plugins/hls-refactor-plugin/test/data/golden/add_to_where" 
      testFileName 
      (rangeName <.> "expected") 
      "hs" 
      action 
       

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')
