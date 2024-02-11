{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ide.Plugin.CompleteCase 
  ( descriptor 
  )
  where 

import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Except           (ExceptT(..), mapExceptT, throwE,runExceptT)

import           Development.IDE                      (Action,
                                                       IdeState (shakeExtras),
                                                       Range (Range), Recorder,
                                                       WithPriority (WithPriority),
                                                       cmapWithPrio)

import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Ide.Logger                           (Pretty (..),
                                                       Priority (Debug, Error, Info, Warning),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio, logWith,
                                                       nest,
                                                       toCologActionWithPrio,
                                                       vcat, viaShow, (<+>))
import           Ide.Plugin.Error
import           Ide.PluginUtils                      (positionInRange)
import           Ide.Types                            (PluginDescriptor (..),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       PluginCommand(..),
                                                       CommandFunction(..), 
                                                       ResolveFunction, 
                                                       mkResolveHandler,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler, defaultPluginPriority)
import           Language.LSP.Protocol.Message        (Method (..),
                                                       SMethod (..))
import           Language.LSP.Protocol.Types          (NormalizedFilePath, Null (Null),
                                                       Position (..),
                                                       CodeAction(..),
                                                       CompletionParams (..),
                                                       CodeActionContext (..), 
                                                       Diagnostic(..),
                                                       Range(..),
                                                       CodeActionKind(..),
                                                       TextEdit(..),
                                                       Position(..), 
                                                       CodeActionParams (..),
                                                       WorkspaceEdit(..), 
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       Uri, type (|?) (InL, InR), CompletionList, CompletionItem (CompletionItem))
import           Prelude                              hiding (log, span)

import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic(..))

import Data.ByteString qualified as BS 

import qualified Development.IDE.Core.Shake         as Shake
import Development.IDE.Graph.Internal.Types
import Development.IDE.Graph.Internal.Rules
import Development.IDE.Core.Tracing
import Development.IDE.Types.Shake (encodeShakeValue, ShakeValue (ShakeNoCutoff))
import Development.IDE.Types.Shake (A(..))
import Development.IDE.Types.Shake (Value(..))

import           Language.LSP.Server                  (ProgressCancellable (Cancellable),
                                                       sendNotification,
                                                       sendRequest,
                                                       withIndefiniteProgress)

import Data.Text qualified as T 
import Data.Map.Strict qualified as Map 


-- data Log 
--     = LogShake Shake.Log
--     | LogNoAST
--     | LogRequest Range 
--       deriving stock Show

-- instance Pretty Log where
--     pretty log = case log of
--         LogShake shakeLog -> pretty shakeLog
--         LogNoAST          -> "no HieAst exist for file"
--         LogRequest range -> pretty $ show range
type CompleteCaseLog = String 


descriptor :: Recorder (WithPriority CompleteCaseLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides **case** completions")
    { pluginHandlers = 
          mkPluginHandler SMethod_TextDocumentCompletion (requestCompletionHandler   recorder)
      <>  mkPluginHandler SMethod_TextDocumentCodeAction (requestResolveMissingCases recorder)
      <>  mkResolveHandler SMethod_CompletionItemResolve (requestCompletionsResolve  recorder)
    , pluginPriority = defaultPluginPriority
    }

-- textCompletionCommand :: PluginId -> PluginCommand IdeState 
-- textCompletionCommand plId = PluginCommand "completeCase" "addCompletion" (runCompletionCommand plId)

-- runCompletionCommand :: PluginId -> CommandFunction IdeState CompletionParams 
-- runCompletionCommand plId st mtoken CompletionParams {..} = 
--   let cmd = do 
--         throwE (PluginInternalError "woops")

--         -- pure $ InR (InR Null)


--   in ExceptT $
--             withIndefiniteProgress "Evaluating" mtoken Cancellable $ \_updater ->
--                 runExceptT $ cmd
  

requestCompletionsResolve :: Recorder (WithPriority CompleteCaseLog) -> ResolveFunction IdeState CompletionItem 'Method_CompletionItemResolve
requestCompletionsResolve recorder ide _ q@CompletionItem {..} file _ = 
  do 
    logWith recorder Info $ "RESOLVE!!!!!:"
    logWith recorder Info $ (show q) 

    pure q 


requestResolveMissingCases :: Recorder (WithPriority CompleteCaseLog) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction  
requestResolveMissingCases recorder ide _ q@(CodeActionParams{..}) = 
  do 
    let has_typecheck_hole = not $ null $ filter (\diag -> T.isInfixOf "Found hole: " $ diag._message ) type_check_diags
        has_missing_patterns = filter (\diag -> T.isInfixOf "Pattern match" diag._message) q._context._diagnostics
    logWith recorder Info $ "requestResolveMissingCases:"
    -- logWith recorder Info $ (show q) 
    logWith recorder Info $ "has_hole: " <> show has_typecheck_hole 
    logWith recorder Info $ "has_missing_pats: " <> (show $ not $ null $ has_missing_patterns)
    -- TODO: 
    let indentation = 2 

    case has_missing_patterns of 
      [missing_diag] -> do 
        let is_lambda_case = T.isInfixOf "\\case" missing_diag._message && not (T.isInfixOf "\\cases" missing_diag._message)
            is_lambda_cases = T.isInfixOf "\\case" missing_diag._message && not (T.isInfixOf "\\cases" missing_diag._message)
            result_start_line = missing_diag._range._end._line
            result_start_col = missing_diag._range._end._character + if is_lambda_case then 5 else if is_lambda_cases then 6 else 1 
            result_whitespace = missing_diag._range._start._character + indentation 
            -- "Pattern match(es) are non-exhaustive\nIn a \\case alternative:\n    Patterns of type \8216Maybe Config\8217 not matched:\n        Nothing\n        Just _"
            msg_lines = T.strip <$> (drop 3 $ T.lines missing_diag._message)

        pure 
          $ InL [ InR 
              CodeAction { 
                  _title = "add missing matches "
                , _kind = Just $ CodeActionKind_RefactorRewrite
                , _diagnostics = Just [missing_diag]
                , _disabled = Nothing 
                , _isPreferred = Just True 
                , _edit = Just 
                    WorkspaceEdit { 
                        _changes = Just $ Map.fromList [(uri, [
                            TextEdit { 
                                _range = 
                                  Range { 
                                      _start = Position { _line = result_start_line, _character = result_start_col }
                                    , _end = Position { _line = result_start_line, _character = result_start_col}
                                  }
                                , _newText = "\n" <> (T.unlines $ (\x -> T.replicate (fromIntegral result_whitespace) " " <> x <> " -> _ ") <$> msg_lines) }
                        ] ) ]
                      , _documentChanges = Nothing
                      , _changeAnnotations = Nothing 
                    } 
                , _command = Nothing 
                , _data_ = Nothing }
            ]
      _ -> pure $ InR Null 

    
  where 
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    type_check_diags = 
      filter (\diag -> diag._source == Just "typecheck") (q._context._diagnostics)



requestCompletionHandler :: Recorder (WithPriority CompleteCaseLog) -> PluginMethodHandler IdeState 'Method_TextDocumentCompletion
requestCompletionHandler recorder ide _ q@CompletionParams {..} = do
   do
        logWith recorder Info $ "requestCompletionHandler:"
        logWith recorder Info $ (show q) 

        let fp :: NormalizedFilePath = undefined -- <- getNormalizedFilePathE uri 
        
        mapExceptT liftIO $ runCompletions ide fp pos

  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    pos = _position 

runCompletions :: IdeState -> NormalizedFilePath -> Position -> ExceptT PluginError IO ([CompletionItem] |? (CompletionList |? Null))
runCompletions ide file positions = 
  pure $ InR (InR Null)



-- data CaseCompletions = CaseCompletions
--     deriving (Eq, Show, Typeable, Generic)
-- instance Hashable CaseCompletions
-- instance NFData   CaseCompletions


-- -- addRule 
-- --   :: forall key value. (RuleResult key ~ value, Typeable key, Hashable key, Eq key,Typeable value) 
-- --   => (key -> Maybe BS.ByteString -> RunMode -> Action (RunResult value)) 
-- --   -> Rules ()

-- produceCompletions :: Recorder (WithPriority CompleteCaseLog) -> Rules ()
-- produceCompletions recorder = do
--     define recorder (\k file -> pure Nothing)
--       -- (\CaseCompletions file -> 
--       --   do 
--       --     logWith recorder Info $ "Trying to find completions on " <> show file 
--       --     pure (RunResult ChangedStore (encodeShakeValue ShakeNoCutoff) $ A (Failed False)  ) :: Action (Shake.IdeResult CachedCompletions)) 
--   where 
--     define :: Shake.IdeRule k v => Recorder (WithPriority CompleteCaseLog) -> (k -> NormalizedFilePath -> Action (Shake.IdeResult v)) -> Rules () 
--     define recorder op = defineEarlyCutOff recorder $ Shake.Rule $ \k v -> (Nothing, ) <$> op k v 

--     defineEarlyCutOff :: Shake.IdeRule k v => Recorder (WithPriority CompleteCaseLog) -> Shake.RuleBody k v -> Rules () 
--     defineEarlyCutOff recorder (Shake.Rule op) = 
--       addRule $ \(Shake.Q (key, file)) (old :: Maybe BS.ByteString) mode -> do
--           extras <- Shake.getShakeExtras 
--           defineEarlyCutoff' key file mbOld mode action 
    
--     defineEarlyCutoff' ::Shake.IdeRule k v => NormalizedFilePath -> Maybe BS.ByteString -> RunMode -> (Development.IDE.Types.Shake.Value v -> Action (Maybe BS.ByteString, Shake.IdeResult v)) -> Action (RunResult (A (RuleResult k)))
--     defineEarlyCutoff' = undefined 
