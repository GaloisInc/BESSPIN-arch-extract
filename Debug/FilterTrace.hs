{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Debug.FilterTrace
( mkTracer
, mkTraceAPI
, alwaysTrace
, TraceAPI(TraceAPI)
) where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import System.IO
import System.IO.Unsafe

import qualified Debug.Trace


mkTracer :: Text -> (forall a. String -> a -> a)
mkTracer modName =
    let {-# NOINLINE enabled #-}
        enabled = unsafePerformIO $ checkEnabled modName in
    \msg x -> if enabled then
        Debug.Trace.trace ("[" ++ T.unpack modName ++ "] " ++ msg) x
    else x

data TraceAPI = TraceAPI
    { trace :: forall a. String -> a -> a
    , traceId :: String -> String
    , traceShow :: forall a b. Show a => a -> b -> b
    , traceShowId :: forall a. Show a => a -> a
    , traceM :: forall f. Applicative f => String -> f ()
    , traceShowM :: forall a f. (Show a, Applicative f) => a -> f ()
    }

mkTraceAPI modName =
    let trace = mkTracer modName in
    TraceAPI
    { trace = trace
    , traceId = \a -> trace a a
    , traceShow = \a b -> trace (show a) b
    , traceShowId = \a -> trace (show a) a
    , traceM = \a -> trace a $ pure ()
    , traceShowM = \a -> trace (show a) $ pure ()
    }

checkEnabled :: Text -> IO Bool
checkEnabled modName = do
    filt <- maybe T.empty T.pack <$> lookupEnv "HS_TRACE"
    let parts = Set.fromList $ T.splitOn "," filt
    return $ modName `Set.member` parts || "all" `Set.member` parts

alwaysTrace = Debug.Trace.trace

