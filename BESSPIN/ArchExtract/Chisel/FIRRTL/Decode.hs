{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms,
   StandaloneDeriving #-}
module BESSPIN.ArchExtract.Chisel.FIRRTL.Decode where

import Prelude hiding (span)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Generics
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word

import Debug.FilterTrace

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import BESSPIN.ArchExtract.Chisel.FIRRTL.AST


TraceAPI trace traceId traceShow traceShowId traceM traceShowM = mkTraceAPI "FIRRTL.Decode"


type DecodeM a = ExceptT Text (Writer [Text]) a


packShort :: Int -> String -> Text
packShort n s = T.pack pre <> if null suf then "" else " ..."
  where
    (pre, suf) = splitAt n s

bad :: Text -> CBOR.Term -> DecodeM a
bad what x = throwError $ "bad " <> what <> " at " <> packShort 500 (show x)

inContext :: Text -> DecodeM a -> DecodeM a
inContext desc act = censor (\errs -> map addDesc errs) $ withExceptT addDesc act
  where 
    addDesc err = err <> ", in " <> desc

-- Run `act`.  If it throws an exception, record the exception and return
-- `Nothing`.
reporting :: DecodeM a -> DecodeM (Maybe a)
reporting act = (Just <$> act) `catchError` \e -> tell [e] >> return Nothing



decode :: Get a => CBOR.Term -> Either Text a
decode x =
    let (r, errs) = runWriter $ runExceptT $ get x in
    foldr (trace . T.unpack) r errs

deserialize :: BSL.ByteString -> Either Text Circuit
deserialize bs = case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
    Left cborErr -> Left $ T.pack $ show cborErr
    Right (_, term) -> decode term


class Get a where
    get :: CBOR.Term -> DecodeM a

-- Default handling of lists is to fail if any element fails.  To discard
-- failed elements instead, see `tryList` below.
instance Get a => Get [a] where
    get (CBOR.TList xs) = mapM get xs
    get (CBOR.TListI xs) = mapM get xs
    get x = bad "list" x

instance Get Text where
    get (CBOR.TString s) = return s
    get (CBOR.TStringI s) = return $ TL.toStrict s
    get x = bad "string" x

instance Get Int where
    get (CBOR.TInt i) = return i
    get (CBOR.TInteger i) =
        if toInteger (minBound :: Int) <= i && i <= toInteger (maxBound :: Int) then
            return $ fromInteger i
        else
            throwError $ "integer out of range for Int at " <> packShort 500 (show i)
    get x = bad "integer" x

instance Get Integer where
    get (CBOR.TInt i) = return $ toInteger i
    get (CBOR.TInteger i) = return i
    get x = bad "integer" x

instance Get Bool where
    get (CBOR.TBool b) = return b
    get x = bad "bool" x

try :: (Get a, Monoid a) => CBOR.Term -> DecodeM a
try x = maybe mempty id <$> reporting (get x)

tryList :: Get a => CBOR.Term -> DecodeM [a]
tryList x = case x of
    CBOR.TList xs -> go xs
    CBOR.TListI xs -> go xs
    _ -> bad "list" x
  where
    go xs = catMaybes <$> mapM (reporting . get) xs


instance Get Circuit where
    get (fir "Circuit" -> [info, modules, main]) =
        Circuit <$> try info <*> tryList  modules <*> get main
    get x = bad "circuit" x

instance Get SourceInfo where
    get (fir0 "NoInfo$" -> True) = return $ SourceInfo []
    get (fir "FileInfo" -> [s]) = get s >>= \s -> return $ SourceInfo [s]
    get (fir "MultiInfo" -> [infos]) = mconcat <$> get infos
    get x = bad "source info" x

instance Get Module where
    get (fir "Module" -> [info, name, ports, body]) =
        Module <$> try info <*> get name <*> tryList ports <*> (MkNormal <$> get body)
    get (fir "ExtModule" -> [info, name, ports, defName]) =
        Module <$> try info <*> get name <*> tryList ports <*> (MkExtern <$> get defName)
    get x = bad "module" x

instance Get Port where
    get (fir "Port" -> [info, name, dir, ty]) =
        Port <$> try info <*> get name <*> get dir <*> get ty
    get x = bad "port" x

instance Get Direction where
    get (fir0 "Input$" -> True) = return Input
    get (fir0 "Output$" -> True) = return Output
    get x = bad "direction" x


instance Get Ty where
    get (fir "UIntType" -> [width]) = TUInt <$> get width
    get (fir "SIntType" -> [width]) = TSInt <$> get width
    get (fir "FixedType" -> [width, point]) = TFixed <$> get width <*> get point
    get (fir "BundleType" -> [fields]) = TBundle <$> tryList fields
    get (fir "VectorType" -> [ty, len]) = TVector <$> get ty <*> get len
    get (fir0 "ClockType$" -> True) = return TClock
    get (fir "AnalogType" -> [width]) = TAnalog <$> get width
    get (fir0 "UnknownType$" -> True) = return TUnknown
    get x = bad "type" x

instance Get Width where
    get (fir "IntWidth" -> [w]) = WInt <$> get w
    get (fir0 "UnknownWidth$" -> True) = return WUnknown
    get x = bad "width" x

instance Get Field where
    get (fir "Field" -> [name, flip, ty]) =
        Field <$> get name <*> get ty <*> getFlip flip
    get x = bad "field" x

getFlip :: CBOR.Term -> DecodeM Bool
getFlip (fir0 "Default$" -> True) = return False
getFlip (fir0 "Flip$" -> True) = return True
getFlip x = bad "flip" x

instance Get Stmt where
    get (fir "DefWire" -> [info, name, ty]) = SDef <$> try info <*>
        (DWire <$> get name <*> get ty)
    get (fir "DefRegister" -> [info, name, ty, clock, reset, init]) = SDef <$> try info <*>
        (DReg <$> get name <*> get ty <*> get clock <*> get reset <*> get init)
    get (fir "DefInstance" -> [info, name, mod]) = SDef <$> try info <*>
        (DInst <$> get name <*> get mod)
    get (fir "DefMemory" -> [info, name, ty, depth, rds, wrs, rdwrs]) = SDef <$> try info <*>
        (DMemory <$> get name <*> get ty <*> get depth <*> get rds <*> get wrs <*> get rdwrs)
    get (fir "DefNode" -> [info, name, val]) = SDef <$> try info <*>
        (DNode <$> get name <*> get val)
    get (fir "Conditionally" -> [info, cond, then_, else_]) =
        SCond <$> try info <*> get cond <*> get then_ <*> get else_
    get (fir "Block" -> [stmts]) =
        SBlock <$> get stmts
    get (fir "PartialConnect" -> [info, loc, expr]) =
        SPartialConnect <$> try info <*> get loc <*> get expr
    get (fir "Connect" -> [info, loc, expr]) =
        SConnect <$> try info <*> get loc <*> get expr
    get (fir "IsInvalid" -> [info, expr]) =
        SIsInvalid <$> try info <*> get expr
    get (fir "Attach" -> [info, exprs]) =
        SAttach <$> try info <*> get exprs
    get (fir "Stop" -> [info, ret, clk, en]) =
        SStop <$> try info <*> get ret <*> get clk <*> get en
    get (fir "Print" -> [info, str, args, clk, en]) =
        SPrint <$> try info <*> get str <*> get args <*> get clk <*> get en
    get (fir0 "EmptyStmt$" -> True) =
        return SEmpty
    get (tag "firrtl.CDefMemory" -> [info, name, ty, size, seq]) = SDef <$> try info <*>
        (DCMem <$> get name <*> get ty <*> get size <*> get seq)
    get (tag "firrtl.CDefMPort" -> [info, name, ty, memName, exprs, dir]) = SDef <$> try info <*>
        (DCMemPort <$> get name <*> get ty <*> get memName <*> get exprs <*> get dir)
    get x = bad "stmt" x

instance Get MemPortDir where
    get (tag0 "firrtl.MInfer$" -> True) = return MpdInfer
    get (tag0 "firrtl.MRead$" -> True) = return MpdRead
    get (tag0 "firrtl.MWrite$" -> True) = return MpdWrite
    get (tag0 "firrtl.MReadWrite$" -> True) = return MpdReadWrite
    get x = bad "mem port dir" x

instance Get Expr where
    get (fir "Literal" -> [lit]) =
        ELit <$> get lit
    get (fir "Reference" -> [name, ty]) =
        ERef <$> get name <*> get ty
    get (fir "SubField" -> [expr, name, ty]) =
        EField <$> get expr <*> get name <*> pure Nothing <*> get ty
    get (fir "SubIndex" -> [expr, idx, ty]) =
        EIndexC <$> get expr <*> get idx <*> get ty
    get (fir "SubAccess" -> [expr, idx, ty]) =
        EIndex <$> get expr <*> get idx <*> get ty
    get (fir "Mux" -> [cond, then_, else_, ty]) =
        EMux <$> get cond <*> get then_ <*> get else_ <*> get ty
    get (fir "ValidIf" -> [cond, then_, ty]) =
        EValidIf <$> get cond <*> get then_ <*> get ty
    get (fir "DoPrim" -> [op, args, consts, ty]) =
        EPrim <$> get op <*> get args <*> get consts <*> get ty
    get x = bad "expr" x

instance Get Lit where
    get (fir "UIntLiteral" -> [val, width]) =
        LUInt <$> get val <*> get width
    get (fir "SIntLiteral" -> [val, width]) =
        LSInt <$> get val <*> get width
    get (fir "FixedLiteral" -> [val, width, point]) =
        LFixed <$> get val <*> get width <*> get point
    get x = bad "lit" x


isExactStr s x = case x of
    CBOR.TString s' -> s == s'
    CBOR.TStringI s' -> TL.fromStrict s == s'
    _ -> False

tag s x = case x of
    CBOR.TList (tag : xs) | isExactStr s tag -> xs
    CBOR.TListI (tag : xs) | isExactStr s tag -> xs
    _ -> []

tag0 s x = case x of
    CBOR.TList [tag] | isExactStr s tag -> True
    CBOR.TListI [tag] | isExactStr s tag -> True
    _ -> False

fir s x = tag ("firrtl.ir." <> s) x
fir0 s x = tag0 ("firrtl.ir." <> s) x
