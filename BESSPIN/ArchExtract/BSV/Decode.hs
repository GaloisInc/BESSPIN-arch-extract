{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms #-}
module BESSPIN.ArchExtract.BSV.Decode where

import Prelude hiding (span)
import Control.Monad
import Control.Monad.Identity
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word

import Debug.Trace

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import BESSPIN.ArchExtract.BSV.Raw


type DecodeM a = Either Text a

bad :: Text -> CBOR.Term -> Text -> a -> DecodeM a
bad what x why dfl =
    trace (T.unpack $ "bad " <> what <> ": " <> why <> ", at " <> T.pack (show x)) $
    Right dfl

bad' :: Text -> CBOR.Term -> a -> DecodeM a
bad' what x dfl =
    trace (T.unpack $ "bad " <> what <> " at " <> T.pack (show x)) $
    Right dfl


getPackage :: CBOR.Term -> DecodeM Package
getPackage (tag "Package" -> [i, _, List defns]) =
    Package
        <$> getId i
        <*> (S.fromList <$> mapM getDefnDef defns)
        <*> (S.fromList <$> mapM getDefnStruct defns)
getPackage x = bad' "Package" x $
    Package (badId "package") S.empty S.empty

getDefnStruct :: CBOR.Term -> DecodeM Struct
getDefnStruct (tag "Defn_Struct" -> [_sub, name, List tyParams, List fields]) =
    Struct
        <$> getId name
        <*> mapM getId tyParams
        <*> mapM getField fields
getDefnStruct x = bad' "Defn_Struct" x (Struct (badId "struct") [] [])

getField :: CBOR.Term -> DecodeM Field
getField (tag "Field" -> [name, ty]) = Field <$> getId name <*> getTy ty

getDefnDef :: CBOR.Term -> DecodeM Def
getDefnDef (tag "Defn_ValueSign" -> [def]) = getDef def
getDefnDef x = bad' "Defn_Def" x badDef

getDef :: CBOR.Term -> DecodeM Def
getDef (tag "Def" -> [i, List tyVars, ty, List clauses]) = do
    baseTy <- getTy ty
    tyVars' <- mapM getId tyVars
    let ty' = if null tyVars' then baseTy else TForall tyVars' baseTy
    Def <$> getId i <*> pure ty' <*> mapM getClause clauses
getDef x = bad' "Def" x badDef

badId what = Id ("<bad-" <> what <> ">") 0 0
badDef = Def (badId "def") (TUnknown CBOR.TNull) []

getClause :: CBOR.Term -> DecodeM Clause
getClause (tag "Clause" -> [List pats, body]) =
    Clause <$> mapM getPat pats <*> getExpr body

getDeflDef :: CBOR.Term -> DecodeM Def
getDeflDef (tag "Defl_ValueSign" -> [def]) = getDef def
getDeflDef x = bad' "Defl" x badDef

getExpr :: CBOR.Term -> DecodeM Expr
getExpr (tag "Expr_Var" -> [i]) = EVar <$> getId i
getExpr (tag "Expr_LetRec" -> [List defls, body]) =
    ELetRec <$> mapM getDeflDef defls <*> getExpr body
getExpr (tag "Expr_LetSeq" -> [List defls, body]) =
    foldr (\d e -> ELet <$> getDeflDef d <*> e) (getExpr body) defls
getExpr (tag "Expr_Apply" -> [f, List args]) =
    EApp <$> getExpr f <*> pure [] <*> mapM getExpr args
getExpr (tag "Expr_TyApply" -> [f, List tys]) =
    EApp <$> getExpr f <*> mapM getTy tys <*> pure []
getExpr (tag "Expr_Rules" -> [List rs]) = ERules <$> mapM getRawRule rs
getExpr (tag "Expr_Lit" -> [l]) = ELit <$> getLit l
getExpr (tag "Expr_LitT" -> [_ty, l]) = ELit <$> getLit l
getExpr (tag "Expr_SelectT" -> [p, f]) =
    EStatic <$> getId p <*> getId f
getExpr (tag "Expr_StructT" -> [ty, List fs]) =
    EStruct <$> getTy ty <*> mapM go fs
  where
    go (List [f, e]) = (,) <$> getId f <*> getExpr e
    go x = bad' "struct entry" x (Id "<bad-struct-entry>" 0 0, EUnknown CBOR.TNull)
--getExpr x = bad' "Expr" x $ EUnknown x
getExpr x = bad' "Expr" x $ EUnknown (maybe CBOR.TNull CBOR.TString $ getTag x)

getRawRule :: CBOR.Term -> DecodeM RawRule
getRawRule (tag "Rule" -> [nameExpr, List quals, body]) =
    RrRule <$> onMaybe getExpr nameExpr <*> mapM getGuard quals <*> getExpr body
getRawRule x = bad' "RawRule" x $ RrUnknown x

getGuard :: CBOR.Term -> DecodeM Guard
getGuard (tag "Qual_Gen" -> [t, p, e]) = GPat <$> getPat p <*> getTy t <*> getExpr e
getGuard (tag "Qual_Filter" -> [e]) = GCond <$> getExpr e

getLit :: CBOR.Term -> DecodeM Lit
getLit (tag "Lit_Str" -> [_pos, Str value]) = return $ LStr value
getLit (tag "Lit_Int" -> [_pos, _, _, Integer value]) = return $ LInt value
getLit x = bad' "Lit" x $ LInt 0

getPat :: CBOR.Term -> DecodeM Pat
getPat (tag "Pat_Var" -> [i]) = PVar <$> getId i
getPat x = bad' "Pat" x $ PUnknown x

getTy :: CBOR.Term -> DecodeM Ty
getTy (tag "Type_Var" -> [i, _]) = TVar <$> getId i
getTy (tag "Type_Con" -> [i, _sort]) = TCon <$> getId i
getTy (tag "Type_Num" -> [Int val]) = TNat <$> pure val
getTy (tag "Type_Ap" -> [t1, t2]) = TApp <$> getTy t1 <*> ((:[]) <$> getTy t2)
getTy x = bad' "Ty" x $ TUnknown x

getId :: CBOR.Term -> DecodeM Id
getId (tag "Id" -> [tag "Position" -> [_, Int line, Int col, _], Str name, _]) =
    Id <$> pure name <*> pure line <*> pure col
getId x = bad' "Id" x $ Id "<bad>" 0 0


maybeToSeq (Just x) = S.singleton x
maybeToSeq Nothing = S.empty

onMaybe _ CBOR.TNull = return Nothing
onMaybe f x = Just <$> f x


pattern Int a <- (asInt -> Just a)
pattern Integer a <- (asInteger -> Just a)
pattern Str a <- (asStr -> Just a)
pattern List xs <- (asList -> Just xs)

asInt x = case x of
    CBOR.TInt i -> Just i
    CBOR.TInteger i -> Just $ fromInteger i
    _ -> Nothing

asInteger x = case x of
    CBOR.TInt i -> Just $ toInteger i
    CBOR.TInteger i -> Just i
    _ -> Nothing

asStr x = case x of
    CBOR.TString s -> Just s
    CBOR.TStringI s -> Just $ TL.toStrict s
    _ -> Nothing

asList x = case x of
    CBOR.TList xs -> Just xs
    CBOR.TListI xs -> Just xs
    _ -> Nothing

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

getTag x = case x of
    CBOR.TList (tag : _) -> asStr tag
    CBOR.TListI (tag : _) -> asStr tag
    _ -> Nothing


deserialize :: BSL.ByteString -> Either Text Package
deserialize bs = case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
    Left cborErr -> Left $ T.pack $ show cborErr
    Right (_, term) -> getPackage term
