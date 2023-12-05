{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module FreeScoped.Foil where

import Data.Bifunctor
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as Set
import Unsafe.Coerce
import System.Exit (exitFailure)
import qualified Util.Syntax.Lambda as LC
import qualified Util.IdInt as IdInt
import qualified Util.Impl as LambdaImpl
import Control.DeepSeq
import GHC.Generics (Generic)

type Id = Int
type RawName = Id
type RawScope = Set.IntSet

data {- kind -} S
  = {- type -} VoidS
  -- | {- type -} Singleton
  -- | {- type -} List

newtype Scope (n :: S) = UnsafeScope RawScope
  deriving newtype NFData
newtype Name (n :: S) = UnsafeName RawName
  deriving newtype NFData
newtype NameBinder (n :: S) (l :: S) =
  UnsafeNameBinder (Name l)
    deriving newtype NFData

emptyScope :: Scope VoidS
emptyScope = UnsafeScope Set.empty

extendScope :: NameBinder n l -> Scope n -> Scope l
extendScope (UnsafeNameBinder (UnsafeName id)) (UnsafeScope scope) =
  UnsafeScope (Set.insert id scope)

rawFreshName :: RawScope -> RawName
rawFreshName scope | Set.null scope = 0
                   | otherwise = Set.findMax scope + 1

withFreshBinder
  :: Scope n
  -> (forall l. NameBinder n l -> r)
  -> r
withFreshBinder (UnsafeScope scope) cont =
  cont binder
  where
    binder = UnsafeNameBinder (UnsafeName (rawFreshName scope))

nameOf :: NameBinder n l -> Name l
nameOf (UnsafeNameBinder name) = name

rawMember :: RawName -> RawScope -> Bool
rawMember i s = Set.member i s

member :: Name l -> Scope n -> Bool
member (UnsafeName name) (UnsafeScope s) = rawMember name s

data Expr n where
  VarE :: Name n -> Expr n
  AppE :: Expr n -> Expr n -> Expr n
  LamE :: NameBinder n l -> Expr l -> Expr n

-- Distinct constraints
class ExtEndo (n :: S)

class (ExtEndo n => ExtEndo l ) => Ext (n:: S) (l :: S)
instance ( ExtEndo n => ExtEndo l ) => Ext n l

class Distinct (n :: S)
instance Distinct VoidS

type DExt n l = (Distinct l, Ext n l)

-- Safer scopes with distinct constraints
data DistinctEvidence ( n :: S) where
  Distinct :: Distinct n => DistinctEvidence n

unsafeDistinct :: DistinctEvidence n
unsafeDistinct = unsafeCoerce (Distinct :: DistinctEvidence VoidS)

data ExtEvidence ( n:: S) ( l :: S) where
  Ext :: Ext n l => ExtEvidence n l

unsafeExt :: ExtEvidence n l
unsafeExt = unsafeCoerce (Ext :: ExtEvidence VoidS VoidS)

withFresh :: Distinct n => Scope n
  -> (forall l . DExt n l => NameBinder n l -> r ) -> r
withFresh scope cont = withFreshBinder scope (\binder ->
  unsafeAssertFresh binder cont)

unsafeAssertFresh :: forall n l n' l' r. NameBinder n l
  -> (DExt n' l' => NameBinder n' l' -> r) -> r
unsafeAssertFresh binder cont =
  case unsafeDistinct @l' of
    Distinct -> case unsafeExt @n' @l' of
      Ext -> cont (unsafeCoerce binder)

withRefreshed :: Distinct o => Scope o -> Name i
  -> (forall o'. DExt o o' => NameBinder o o' -> r) -> r
withRefreshed scope@(UnsafeScope rawScope) name@(UnsafeName id) cont
  | Set.member id rawScope = withFresh scope cont
  | otherwise = unsafeAssertFresh (UnsafeNameBinder name) cont

-- generic sinking
concreteSink :: DExt n l => Expr n -> Expr l
concreteSink = unsafeCoerce

class Sinkable (e :: S -> *) where
  sinkabilityProof :: (Name n -> Name l) -> e n -> e l

instance Sinkable Name where
  sinkabilityProof rename = rename

sink :: (Sinkable e, DExt n l) => e n -> e l
sink = unsafeCoerce

instance Sinkable Expr where
  sinkabilityProof rename (VarE v) = VarE (rename v)
  sinkabilityProof rename (AppE f e) = AppE (sinkabilityProof rename f) (sinkabilityProof rename e)
  sinkabilityProof rename (LamE binder body) = extendRenaming rename binder \rename' binder' ->
    LamE binder' (sinkabilityProof rename' body)

extendRenaming :: (Name n -> Name n') -> NameBinder n l
  -> (forall l'. (Name l -> Name l') -> NameBinder n' l' -> r ) -> r
extendRenaming _ (UnsafeNameBinder name) cont =
  cont unsafeCoerce (UnsafeNameBinder name)

-- Substitution
data Substitution (e :: S -> *) (i :: S) (o :: S) =
  UnsafeSubstitution (forall n. Name n -> e n) (Map Int (e o))

lookupSubst :: Substitution e i o -> Name i -> e o
lookupSubst (UnsafeSubstitution f env) (UnsafeName id) =
    case Map.lookup id env of
        Just ex -> ex
        Nothing -> f (UnsafeName id)

identitySubst :: (forall n. Name n -> e n) -> Substitution e i i
identitySubst f = UnsafeSubstitution f Map.empty

addSubst :: Substitution e i o -> NameBinder i i' -> e o -> Substitution e i' o
addSubst (UnsafeSubstitution f env) (UnsafeNameBinder (UnsafeName id)) ex = UnsafeSubstitution f (Map.insert id ex env)

addRename :: Substitution e i o -> NameBinder i i' -> Name o -> Substitution e i' o
addRename s@(UnsafeSubstitution f env) b@(UnsafeNameBinder (UnsafeName name1)) n@(UnsafeName name2)
    | name1 == name2 = UnsafeSubstitution f env
    | otherwise = addSubst s b (f n)

instance (Sinkable e) => Sinkable (Substitution e i) where
  sinkabilityProof rename (UnsafeSubstitution f env) =
    UnsafeSubstitution f (fmap (sinkabilityProof rename) env)

substitute :: Distinct o => Scope o -> Substitution Expr i o -> Expr i -> Expr o
substitute scope subst = \case
    VarE name -> lookupSubst subst name
    AppE f x -> AppE (substitute scope subst f) (substitute scope subst x)
    LamE binder body -> withRefreshed scope (nameOf binder) (\binder' ->
        let subst' = addRename (sink subst) binder (nameOf binder')
            scope' = extendScope binder' scope
            body' = substitute scope' subst' body in LamE binder' body'
        )

toFoilTerm :: (Distinct n) => Term -> Scope n -> Map String (Name n) -> Expr n
toFoilTerm (App a b) scope env = AppE (toFoilTerm a scope env) (toFoilTerm b scope env)
toFoilTerm (Lam (Ident str) body) scope env = withFresh scope str (\s ->
  let scope' = extendScope s scope
      env' = Map.insert str (nameOf s) (fmap sink env) in LamE s (toFoilTerm body scope' env'))
toFoilTerm (Var (Ident str)) scope env = case Map.lookup str env of
  Just name -> VarE name
  Nothing -> error ("Unbound variable " ++ str)
-- #TODO: suppori Pi expressions
toFoilTerm (Pi _ _ _) scope env = error ("Not yet implemented")

whnf :: (Distinct n) => Scope n -> Expr n -> Expr n
whnf scope = \case
  AppE f arg ->
    case whnf scope f of
      LamE z body ->
        let subst = UnsafeSubstitution VarE (Map.insert (idOf (nameOf z)) arg Map.empty)
        in substitute scope subst body
      f' -> AppE f' arg
  t -> t
