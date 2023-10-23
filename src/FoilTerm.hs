{-# LANGUAGE DataKinds #-}

module FoilTerm where

import qualified Data.StringMap as SM

type Ident = String
type RawName = Ident
type RawScope = [Ident]

data {- kind -} S
  = {- type -} VoidS
  -- | {- type -} Singleton
  -- | {- type -} List

-- values:
-- 0, 1, 2        :: Int
-- True           :: Bool
-- (+1)           :: Int -> Int

-- types:
-- Int, Bool      :: Type
-- (Int -> Int)   :: Type
-- Maybe Int      :: Type
-- Maybe          :: Type -> Type
-- VoidS          :: S

-- kinds:
-- Type
-- Type -> Type
-- (Type -> Type) -> Type
-- S

data Scope (n :: S) = UnsafeScope RawScope
data Name (n :: S) = UnsafeName RawName
data NameBinder (n :: S) (l :: S) =
  UnsafeNameBinder (Name l)

ppName :: Name n -> String
ppName (UnsafeName name) = name

-- UnsafeName "z" :: Name ["x", "y", "z"]
-- UnsafeNameBinder (UnsafeName "z")
--   :: NameBinder ["x"] ["x", "y", "z"]

emptyScope :: Scope VoidS
emptyScope = UnsafeScope []

extendScope :: NameBinder n l -> Scope n -> Scope l
extendScope (UnsafeNameBinder (UnsafeName name)) (UnsafeScope scope) =
  UnsafeScope (name : scope)

rawFreshName :: RawScope -> RawName
rawFreshName scope = head {- DO NOT WRITE LIKE ME -}
  [ name
  | n <- [1..]
  , let name = "x" <> show n
  , name `notElem` scope
  ]

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

rawMember :: RawName −> RawScope −> Bool
rawMember (RawName i) (RawScope s) = elem i s

member :: Name l −> Scope n −> Bool
member (UnsafeName name) (UnsafeScope s) = rawMember name s

data Expr n where
  Var :: Name n -> Expr n
  App :: Expr n -> Expr n -> Expr n
  Lam :: NameBinder n l -> Expr l -> Expr n

-- λ s. s
identity :: Expr VoidS
identity = withFreshBinder emptyScope
  (\ s -> Lam s (Var (nameOf s)))

sink :: NameBinder n l -> Name n -> Name l
sink _ (UnsafeName x) = UnsafeName x

sinkGeneric :: e n −> e l
sinkGeneric = unsafeCoerce

-- λ s. λ z. s (s z)
two :: Expr VoidS
two = withFreshBinder emptyScope
  (\ s -> Lam s $ withFreshBinder (extendScope s emptyScope)
    (\ z -> Lam z (App (Var (sink z (nameOf s)))
                        (App (Var (sink z (nameOf s)))
                             (Var (nameOf z))))))

-- >>> putStrLn $ ppExpr identity
-- λx1. x1
-- >>> putStrLn $ ppExpr two
-- λx1. λx2. x1(x1(x2))
ppExpr :: Expr n -> String
ppExpr expr = case expr of
  Var name -> ppName name
  App e1 e2 -> ppExpr e1 ++ "(" ++ ppExpr e2 ++ ")"
  Lam x body -> "λ" ++ ppName (nameOf x) ++ ". " ++ ppExpr body

data Substitution (ex::S -> *) (in::S) (out::S) = UnsafeSubstitution (forall n. Name n -> ex n) (SM (e o))

lookupSusbst :: Substitution ex in out -> Name in -> ex out
lookupSusbst (UnsafeSubstitution f env) (UnsafeName (RawName name)) =
    case SM.lookup name env of
        Just ex -> ex
        Nothing -> f (UnsafeName (RawName name))

identitySubst :: (forall n. Name n -> e n) -> Substitution ex in in
identitySubst f = UnsafeSubstitution f IS.empty

addSubst :: Substitution ex in out -> NameBinder in in' -> ex out -> Substitution ex in' out
addSubst
(UnsafeSubstitution f env)
(UnsafeNameBinder (UnsafeName RawName name)) ex = UnsafeSubstitution f (IS.insert name ex env)

addRename :: Substitution ex in out -> NameBinder in in' -> Name out -> Substitution ex in' out
addRename s@(UnsafeSubstitution f env) b@(UnsafeBinder (UnsafeName name1)) n@(UnsafeName name2)
    | name1 == name2 = UnsafeSubstitution f env
    | otherwise = addSubst s b (f n)

withRefreshed:: Scope o -> Name l -> (forall o'. NameBinder o o' −> r ) −> r
withRefreshed scope name cont = if member name scope
    then withFreshBinder scope cont
    else cont (UnsafeBinder name)

substitute :: Scope out -> Substitution Expr in out -> Expr in -> Expr out
substitute scope subst = \case
    Var name -> lookupSusbst subst name
    App f x -> App (substitute scope subst f) (substitute scope subst x)
    Lam binder body -> withRefreshed scope (nameOf binder) (\binder` ->
        let subst' = addRename (sinkGeneric subst) binder (nameOf binder`)
            scope' = extendScope binder' scope
            body' = substituteExpr scope' subst' body' in Lam binder' body'
        )

-- data Maybe (a :: Type) = Nothing | Just a