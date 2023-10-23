module FreeScopedTerm where

----------------------- Freescoped part ------------------------
--- Redoing AST and TermNode

-- Term Functor (generating functor)
-- TermF
data TermNode scope term
  = AppNode term term
  | LamNode scope -- CORRECT!
  -- | Let term scope -- let x = e1 in e2
  deriving (Show)
$(deriveBifunctor ''TermNode)

-- Free Scoped monads
-- data FS
data AST node var
  = VarT var
  | NodeT (node (AST node (Maybe var)) (AST node var))

  -- VarT (Name n)
  -- NodeT (node (FoilScope (AST node) n) (AST node n))
  -- (Name n -> AST t n')

-- data FoilScope f n = FoilScope
--  { arg :: NameBinder n l
--  , scope :: f l
--  }

deriving instance (forall scope term. (Show scope, Show term) => Show (node scope term), Show var) => Show (AST node var)

type Term'' var = AST TermNode var

pattern VarP x = VarT x
pattern LamP s = NodeT (LamNode s)
pattern AppP t1 t2 = NodeT (AppNode t1 t2)

mapAST :: Bifunctor t => (a -> b) -> AST t a -> AST t b
mapAST f (VarT x) = VarT (f x)
mapAST f (NodeT t) = NodeT (bimap (mapAST (mapMaybe f)) (mapAST f) t)

instance Bifunctor t => Functor (AST t) where
  fmap = mapAST

expandMaybeF :: Bifunctor t =>  Maybe (AST t a) ->  AST t (Maybe a)
expandMaybeF Nothing = VarT Nothing
expandMaybeF (Just a) = mapAST Just a

expandScope :: Bifunctor t => (var -> AST t var')
  -> Maybe var
  -> AST t (Maybe var')
expandScope _f Nothing = expandMaybeF Nothing
expandScope f (Just c) = expandMaybeF (Just (f c))

-- Functor
-- fmap :: (a -> b) -> Maybe a -> Maybe b
-- fmap :: (a -> b) -> AST t a -> AST t b          -- rename variables

-- Applicative (Functor)
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
-- (<*>) :: AST t (a -> b) -> AST a -> AST b       -- ???

-- Monad
-- (=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
-- (=<<) :: (a -> AST t b) -> AST t a -> AST t b   -- substitute

expandVarsF
  :: Bifunctor t
  => (var -> AST t var')
  -> AST t var
  -> AST t var'
expandVarsF f (VarT x) = f x
expandVarsF f (NodeT t) = NodeT (bimap ((expandVarsF (expandScope f))) (expandVarsF f) t)

instance Bifunctor t => Applicative (AST t) where
  pure = return
  (<*>) = ap

instance Bifunctor t => Monad (AST t) where
  -- return :: a -> AST t a
  return = VarT
  (>>=) = flip expandVarsF

substituteF
  :: Bifunctor node =>
  AST node var -> AST node (Maybe var) -> AST node var
substituteF arg term = expandVarsF f term where
    f Nothing = arg
    f (Just y) = VarP y

evalF :: Term'' var -> Term'' var
evalF (AppP (LamP body) arg) = evalF ((substituteF arg body))
evalF (AppP func arg) = AppP func arg
evalF (LamP body) = LamP (evalF body)
evalF a = a