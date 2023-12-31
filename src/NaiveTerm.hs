module NaiveTerm where

data Term var
  = Var var
  | App (Term var) (Term var)
  | Lam (Term (Maybe var)) deriving Show

-- (\ x . \ y . x y z) y
--
-- App (Lam (Lam (App
--  (App (Var (Just Nothing))
--       (Var Nothing))
--       (Var (Just (Just "z"))))))
--      (Var "y") :: Term String

-- Nothing             == 0
-- Just Nothing        == 1
-- Just (Just Nothing) == 2

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

mapTerm :: (a -> b) -> Term a -> Term b
mapTerm f (Var x) = Var (f x)
mapTerm f (App a b) = App (mapTerm f a) (mapTerm f b)
mapTerm f (Lam body) = Lam (mapTerm (mapMaybe f) body)

expandMaybe :: Maybe (Term a) ->  Term (Maybe a)
expandMaybe Nothing = Var Nothing
expandMaybe (Just a) = mapTerm Just a

expandLam
    :: (var -> Term var')
    -> Maybe var
    -> Term (Maybe var')
expandLam _f Nothing = expandMaybe Nothing
expandLam f (Just c) = expandMaybe (Just (f c))

expandVars
  :: (var -> Term var')
  -> Term var
  -> Term var'
expandVars f (App a b) = App (expandVars f a) (expandVars f b)
expandVars f (Var x)  = f x
expandVars f (Lam body) = Lam (expandVars (expandLam f) body)

substitute
  :: Term var
  -> Term (Maybe var)
  -> Term var
substitute arg term = expandVars f term where
    f Nothing =  arg
    f (Just y) =  Var y

eval :: Term var -> Term var
eval (App (Lam body) arg) = eval ( (substitute arg body))
eval (App func arg) = App func arg
eval (Lam body) = Lam (eval body)
eval a = a

-- gives: Lam (App (App (Var (Just "y")) (Var Nothing)) (Var (Just "z")))

--Lam (App (Lam (Lam (App
-- (App (Var (Just Nothing))
--       (Var Nothing))
--      (Var (Just (Just (Just "z")))))))
--     (Var Nothing))
-- Lam (Lam (App (App (Var (Just Nothing)) (Var Nothing)) (Var (Just (Just "z")))))

-- task: Lam (App (App (Lam (Lam (App (App (Var (Just Nothing)) (Var Nothing)) (Var (Just (Just (Just  "z"))))))) (Var Nothing)) (Var (Just "p")))
-- answer: Lam (App (Lam (App (App (Var (Just Nothing)) (Var Nothing)) (Var (Just (Just "z"))))) (Var (Just "p")))
