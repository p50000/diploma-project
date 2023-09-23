module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data Inc var = Free var | Bound

-- data Term var
--   = Var var
--   | App (Term var) (Term var)
--   | Lam (Term (Maybe var))

-- data MyMaybe = MyJust a | MyNothing

-- data Comp = More | Less | Equal

-- eval :: Term var -> Term var
-- eval (App (Lam body) arg) = eval (substitute arg body)
-- eval anything = anything

-- -- (\ x . \ y . x y z) y
-- --
-- -- App (Lam (Lam (App
-- --  (App (Var (Just Nothing))
-- --       (Var Nothing))
-- --       (Var (Just (Just "z"))))))
-- --      (Var "y") :: Term String

-- -- Nothing             == 0
-- -- Just Nothing        == 1
-- -- Just (Just Nothing) == 2

-- comp
--     :: a -> b -> Comp
-- comp (Just val) (Just val2) = comp val val2
-- comp v z = Equal
-- comp Nothing _ = Less
-- comp _ Nothing = More

-- substitute
--   :: Term var
--   -> Term (Maybe var)
--   -> Term var
-- substitute arg term = substitute_internal arg term Nothing

-- substitute_internal
--     :: Term var -> Term var' -> Maybe var -> Term var
-- substitute_internal arg (App l r) level = App (substitute_internal arg l level) (substitute_internal arg r level)
-- substitute_internal arg (Var val) level = case comp level var of
--     Equal -> arg
--     _ -> Var val
-- substitute_internal arg (Lam v) level = Lam (substitute_internal (increase_ids arg Nothing) v (Just level))

-- increase_ids
--     :: Term var -> Maybe var -> Term var
-- increase_ids  (App l r) level = App (increase_ids l level) (increase_ids r level)
-- increase_ids  (Var val) level = case comp val level of
--       More -> Var (Just val)
--       _ -> Var val
-- increase_ids  (Lam body) level = Lam (increase_ids body (Just level))

-- main :: IO ()
-- main = print (eval (App (Lam (Lam (App
--  (App (Var (Just Nothing))
--        (Var Nothing))
--       (Var (Just (Just "z"))))))
--      (Var "y")))
