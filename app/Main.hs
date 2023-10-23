{-# LANGUAGE PatternSynonyms, TemplateHaskell, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
import Data.Bifunctor.TH
import Data.Bifunctor
import Control.Monad (ap)

import NaiveTerm
import FreeScopedTerm

-- (\x. x) z
main :: IO ()
main = do
  let t = (AppP (LamP (VarP Nothing)) (VarP (Just "z")))
  print t
  print (evalF t)


