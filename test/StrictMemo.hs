{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
-- This module defers type errors to run time, so that it compiles whether or
-- not a strict monad has a MonadMemo instance.
module StrictMemo (strictMemoChecks) where

import Control.Exception (SomeException, TypeError(..), evaluate, fromException, try)
import Data.Maybe (isJust)
import LazyPPL.Distributions.Memoization (memoize)
import System.Timeout (timeout)

-- Memoization at Int builds an infinite trie inside the monad, which only a
-- lazy monad such as Prob can return. A strict monad such as Maybe would
-- never return the memo function, so it has no instance and the call below
-- does not type-check.
strictMemoChecks :: IO [Bool]
strictMemoChecks = do
  result <- try (timeout 200000 (evaluate (isJust (memoize (Just :: Int -> Maybe Int)))))
  let (ok, outcome) = case result of
        Left err | Just (TypeError _) <- fromException err -> (True, "rejected by the type checker")
                 | otherwise -> (False, "an exception: " ++ show (err :: SomeException))
        Right Nothing -> (False, "no result within 0.2 s")
        Right (Just _) -> (False, "a result")
  putStrLn $ (if ok then "PASS" else "FAIL") ++ " memoize at Maybe Int gives " ++ outcome
  pure [ok]
