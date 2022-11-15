{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( Idx (..),
    Lvl (..),
    Term (..),
    Value (..),
    Neutral (..),
    vvar,
    eval,
    evalWithEnv,
    quote,
    normalize,
  )
where

newtype Idx = Idx {unIdx :: Int} deriving (Show, Eq, Num)

newtype Lvl = Lvl {unLvl :: Int} deriving (Show, Eq, Num)

data Term
  = TVar Idx
  | TLam Term
  | TAppl Term Term
  deriving (Show, Eq)

data Value
  = VClosure Env Term
  | VNeutral Neutral
  deriving (Show, Eq)

data Neutral
  = NVar Lvl
  | NAppl Neutral Value
  deriving (Show, Eq)

vvar :: Lvl -> Value
vvar lvl = VNeutral $ NVar lvl

type Env = [Value]

evalWithEnv :: Env -> Term -> Value
evalWithEnv env = \case
  TVar (Idx x) -> env !! x
  TLam body -> VClosure env body
  TAppl m n ->
    let rator = evalWithEnv env m
        rand = evalWithEnv env n
     in case rator of
          VClosure env' body -> evalWithEnv (rand : env') body
          VNeutral neutral -> VNeutral $ NAppl neutral rand

eval :: Term -> Value
eval = evalWithEnv []

quoteWithLvl :: Lvl -> Value -> Term
quoteWithLvl lvl = \case
  VClosure env body ->
    let evaluatedBody = evalWithEnv (vvar lvl : env) body
     in TLam $ quoteWithLvl (lvl + 1) evaluatedBody
  VNeutral (NVar (Lvl x)) -> TVar $ Idx $ unLvl lvl - x - 1
  VNeutral (NAppl m n) ->
    let rator = quoteWithLvl lvl (VNeutral m)
        rand = quoteWithLvl lvl n
     in TAppl rator rand

quote :: Value -> Term
quote = quoteWithLvl 0

normalize :: Term -> Term
normalize = quote . eval
