{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( Idx (..),
    Lvl (..),
    Term (..),
    Value (..),
    Neutral (..),
    vvar,
    vappl,
    eval,
    quote,
    normalize,
  )
where

newtype Idx = Idx {unIdx :: Int} deriving (Show, Eq, Num)

newtype Lvl = Lvl {unLvl :: Int} deriving (Show, Eq, Num)

data Term
  = TLam Term
  | TVar Idx
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

vappl :: Neutral -> Value -> Value
vappl m n = VNeutral $ NAppl m n

type Env = [Value]

eval :: Env -> Term -> Value
eval env = \case
  TLam m -> VClosure env m
  TVar (Idx i) -> env !! i
  TAppl m n -> case (eval env m, eval env n) of
    (VClosure env' m', n') -> eval (n' : env') m'
    (VNeutral m', n') -> vappl m' n'

quote :: Lvl -> Value -> Term
quote lvl = \case
  VClosure env m -> TLam $ normalizeAt lvl env m
  VNeutral (NVar (Lvl var)) -> TVar $ Idx $ unLvl lvl - var - 1
  VNeutral (NAppl m n) -> TAppl (quote lvl (VNeutral m)) (quote lvl n)

normalize :: Lvl -> Env -> Term -> Term
normalize lvl env = quote lvl . eval env

normalizeAt :: Lvl -> Env -> Term -> Term
normalizeAt lvl env = normalize (lvl + 1) (vvar lvl : env)
