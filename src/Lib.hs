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

eval :: Env -> Term -> Value
eval env = \case
  TVar (Idx x) -> env !! x
  TLam body -> VClosure env body
  TAppl m n ->
    let rator = eval env m
        rand = eval env n
     in case rator of
          VClosure env' body -> eval (rand : env') body
          VNeutral neutral -> VNeutral $ NAppl neutral rand

quote :: Lvl -> Value -> Term
quote lvl = \case
  VClosure env body ->
    let evaluatedBody = eval (vvar lvl : env) body
     in TLam $ quote (lvl + 1) evaluatedBody
  VNeutral (NVar (Lvl x)) -> TVar $ Idx $ unLvl lvl - x - 1
  VNeutral (NAppl m n) ->
    let rator = quote lvl (VNeutral m)
        rand = quote lvl n
     in TAppl rator rand

normalize :: Term -> Term
normalize = quote 0 . eval []
