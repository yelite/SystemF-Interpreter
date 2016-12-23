{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

class Sub t v where
  subst :: t -> v -> t -> t

type TypeVar = Char

data Type = ForAll TypeVar Type
          | FunctionType Type Type
          | T TypeVar
          deriving (Eq)

instance Show Type where
  showsPrec d (ForAll tv t) = showParen (d > 4)
                              $ showString "\8704"
                              . showString ([tv] ++ ". ")
                              . showsPrec 4 t
  showsPrec d (FunctionType t0 t1) = showParen (d > 5)
                                     $ showsPrec 6 t0
                                     . showString " -> "
                                     . showsPrec 5 t1
  showsPrec d (T t) = showString [t]

instance Sub Type TypeVar where
  subst (ForAll v1 t1) v2 t2
    | (v1 == v2) = ForAll v1 t1
    | otherwise = ForAll v1 (subst t1 v2 t2)
  subst (FunctionType t0 t1) v t2 = FunctionType (subst t0 v t2) (subst t1 v t2)
  subst (T v1) v2 t2
    | (v1 == v2) = t2
    | otherwise = T v1

type Var = String

data Term = Func Var Type Term
          | App Term Term
          | TFunc TypeVar Term
          | TApp Term Type
          | V Var
          deriving (Eq)

instance Show Term where
  showsPrec d (Func v t term) = showParen (d > 3)
                                $ showString ("\955" ++ v ++ ":")
                                . showsPrec 5 t
                                . showString ". "
                                . showsPrec 3 term
  showsPrec d (App term1 term2) = showParen (d > 4)
                                  $ showsPrec 4 term1
                                  . showString " "
                                  . showsPrec 4 term2
  showsPrec d (TFunc tv term) = showParen (d > 3)
                                $ showString ("\923" ++ [tv] ++ ". ")
                                . showsPrec 3 term
  showsPrec d (TApp term t) = showParen (d > 4)
                              $ showsPrec 4 term
                              . showString " "
                              . showsPrec 6 t
  showsPrec d (V v) = showString v

  
instance Sub Term Var where
  subst (V v1) v2 term
    | (v1 == v2) = term
    | otherwise = V v1
  subst (TApp term1 t) v term2 = TApp (subst term1 v term2) t
  subst (TFunc tv term1) v term2 = TFunc tv (subst term1 v term2)
  subst (App term0 term1) v term2 = App (subst term0 v term2) (subst term1 v term2)
  subst (Func v1 t term1) v2 term2
    | (v1 == v2) = Func v1 t term1
    | otherwise = Func v1 t (subst term1 v2 term2)


substType :: Term -> TypeVar -> Type -> Term
substType (Func v t1 term) tv t2 = Func v (subst t1 tv t2) (substType term tv t2)
substType (App term0 term1) tv t2 = App (substType term0 tv t2) (substType term1 tv t2)
substType (TFunc tv1 term) tv2 t2
  | (tv1 == tv2) = TFunc tv1 term
  | otherwise = TFunc tv1 (substType term tv2 t2)
substType (TApp term t1) tv2 t2 = TApp (substType term tv2 t2) (subst t1 tv2 t2)
substType (V v) _ _ = V v


reduce :: Term -> Term
reduce (Func v t term) = Func v t (reduce term)
reduce (App term0 term1) = case (reduce term0) of
                             (Func v _ term) -> reduce (subst term v (reduce term1))
                             term -> App term (reduce term1)
reduce (TFunc v term) = TFunc v (reduce term)
reduce (TApp term t) = case (reduce term) of
                         (TFunc v term) -> reduce (substType term v t)
                         term -> TApp term t
reduce (V v) = (V v)


_typecheck :: Map Var Type -> Term -> Type
_typecheck m (Func v t term) = FunctionType t (_typecheck (Map.insert v t m) term)
_typecheck m (App term0 term1) =
  case (_typecheck m term0) of
    (FunctionType t1 t2) | t1 == (_typecheck m term1) -> t2
    otherwise -> error $ "type error in app \n"
                 ++ (show m) ++ "\n" ++ (show term0) ++ "\n" ++ (show term1)
_typecheck m (TFunc v term) = ForAll v (_typecheck m term)
_typecheck m (TApp term t) =
  case (_typecheck m term) of
    (ForAll v t1) -> subst t1 v t
    otherwise -> error $ "type error in tapp \n" ++ (show m) ++ "\n"
                 ++ (show term) ++ "\n" ++ (show t)
_typecheck m (V v) = case Map.lookup v m of
  Just t -> t
  Nothing -> error $ "free variable " ++ v

typecheck :: Term -> Type
typecheck = _typecheck Map.empty

identity :: Term
identity = TFunc 't' (Func "x" (T 't') (V "x"))
identityType = typecheck identity


_quote :: Map Var Type -> Term -> Term
_quote m (Func v t term) = Func v t (_quote (Map.insert v t m) term)
_quote m (App term0 term1) = App (App (TApp (V "id") t) q0) q1
  where t = _typecheck m term0
        q0 = _quote m term0
        q1 = _quote m term1
_quote m (TFunc v term) = TFunc v (_quote m term)
_quote m (TApp term t1) = TApp (App (TApp (V "id") t0) q) t1
  where t0 = typecheck term
        q = _quote m term
_quote m (V v) = (V v)

quote :: Term -> Term
quote term = Func "id" identityType (_quote Map.empty term)

unquote = TFunc 'a' (Func "q" rtype (App (V "q") identity))
  where rtype = (FunctionType identityType (T 'a'))

interpret :: Type -> Term -> Term
interpret t term = App (TApp unquote t) term

validate :: Term -> IO ()
validate term = do
  let original_type = typecheck term
      representation = quote term
      result = interpret original_type representation
      result_type = typecheck result
    in
    do putStrLn "The representation: "
       print representation
       putStrLn ""
       putStrLn "The original type: "
       print original_type
       putStrLn " The result type: "
       print result_type
       if (reduce term) == (reduce result)
         then putStrLn "The interpreted term is equivalent to the original one"
         else putStrLn "The interpreted term is different from the original one"

main :: IO ()
main = validate (App (TApp identity (typecheck unquote)) unquote)
