module Solution where

import Data.Either
import Types

typeOf :: Term -> Either String Type

typeOf = typeOf' []
  where
    typeOf' ctx (Sym x) =
      case lookup x ctx of
        Just typeOfX  -> Right typeOfX
        Nothing -> Left $ "Undefined variable: " ++ x

    typeOf' ctx (Lam x symType term) =
      case typeOf' newContext term of
        Left exception -> Left exception
        Right returnType -> Right (Fun symType returnType)
      where
        newContext = (x, symType) : ctx

    typeOf' ctx (App term1 term2) = case typeOf' ctx term1 of
      Left exception -> Left exception
      Right (Fun inputTypeForTerm1 outputTypeForTerm1)
        | Right inputTypeForTerm1 == (typeOf' ctx term2) -> Right outputTypeForTerm1
        | otherwise -> Left "App: wrong argument type"
      Right _ -> Left "App: first term is not a function"


    typeOf' ctx (Natural n)
      | n >= 0 = Right Nat
      | otherwise = Left $ show n ++ " is not a natural number"

    typeOf' ctx (Add term1 term2)
      | isLeft typeOfTerm1 = typeOfTerm1
      | isLeft typeOfTerm2 = typeOfTerm2
      | typeOfTerm1 /= Right Nat = Left "Add: left operand is not a natural number"
      | typeOfTerm2 /= Right Nat = Left "Add: right operand is not a natural number"
      | otherwise = Right Nat
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2

    typeOf' ctx (Mult term1 term2)
      | isLeft typeOfTerm1 = typeOfTerm1
      | isLeft typeOfTerm2 = typeOfTerm2
      | typeOfTerm1 /= Right Nat = Left "Mult: left operand is not a natural number"
      | typeOfTerm2 /= Right Nat = Left "Mult: right operand is not a natural number"
      | otherwise = Right Nat
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2

    typeOf' ctx (Boolean _) = Right Bool

    typeOf' ctx (Not term)
      | isLeft typeOfTerm = typeOfTerm
      | typeOfTerm /= Right Bool = Left "Not: argument is not a boolean"
      | otherwise = Right Bool
      where
        typeOfTerm = typeOf' ctx term

    typeOf' ctx (And term1 term2)
      | isLeft typeOfTerm1 = typeOfTerm1
      | isLeft typeOfTerm2 = typeOfTerm2
      | typeOfTerm1 /= Right Bool = Left "And: left operand is not a boolean"
      | typeOfTerm2 /= Right Bool = Left "And: right operand is not a boolean"
      | otherwise = Right Bool
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2

    typeOf' ctx (Or term1 term2)
      | isLeft typeOfTerm1 = typeOfTerm1
      | isLeft typeOfTerm2 = typeOfTerm2
      | typeOfTerm1 /= Right Bool = Left "Or: left operand is not a boolean"
      | typeOfTerm2 /= Right Bool = Left "Or: right operand is not a boolean"
      | otherwise = Right Bool
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2

    typeOf' ctx (Iff predicat thenTerm elseTerm)
      | isLeft typeOfPredicat = typeOfPredicat
      | isLeft typeOfThenTerm = typeOfThenTerm
      | isLeft typeOfElseTerm = typeOfElseTerm
      | typeOfPredicat /= Right Bool = Left "Iff: predicat is not a boolean"
      | typeOfThenTerm /= typeOfElseTerm = Left "Iff: values from then clause and else clause have different types"
      | otherwise = typeOfThenTerm
      where
        typeOfPredicat = typeOf' ctx predicat
        typeOfThenTerm = typeOf' ctx thenTerm
        typeOfElseTerm = typeOf' ctx elseTerm

    typeOf' ctx (Pair term1 term2) =
      case typeOfTerm1 of
        Left exception -> Left exception
        Right typeOfFirst ->
          case typeOfTerm2 of
            Left exception -> Left exception
            Right typeOfSecond -> Right $ PairT typeOfFirst typeOfSecond
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2

    typeOf' ctx (Fst term) =
      case typeOf' ctx term of
        Left exception -> Left exception
        Right (PairT typeOfFirstTerm _) -> Right typeOfFirstTerm
        Right _ -> Left "Fst: argument is not a pair"

    typeOf' ctx (Snd term) =
      case typeOf' ctx term of
        Left exception -> Left exception
        Right (PairT _ typeOfSecondTerm) -> Right typeOfSecondTerm
        Right _ -> Left "Snd: argument is not a pair"

    typeOf' ctx (Nil typeOfNil) = Right $ List typeOfNil

    typeOf' ctx (IsNil term) =
      case typeOf' ctx term of
        Left exception -> Left exception
        Right (List _) -> Right Bool
        Right _ -> Left "IsNil: argument is not a list"

    typeOf' ctx (Cons term1 term2)
      | isLeft typeOfTerm1 = typeOfTerm1
      | isLeft typeOfTerm2 = typeOfTerm2
      | not (isList typeOfTerm2) = Left "Cons: right operand is not a list"
      | typeOfTerm1 /= Right typeOfListElements = Left "Cons: first argument type and type of list elements are different"
      | otherwise = Right $ List typeOfListElements
      where
        typeOfTerm1 = typeOf' ctx term1
        typeOfTerm2 = typeOf' ctx term2
        isList (Right (List _)) = True
        isList _ = False
        typeOfListElements = typeOfListElements' typeOfTerm2
          where
            typeOfListElements' (Right (List t)) = t

    typeOf' ctx (Head term) =
      case typeOfTerm of
        Left exception -> Left exception
        Right (List typeOfListElements) -> Right typeOfListElements
        Right _ -> Left "Head: argument is not a list"
      where
        typeOfTerm = typeOf' ctx term

    typeOf' ctx (Tail term) =
      case typeOfTerm of
        Left exception -> Left exception
        Right (List typeOfListElements) -> Right $ List typeOfListElements
        Right _ -> Left "Tail: argument is not a list"
      where
        typeOfTerm = typeOf' ctx term
