module SolutionSpec where

import Test.Hspec

import Types
import Solution

spec = do
  it "typeOf" $ do
    typeOf (Lam "x" Nat (Add (Sym "x") (Natural 5))) `shouldBe` Right (Fun Nat Nat)

    typeOf (Lam "x" Bool $ Sym "x") `shouldBe` Right (Fun Bool Bool)

    typeOf (Add (Natural 5) (Boolean False)) `shouldBe` Left "Add: right operand is not a natural number"

    typeOf (App (Lam "x" Nat $ Sym "x") (Natural 5)) `shouldBe` Right Nat

    typeOf (App (Lam "x" Nat $ Boolean False) (Natural 5)) `shouldBe` Right Bool

    typeOf (App (Lam "x" Bool $ Boolean False) (Natural 5)) `shouldBe` Left "App: wrong argument type"

    typeOf (Nil Nat) `shouldBe` Right (List Nat)

    typeOf (Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat) `shouldBe` Left "Cons: first argument type and type of list elements are different"

    typeOf (App (Lam "x" (List Nat) (Sym "x")) (Cons (Natural 5) (Nil Nat))) `shouldBe` Right (List Nat)

    typeOf (Head (App (Lam "x" (List Nat) (Sym "x")) (Cons (Natural 5) (Nil Nat)))) `shouldBe` Right Nat

    typeOf (Tail (App (Lam "x" (List Nat) (Sym "x")) (Cons (Natural 5) (Nil Nat)))) `shouldBe` Right (List Nat)

    typeOf (Pair (Natural 5) (Boolean True)) `shouldBe` Right (PairT Nat Bool)
