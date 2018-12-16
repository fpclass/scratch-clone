--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad

import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Language
import Interpreter

--------------------------------------------------------------------------------

instance Arbitrary Op where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Expr where
    arbitrary = resize 10 $ sized arbExpr
        where
            arbExpr :: Int -> Gen Expr
            arbExpr 0 = do
                n <- arbitrary
                return (ValE n)
            arbExpr n = do
                m <- elements [0..n-1]
                op <- arbitrary `suchThat` (`notElem` [Div,Pow])
                l <- arbExpr (n-1-m)
                r <- arbExpr m
                return $ BinOpE op l r

--------------------------------------------------------------------------------

isSuccessful :: Either a b -> Bool
isSuccessful (Right _) = True
isSuccessful _         = False

hasMemory :: Memory -> Either Err Memory -> Bool
hasMemory xs (Right ys) = all (`elem` ys) xs
hasMemory _  _          = False

--------------------------------------------------------------------------------

-- | Constructs a test for n-many iterations.
repeatTest :: Int -> Program
repeatTest n =
    [ RepeatStmt (ValE n)
        [ AssignStmt "x" (BinOpE Add (VarE "x") (ValE 1)) ]
    ]

-- | Constructs a test for an arbitary expression in the repeat statement.
repeatArbitraryTest :: Expr -> Program
repeatArbitraryTest expr =
    [ RepeatStmt expr
        [ AssignStmt "x" (BinOpE Add (VarE "x") (ValE 1)) ]
    ]

-- | Constructs a test for if statements.
rawIfTest :: Expr -> Int -> Program
rawIfTest expr n =
    [ IfStmt expr
        [ AssignStmt "x" (ValE n) ]
        []
        []
    ]

-- | Constructs a test for if statements.
ifTest :: Expr -> Int -> Program
ifTest expr n = rawIfTest (BinOpE Equal expr expr) n

-- | Constructs a test for else.
elseTest :: Expr -> Int -> Program
elseTest expr n =
    [ IfStmt (BinOpE Neq expr expr)
        [ AssignStmt "x" (ValE (n+1)) ]
        []
        [ AssignStmt "x" (ValE n) ]
    ]

-- | Constructs a test for if else.
rawIfElseTest :: Expr -> Int -> Program
rawIfElseTest expr n =
    [ IfStmt (BinOpE Neq (ValE 0) (ValE 0))
        [ AssignStmt "x" (ValE (n+1)) ]
        [ (expr, [ AssignStmt "x" (ValE n) ]) ]
        [ AssignStmt "x" (ValE (n-1)) ]
    ]

-- | Constructs a test for if else.
ifElseTest :: Expr -> Int -> Program
ifElseTest expr n = rawIfElseTest (BinOpE Equal expr expr) n

-- | 'fibs' is a stream of fibonacci numbers.
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | 'fib' @n@ constructs a program which calculates the @n@th fibonacci number.
fib :: Int -> Program
fib n =
    [ RepeatStmt (ValE n)
        [
            AssignStmt "x" (VarE "y"),
            AssignStmt "y" (VarE "z"),
            AssignStmt "z" (BinOpE Add (VarE "x") (VarE "y"))
        ]
    ]

-- tests to add
-- * exception handling in nested contexts

-- | The main entry point to the test suite.
main :: IO ()
main = do
    hspec $ do
        describe "Interpreter.interpret" $ do
            it "handles the empty program" $
                interpret [] []
                `shouldSatisfy` isSuccessful
            prop "stores values in memory" $ \(x :: Int) ->
                hasMemory [("x",x)] $ interpret [AssignStmt "x" (ValE x)] []
            prop "loads values from memory" $ \(x :: Int) ->
                hasMemory [("x",x),("y",x)] $
                    interpret [AssignStmt "y" (VarE "x")] [("x",x)]
            prop "memory only contains one entry per variable after multiple assignments" $
                \(x :: Int) (y :: Int) (z :: Int) ->
                    let r = interpret [ AssignStmt "x" (ValE x)
                                      , AssignStmt "x" (ValE y)
                                      , AssignStmt "x" (ValE z)
                                      ] [("x",0)]
                    in case r of
                        Left _  -> property False
                        Right m -> length (map fst m) ===
                                   length (nub $ map fst m)
            prop "handles division by zero in assignments" $ \(x :: Int) ->
                interpret [AssignStmt "x" (BinOpE Div (ValE x) (ValE 0))] []
                === Left DivByZeroError
            prop "handles negative exponents in assignments" $ \(x :: Int) ->
                interpret [AssignStmt "x" (BinOpE Pow (ValE x) (ValE (-1)))] []
                === Left NegativeExponentError
            it "handles uninitialised memory in assignments" $
                interpret [AssignStmt "y" (VarE "x")] []
                `shouldBe` Left (UninitialisedMemory "x")
            prop "interprets arbitrary expressions in assignments" $ \expr ->
                isSuccessful $ interpret [AssignStmt "x" expr] []
            prop "implements repeat" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (repeatTest n) [("x", 0)]
            prop "repeat condition can contain arbitrary expressions" $ \expr ->
                isSuccessful $ interpret (repeatArbitraryTest expr) [("x", 0)]
            prop "repeat correctly handles exceptions in condition" $ \(x :: Int) ->
                     interpret [RepeatStmt (BinOpE Div (ValE x) (ValE 0)) []] []
                     === Left DivByZeroError
                .&&. interpret [RepeatStmt (BinOpE Pow (ValE x) (ValE (-1))) []] []
                     === Left NegativeExponentError
                .&&. interpret [RepeatStmt (VarE "x") []] []
                     === Left (UninitialisedMemory "x")
            prop "repeat correctly handles exceptions in body" $ \(x :: Int) ->
                     interpret [RepeatStmt (ValE 10) [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))]] []
                     === Left DivByZeroError
                .&&. interpret [RepeatStmt (ValE 10) [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))]] []
                     === Left NegativeExponentError
                .&&. interpret [RepeatStmt (ValE 10) [AssignStmt "y" (VarE "x")]] []
                     === Left (UninitialisedMemory "x")
            prop "implements if" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (ifTest (ValE 0) n) [("x", 0)]
            prop "if condition can contain arbitrary expressions" $ \(Positive n) expr ->
                hasMemory [("x",n)] $ interpret (ifTest expr n) [("x", 0)]
            prop "if condition treats non-zero numbers as true" $
                forAll (arbitrary `suchThat` (/= 0)) $ \(n :: Int) ->
                hasMemory [("x",n+1)] $ interpret (rawIfTest (ValE n) (n+1)) [("x", 0)]
            prop "if correctly handles exceptions in condition" $ \(x :: Int) ->
                     interpret [IfStmt (BinOpE Div (ValE x) (ValE 0)) [] [] []] []
                     === Left DivByZeroError
                .&&. interpret [IfStmt (BinOpE Pow (ValE x) (ValE (-1))) [] [] []] []
                     === Left NegativeExponentError
                .&&. interpret [IfStmt (VarE "x") [] [] []] []
                     === Left (UninitialisedMemory "x")
            prop "if correctly handles exceptions in body" $ \(x :: Int) ->
                     interpret [IfStmt (ValE 1) [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))] [] []] []
                     === Left DivByZeroError
                .&&. interpret [IfStmt (ValE 1) [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))] [] []] []
                     === Left NegativeExponentError
                .&&. interpret [IfStmt (ValE 1) [AssignStmt "y" (VarE "x")] [] []] []
                     === Left (UninitialisedMemory "x")
            prop "implements else" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (elseTest (ValE 0) n) [("x", 0)]
            prop "else correctly handles exceptions in body" $ \(x :: Int) ->
                     interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))]] []
                     === Left DivByZeroError
                .&&. interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))]] []
                     === Left NegativeExponentError
                .&&. interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (VarE "x")]] []
                     === Left (UninitialisedMemory "x")
            prop "implements else if" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (ifElseTest (ValE 1) n) [("x", 0)]
            prop "else if condition can contain arbitary expressions" $ \(Positive n) expr ->
                hasMemory [("x",n)] $ interpret (ifElseTest expr n) [("x", 0)]
            prop "else if condition treats non-zero numbers as true" $
                forAll (arbitrary `suchThat` (/= 0)) $ \(n :: Int) ->
                hasMemory [("x",n+1)] $ interpret (rawIfElseTest (ValE n) (n+1)) [("x", 0)]
            prop "else if correctly handles exceptions in condition" $ \(x :: Int) ->
                     interpret [IfStmt (ValE 0) [] [(BinOpE Div (ValE x) (ValE 0), [])] []] []
                     === Left DivByZeroError
                .&&. interpret [IfStmt (ValE 0) [] [(BinOpE Pow (ValE x) (ValE (-1)), [])] []] []
                     === Left NegativeExponentError
                .&&. interpret [IfStmt (ValE 0) [] [(VarE "x", [])] []] []
                     === Left (UninitialisedMemory "x")
            prop "else if correctly handles exceptions in body" $ \(x :: Int) ->
                     interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))])] []] []
                     === Left DivByZeroError
                .&&. interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))])] []] []
                     === Left NegativeExponentError
                .&&. interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (VarE "x")])] []] []
                     === Left (UninitialisedMemory "x")
        describe "Example programs" $ do
            prop "computes fibonacci numbers" $ \(Positive n) ->
                hasMemory [("x", fibs !! (n-1))] $
                    interpret (fib n) [("x",0),("y",0),("z",1)]

--------------------------------------------------------------------------------
