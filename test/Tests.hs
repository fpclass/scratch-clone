--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (nub)

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

import Language
import Interpreter

--------------------------------------------------------------------------------

instance Arbitrary Op where
    arbitrary = elements [minBound..maxBound]

safeOps :: [Op]
safeOps = [op | op <- [minBound..maxBound], op /= Div, op /= Pow]

instance Arbitrary Expr where
    arbitrary = resize 10 $ sized arbExpr
        where
            arbExpr :: Int -> Gen Expr
            arbExpr 0 = do
                n <- arbitrary
                return (ValE n)
            arbExpr n = do
                m <- elements [0..n-1]
                op <- elements safeOps
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

-- | `terminates` @result@ essentially does nothing except pattern-match on 
-- @result@. If pattern-matching succeeds (i.e. the computation calculating
-- the argument terminates), `True` is returned.
terminates :: Either a b -> Bool 
terminates (Left _)  = True
terminates (Right _) = True

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

--------------------------------------------------------------------------------

-- | The tests.
tests :: TestTree
tests = localOption (Timeout (5*1000000) "5s") $ testGroup "Interpreter.interpret" 
    [
        testCase "handles the empty program" $
        isSuccessful (interpret [] []) @?= True
    ,   QC.testProperty "stores values in memory" $ \(x :: Int) ->
        hasMemory [("x",x)] $ interpret [AssignStmt "x" (ValE x)] []
    ,   QC.testProperty "loads values from memory" $ \(x :: Int) ->
        hasMemory [("x",x),("y",x)] $ interpret [AssignStmt "y" (VarE "x")] [("x",x)]
    ,   QC.testProperty "memory only contains one entry per variable after multiple assignments" $
        \(x :: Int) (y :: Int) (z :: Int) ->
            let r = interpret [ AssignStmt "x" (ValE x)
                              , AssignStmt "x" (ValE y)
                              , AssignStmt "x" (ValE z)
                              ] [("x",0)]
            in case r of
                Left _  -> property False
                Right m -> length (map fst m) ===
                           length (nub $ map fst m)
    ,   QC.testProperty "handles division by zero in assignments" $ \(x :: Int) ->
        interpret [AssignStmt "x" (BinOpE Div (ValE x) (ValE 0))] []
        === Left DivByZeroError
    ,   QC.testProperty "handles negative exponents in assignments" $ \(x :: Int) ->
        interpret [AssignStmt "x" (BinOpE Pow (ValE x) (ValE (-1)))] []
        === Left NegativeExponentError
    ,   testCase "handles uninitialised memory in assignments" $
        interpret [AssignStmt "y" (VarE "x")] []
        @?= Left (UninitialisedMemory "x")
    ,   QC.testProperty "interprets arbitrary expressions in assignments" $ \expr ->
        isSuccessful $ interpret [AssignStmt "x" expr] []
    ,   QC.testProperty "implements repeat" $ \(Positive n) ->
        hasMemory [("x",n)] $ interpret (repeatTest n) [("x", 0)]
    ,   QC.testProperty "repeat condition can contain arbitrary expressions" $ \expr ->
        terminates $ interpret (repeatArbitraryTest expr) [("x", 0)]
    ,   QC.testProperty "repeat correctly handles exceptions in condition" $ \(x :: Int) ->
             interpret [RepeatStmt (BinOpE Div (ValE x) (ValE 0)) []] []
             === Left DivByZeroError
        .&&. interpret [RepeatStmt (BinOpE Pow (ValE x) (ValE (-1))) []] []
             === Left NegativeExponentError
        .&&. interpret [RepeatStmt (VarE "x") []] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "repeat correctly handles exceptions in body" $ \(x :: Int) ->
             interpret [RepeatStmt (ValE 10) [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))]] []
             === Left DivByZeroError
        .&&. interpret [RepeatStmt (ValE 10) [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))]] []
             === Left NegativeExponentError
        .&&. interpret [RepeatStmt (ValE 10) [AssignStmt "y" (VarE "x")]] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "implements if" $ \(Positive n) ->
        hasMemory [("x",n)] $ interpret (ifTest (ValE 0) n) [("x", 0)]
    ,   QC.testProperty "if condition can contain arbitrary expressions" $ \(Positive n) expr ->
        hasMemory [("x",n)] $ interpret (ifTest expr n) [("x", 0)]
    ,   QC.testProperty "if condition treats non-zero numbers as true" $
        forAll (arbitrary `suchThat` (/= 0)) $ \(n :: Int) ->
        hasMemory [("x",n+1)] $ interpret (rawIfTest (ValE n) (n+1)) [("x", 0)]
    ,   QC.testProperty "if correctly handles exceptions in condition" $ \(x :: Int) ->
             interpret [IfStmt (BinOpE Div (ValE x) (ValE 0)) [] [] []] []
             === Left DivByZeroError
        .&&. interpret [IfStmt (BinOpE Pow (ValE x) (ValE (-1))) [] [] []] []
             === Left NegativeExponentError
        .&&. interpret [IfStmt (VarE "x") [] [] []] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "if correctly handles exceptions in body" $ \(x :: Int) ->
             interpret [IfStmt (ValE 1) [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))] [] []] []
             === Left DivByZeroError
        .&&. interpret [IfStmt (ValE 1) [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))] [] []] []
             === Left NegativeExponentError
        .&&. interpret [IfStmt (ValE 1) [AssignStmt "y" (VarE "x")] [] []] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "implements else" $ \(Positive n) ->
        hasMemory [("x",n)] $ interpret (elseTest (ValE 0) n) [("x", 0)]
    ,   QC.testProperty "else correctly handles exceptions in body" $ \(x :: Int) ->
             interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))]] []
             === Left DivByZeroError
        .&&. interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))]] []
             === Left NegativeExponentError
        .&&. interpret [IfStmt (ValE 0) [] [] [AssignStmt "y" (VarE "x")]] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "implements else if" $ \(Positive n) ->
        hasMemory [("x",n)] $ interpret (ifElseTest (ValE 1) n) [("x", 0)]
    ,   QC.testProperty "else if condition can contain arbitary expressions" $ \(Positive n) expr ->
        hasMemory [("x",n)] $ interpret (ifElseTest expr n) [("x", 0)]
    ,   QC.testProperty "else if condition treats non-zero numbers as true" $
        forAll (arbitrary `suchThat` (/= 0)) $ \(n :: Int) ->
        hasMemory [("x",n+1)] $ interpret (rawIfElseTest (ValE n) (n+1)) [("x", 0)]
    ,   QC.testProperty "else if correctly handles exceptions in condition" $ \(x :: Int) ->
             interpret [IfStmt (ValE 0) [] [(BinOpE Div (ValE x) (ValE 0), [])] []] []
             === Left DivByZeroError
        .&&. interpret [IfStmt (ValE 0) [] [(BinOpE Pow (ValE x) (ValE (-1)), [])] []] []
             === Left NegativeExponentError
        .&&. interpret [IfStmt (ValE 0) [] [(VarE "x", [])] []] []
             === Left (UninitialisedMemory "x")
    ,   QC.testProperty "else if correctly handles exceptions in body" $ \(x :: Int) ->
             interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (BinOpE Div (ValE x) (ValE 0))])] []] []
             === Left DivByZeroError
        .&&. interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (BinOpE Pow (ValE x) (ValE (-1)))])] []] []
             === Left NegativeExponentError
        .&&. interpret [IfStmt (ValE 0) [] [(ValE 1, [AssignStmt "y" (VarE "x")])] []] []
             === Left (UninitialisedMemory "x")
    ,   testGroup "Example programs" [
            QC.testProperty "computes fibonacci numbers" $ \(Positive n) ->
                hasMemory [("x", fibs !! (n-1))] $ interpret (fib n) [("x",0),("y",0),("z",1)]
        ]
    ]

-- | The list of tasty ingredients. Note: the order seems to matter, 
-- anyXMLRunner won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | The main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------