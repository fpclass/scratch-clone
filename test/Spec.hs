--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

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

-- | Constructs a test for if statements.
ifTest :: Int -> Program
ifTest n =
    [ IfStmt (BinOpE Equal (ValE 0) (ValE 0))
        [ AssignStmt "x" (ValE n) ]
        []
        []
    ]

-- | Constructs a test for else.
elseTest :: Int -> Program
elseTest n =
    [ IfStmt (BinOpE Neq (ValE 0) (ValE 0))
        [ AssignStmt "x" (ValE (n+1)) ]
        []
        [ AssignStmt "x" (ValE n) ]
    ]

-- | Constructs a test for if else.
ifElseTest :: Int -> Program
ifElseTest n =
    [ IfStmt (BinOpE Neq (ValE 0) (ValE 0))
        [ AssignStmt "x" (ValE (n+1)) ]
        [ (BinOpE Equal (ValE 1) (ValE 1), [ AssignStmt "x" (ValE n) ]) ]
        [ AssignStmt "x" (ValE (n-1)) ]
    ]

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- x = 0
-- y = 0
-- z = 1
fib :: Int -> Program
fib n =
    [ RepeatStmt (ValE n)
        [
            AssignStmt "x" (VarE "y"),
            AssignStmt "y" (VarE "z"),
            AssignStmt "z" (BinOpE Add (VarE "x") (VarE "y"))
        ]
    ]

-- | The main entry point to the test suite.
main :: IO ()
main = do
    hspec $ do
        describe "Interpreter.interpret" $ do
            it "handles the empty program" $
                interpret [] []
                `shouldSatisfy` isSuccessful
            it "stores values in memory" $
                interpret [AssignStmt "x" (ValE 42)] []
                `shouldSatisfy` hasMemory [("x",42)]
            it "loads values from memory" $
                interpret [AssignStmt "y" (VarE "x")] [("x",42)]
                `shouldSatisfy` hasMemory [("x",42),("y",42)]
            it "handles division by zero" $
                interpret [AssignStmt "x" (BinOpE Div (ValE 42) (ValE 0))] []
                `shouldBe` Left DivByZeroError
            it "handles negative exponents" $
                interpret [AssignStmt "x" (BinOpE Pow (ValE 42) (ValE (-1)))] []
                `shouldBe` Left NegativeExponentError
            it "handles uninitialised memory" $
                interpret [AssignStmt "y" (VarE "x")] []
                `shouldBe` Left (UninitialisedMemory "x")
            prop "interprets expressions" $ \expr ->
                isSuccessful $ interpret [AssignStmt "x" expr] []
            prop "implements repeat" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (repeatTest n) [("x", 0)]
            prop "implements if" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (ifTest n) [("x", 0)]
            prop "implements else" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (elseTest n) [("x", 0)]
            prop "implements if else" $ \(Positive n) ->
                hasMemory [("x",n)] $ interpret (ifElseTest n) [("x", 0)]
        describe "Example programs" $ do
            prop "computes fibonacci numbers" $ \(Positive n) ->
                hasMemory [("x", fibs !! (n-1))] $
                    interpret (fib n) [("x",0),("y",0),("z",1)]

--------------------------------------------------------------------------------
