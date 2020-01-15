module Expr where

import Parsing
import Prelude 
import Data.Char(isSpace,isDigit,toLower)
import Test.QuickCheck

------ Examples
ex1 = BinOperation Mul  (BinOperation Add Var (Num 2)) Var 
ex2 = BinOperation Add Var (BinOperation Mul  (Num 2) Var )
ex3 = BinOperation Add (Num (-5)) (BinOperation Mul (Num 2)  (Num 4))
-------------------- A
-- Represents an expression

data BinOperator = Add | Mul
  deriving (Eq)
instance Show BinOperator where
  show Add = "+"
  show Mul = "*"

getBinPrec :: BinOperator -> Int
getBinPrec Add = 1
getBinPrec Mul = 2

getBinOp :: BinOperator -> (Double -> Double -> Double)
getBinOp Add = (+)
getBinOp Mul = (*)

data FuncType = Sin | Cos
  deriving (Eq)
instance Show FuncType where
  show Sin = "sin"
  show Cos = "cos"

getFunc :: FuncType -> (Double -> Double)
getFunc Sin = Prelude.sin
getFunc Cos = Prelude.cos

data Expr = Num Double
          | Var 
          | BinOperation BinOperator Expr Expr
          | Function FuncType Expr
          deriving(Eq)

x :: Expr
x = Var
          
num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = BinOperation Add 
mul = BinOperation Mul

sin,cos :: Expr -> Expr 
sin = Function Sin 
cos = Function Cos

-------------------- B

-- Returns a string representation of an expression
showExpr :: Expr -> String
showExpr (Num n)                 = show n
showExpr Var                     = "x"
showExpr (Function t Var)        = show t ++ " "  ++ showExpr Var              
showExpr (Function t (Num n))    = show t ++ " "  ++ showExpr (Num n)       
showExpr (Function t e)          = show t ++ " (" ++ showExpr e ++ ")"
showExpr (BinOperation op e1 e2) = showPrecedence op e1  ++ " " ++ show op ++ " " ++ showPrecedence op e2
  where 
    showPrecedence :: BinOperator -> Expr -> String
    showPrecedence op1 (BinOperation op2 e1 e2)
     | (getBinPrec op1) > (getBinPrec op2) =  "(" ++ showExpr (BinOperation op2 e1 e2) ++ ")"
     | otherwise                           =  showExpr (BinOperation op2 e1 e2)
    showPrecedence _ e = showExpr e
instance Show Expr where 
  show = showExpr


-------------------- C
-- Evaluates an expression
eval :: Expr -> Double -> Double 
eval Var x = x
eval (Num n) _ = n
eval (BinOperation op e1 e2) x = (getBinOp op) (eval e1 x) (eval e2 x)
eval (Function t e) x = getFunc t $ (eval e x)

-------------------- D

-- Parses a string to an expression
readExpr :: String -> Maybe Expr
readExpr s = 
    let s' = map toLower $ filter (not.isSpace) s in 
        case parse expr s' of
            Just (e, "") -> Just e
            _            -> Nothing

-- Parses a number (-, ., digits)
number :: Parser Double
number = readsP

-- Parses a sinus function
sinus :: Parser Expr
sinus = do
  c1 <- char 's'
  c2 <- char 'i'
  c3 <- char 'n'
  e <- factor
  return (Function Sin e)

-- Parses a cosine function
cosine:: Parser Expr
cosine = do
  c1 <- char 'c'
  c2 <- char 'o'
  c3 <- char 's'
  e <- factor
  return (Function Cos e)

-- Parses expressions 
expr, term, factor, var :: Parser Expr
expr   = foldl1 (BinOperation Add) <$> chain term (char '+')
term   = foldl1 (BinOperation Mul) <$> chain factor (char '*')
factor =  
  Num <$> number 
  <|> var 
  <|>  sinus 
  <|> cosine 
  <|> char '(' *> expr <* char ')'
var = do 
  t <-  sat (\x -> x == 'x' || x == '-')
  return Var

-------------------- E

-- Property that validates that show and read works

prop_ShowReadExpr :: Expr -> Double -> Bool
prop_ShowReadExpr e d = case readExpr (showExpr e) of
  Just e' -> abs (eval e d - eval e' d) < 0.00001
  _         -> False

-- Generates an arbitrary expression based on a sized parameter

arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1, rNum), (i, rBin i), (i `div` 2, rFunc i)] 
  where
    rNum = Num <$> arbitrary
    rBin s = do
      let s' = s `div` 2
      op <- elements [Add, Mul]
      e1 <- arbExpr s' 
      e2 <- arbExpr s' 
      return $ BinOperation op e1 e2
    rFunc d = do
      let d' = d `div` 2
      op <- elements [Sin, Cos]
      e <- arbExpr d'
      return $ Function op e

instance Arbitrary Expr where
  arbitrary = sized arbExpr
-------------------- F

-- Represents if the expression has been simplified
type Simplfied = Bool

-- Simplifies an expression
simplify :: Expr -> Expr
simplify e = simplify' e False
  where
    simplify' :: Expr -> Simplfied -> Expr
    simplify' Var _                     = Var
    simplify' (Num n) _                 = Num n

    simplify' (BinOperation Add (Num 0) e) True      = e
    simplify' (BinOperation Add e (Num 0)) True      = e
    simplify' (BinOperation Add (Num n1) (Num n2)) _ = Num (n1 + n2)
    simplify' (BinOperation Add e1 e2) False         = 
      simplify' (BinOperation Add (simplify' e1 False) (simplify' e2 False)) True
    simplify' (BinOperation Add e1 e2) True          = BinOperation Add e1 e2

    simplify' (BinOperation Mul (Num 0) _) _         = Num 0
    simplify' (BinOperation Mul _ (Num 0)) _         = Num 0
    simplify' (BinOperation Mul (Num 1) e) True      = e
    simplify' (BinOperation Mul e (Num 1)) True      = e
    simplify' (BinOperation Mul (Num n1) (Num n2)) _ = Num (n1 * n2)
    simplify' (BinOperation Mul e1 e2) False         = 
      simplify' (BinOperation Mul (simplify' e1 False) (simplify' e2 False)) True
    simplify' (BinOperation Mul e1 e2) True          = BinOperation Mul e1 e2

    simplify' (Function t (Num n)) _           = Num ((getFunc t) n)
    simplify' (Function t e) False             = 
      simplify' (Function t (simplify' e False)) True
    simplify' (Function t e) True              = Function t e
    
prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e d = (eval e d) == (eval (simplify e) d)
-------------------- G

-- Derives an expression
differentiate :: Expr -> Expr
differentiate = simplify.differentiate'
    where
      differentiate' (Num n)     = Num 0
      differentiate' Var         = Num 1
      differentiate' (BinOperation Add e1 e2) = BinOperation Add (differentiate' e1) (differentiate' e2)
      differentiate' (BinOperation Mul e1 e2) = BinOperation Add (BinOperation Mul (differentiate' e1) e2) 
                                                    (BinOperation Mul e1 (differentiate' e2))
      differentiate' (Function Cos e)     = BinOperation Mul (BinOperation Mul (Num (-1)) (Function Sin e)) (differentiate' e)
      differentiate' (Function Sin e)     = BinOperation Mul (Function Cos e) (differentiate' e)

-------------------- DONE



