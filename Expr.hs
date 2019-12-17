module Expr(
  Expr,eval,showExpr,readExpr,simplify,differentiate
) where

import Parsing
import Prelude 
import Data.Char(isSpace,isDigit,toLower)
import Test.QuickCheck

------ EX
ex1 = Mul (Add Var (Num 2)) Var 
ex2 = Add Var (Mul (Num 2) Var )
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)
-------------------- A
data Expr = Num Double
          | Var 
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving(Eq)


          
x :: Expr
x = Var
          
num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2
mul e1 e2 = Mul e1 e2

sin,cos :: Expr -> Expr 
sin e1 = Sin e1 
cos e1 = Cos e1 

-------------------- B
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Var) =  "x"
showExpr (Add e e') = 
  showExpr e ++ " + " ++ showExpr e'
showExpr (Sin e) = "Sin " ++ showExpr e
showExpr (Cos e) = "Cos " ++ showExpr e 
showExpr (Mul e e') = 
  showFactor e ++ " * " ++ showFactor e'
  where 
    showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
    showFactor e           = showExpr e
instance Show Expr
  where show = showExpr


-------------------- C
eval :: Expr -> Double -> Double 
eval Var x = x
eval (Num n) _ = n
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Sin e) x = Prelude.sin $ eval e x
eval (Cos e) x = Prelude.cos $ eval e x

-------------------- D

readExpr :: String -> Maybe Expr
readExpr s = 
    let s' = map toLower $ filter (not.isSpace) s in 
        case parse expr s' of
            Just (e, "") -> Just e
            otherwise -> Nothing




number :: Parser Double
number = read <$> oneOrMore (sat (\x -> (isDigit x ||  (x =='.') || (x == '-'))))

sinus :: Parser Expr
sinus = do
  c1 <- char 's'
  c2 <- char 'i'
  c3 <- char 'n'
  expr

cosine:: Parser Expr
cosine = do
  c1 <- char 'c'
  c2 <- char 'o'
  c3 <- char 's'
  expr

expr, term, factor, var :: Parser Expr
expr   = foldl1 Add <$> chain term (char '+')
term   = foldl1 Mul <$> chain factor (char '*')
factor =  Num <$> number <|> var <|> Sin <$> sinus <|> Cos <$> cosine <|> char '(' *> expr <* char ')'
var = do 
  t <-  (sat (\x -> (x == 'x' || x == '-')))
  return Var

-------------------- E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case readExpr (showExpr e) of
  Just (e') -> e' == e || e' == assoc e
  _         -> False 
  where
    assoc :: Expr -> Expr
    assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3))
    assoc (Add e1          e2) = Add (assoc e1) (assoc e2)
    assoc (Mul e1 e2)          = Mul (assoc e1) (assoc e2)
    assoc (Sin e)              = Sin (assoc e)
    assoc (Cos e)              = Cos (assoc e)
    assoc (Var)                = Var  
    assoc (Num n)              = Num n


arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1, rNum), (i, rBin i), (i `div` 2, rFunc i)] 
  where
    range = 4
    rNum = elements $ map Num [-range..range]
    rBin s = do
      let s' = s `div` 2
      op <- elements [Add, Mul]
      e1 <- arbExpr s' 
      e2 <- arbExpr s' 
      return $ op e1 e2
    rFunc d = do
      let d' = d `div` 2
      op <- elements [Sin, Cos]
      e <- arbExpr d'
      return $ op e

instance Arbitrary Expr where
  arbitrary = sized arbExpr
-------------------- F

type Simplfied = Bool


simplify :: Expr -> Expr
simplify e = simplify' e False
  where
    simplify' :: Expr -> Simplfied -> Expr
    simplify' (Var) _ = Var
    simplify' (Num n) _ = Num n

    simplify' (Add (Num 0) e) True = e
    simplify' (Add e (Num 0)) True = e
    simplify' (Add (Num n1) (Num n2)) _ = Num (n1 + n2)
    simplify' (Add e1 e2) False = simplify' (Add (simplify' e1 False) (simplify' e2 False)) True
    simplify' (Add e1 e2) True = Add e1 e2

    simplify' (Mul (Num 0) _) _ = Num 0
    simplify' (Mul _ (Num 0)) _ = Num 0
    simplify' (Mul (Num 1) e) True = e
    simplify' (Mul e (Num 1)) True = e
    simplify' (Mul (Num n1) (Num n2)) _ = Num (n1 * n2)
    simplify' (Mul e1 e2) False = simplify' (Mul (simplify' e1 False) (simplify' e2 False)) True
    simplify' (Mul e1 e2) True = Mul e1 e2

    simplify' (Sin (Num n)) _ = Num (Prelude.sin n)
    simplify' (Sin e) False = simplify' (Sin (simplify' e False)) True
    simplify' (Sin e) True = Sin e

    simplify' (Cos (Num n)) _ = Num (Prelude.cos n)
    simplify' (Cos e) False = simplify' (Cos (simplify' e False)) True
    simplify' (Cos e) True = Cos e

-------------------- G

differentiate :: Expr -> Expr
differentiate = simplify.differentiate'
    where
      differentiate' (Num n) = Num 0
      differentiate' (Var) = Num 1
      differentiate' (Add e1 e2) = Add (differentiate' e1) (differentiate' e2)
      differentiate' (Mul e1 e2) = Add (Mul (differentiate' e1) e2) (Mul e1 (differentiate' e2))
      differentiate' (Cos e) = Mul (Mul (Num (-1)) (Sin e)) (differentiate' e)
      differentiate' (Sin e) = Mul (Cos e) (differentiate' e)

-------------------- DONE



