import Parsing
import Prelude 
import Data.Char(isSpace,isDigit)
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
          deriving(Eq,Show)


          
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
--instance Show Expr
  --where show = showExpr


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
    let s' = filter (not.isSpace) s in 
        case parse expr s' of
            Just (e, "") -> Just e
            otherwise -> Nothing





number = read <$> oneOrMore (sat (\x -> (isDigit x ||  (x =='.'))))

func c1 c2 c3 f = do 
  c1 <- char c1

expr, term, factor, func, var :: Parser Expr
expr   = foldl1 Add <$> chain term (char '+')
term   = foldl1 Mul <$> chain factor (char '*')
factor =  Num <$> number  <|> var <|>  char '(' *> expr <* char ')'
func = undefined
var = do 
  t <-  (sat (\x -> x == 'x'))
  return Var

prop_readShow_valid :: Expr -> Bool
prop_readShow_valid e = case readExpr (showExpr e) of
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

-------------------- E

-------------------- F

-------------------- G

-------------------- DONE



