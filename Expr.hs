import Parsing
import Prelude 
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


-------------------- E
-------------------- F
-------------------- G


