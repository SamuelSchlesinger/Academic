data Expr a 
  = Oper a
  | Mult [Expr a]
  | Plus [Expr a] deriving (Show, Eq)

simplify :: Expr a -> Expr a

simplify (Plus args) = Plus (f args) where
    f [] = []
    f (Plus args' : rest) = (map simplify args') ++ f rest
    f x = [x]
