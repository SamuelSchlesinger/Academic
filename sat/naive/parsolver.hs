{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.Directory
import Control.Parallel.Strategies
import System.IO
import System.Environment

data Expr b
  = Val Bool
  | Var b
  | Not (Expr b)
  | And (Expr b) (Expr b)
  | Or  (Expr b) (Expr b)

instance (Show b) => Show (Expr b) where
  show (Val val) = show val
  show (Var var) = show var
  show (Not e) = '~' : show e
  show (Or  e e') = "(" ++ show e ++ " | " ++ show e' ++ ")"
  show (And e e') = "(" ++ show e ++ " & " ++ show e' ++ ")"

assign :: (Eq b) => Expr b -> b -> Bool -> Expr b
assign v@(Val _) b val = v
assign v@(Var b') b val
  | b == b'   = Val val
  | otherwise = v
assign (Not e) b val = Not (assign e b val)
assign (And e e') b val = And (assign e b val) (assign e' b val)
assign (Or  e e') b val = Or  (assign e b val) (assign e' b val)

simplify :: Expr b -> Expr b
simplify (And (Val True) (Val True)) = Val True
simplify (And (Val False) _)        = Val False
simplify (And _ (Val False))         = Val False
simplify (And (Val True) e)          = e
simplify (And e (Val True))          = e
simplify a@(And e e') = simplify' (And (simplify e) (simplify e'))
simplify (Or (Val False) (Val False)) = Val False
simplify (Or (Val True) _)            = Val True
simplify (Or _ (Val True))            = Val True
simplify (Or (Val False) e)           = e
simplify (Or e (Val False))           = e
simplify o@(Or  e e') = simplify' (Or  (simplify e) (simplify e'))
simplify (Not (Val b)) = Val $ not b
simplify (Not e)       = simplify' (Not (simplify e))
simplify v = v

simplify' (And (Val True) (Val True)) = Val True
simplify' (And (Val False) _)         = Val False
simplify' (And _ (Val False))         = Val False
simplify' a@(And _ _)                 = a
simplify' (Or (Val False) (Val False)) = Val False
simplify' (Or (Val True) _)            = Val True
simplify' (Or _ (Val True))            = Val True
simplify' o@(Or  e e') = o
simplify' (Not (Val b)) = Val $ not b
simplify' n@(Not e)       = n

vars :: Expr b -> [b]
vars (Var b) = [b]
vars (Val _) = []
vars (Not e) = vars e
vars (And e e') = vars e ++ vars e'
vars (Or  e e') = vars e ++ vars e'

sat :: (Eq b) => Expr b -> Bool
sat (Val True) = True
sat (Val False) = False
sat e = case vars e of
  [] -> sat $ simplify e
  (x:_) -> sat (simplify $ assign e x True) || sat (simplify $ assign e x False)

solution :: (Eq b) => Expr b -> Maybe [(b, Bool)]
solution = solution' []

solution' :: (Eq b) => [(b, Bool)] -> Expr b -> Maybe [(b, Bool)]
solution' bs (Val False) = Nothing
solution' bs (Val True)  = Just bs
solution' bs e = case vars e of
  []    -> solution' bs $ simplify e
  (x:_) -> runEval $ do
    ts <- rpar $ solution' ((x, True):bs) (simplify $ assign e x True) 
    tf <- rpar $ solution' ((x, False):bs) (simplify $ assign e x False)
    return $ ts <|> tf

fromFile :: FilePath -> IO (Expr Int)
fromFile fp = do
  contents <- readFile fp
  let rows = drop 8 $ map words (lines contents)
  let rows' = take (length rows - 3)  rows
  let exps = map fromLine rows'
  return (foldl And (Val True) exps)

fromLine (a:as) | n < 0 = Not (Var (-n))
                | n > 0 = Var n `Or` fromLine as
                | n == 0 = Val True where
  n = read a :: Int

mainSolve :: [String] -> IO ()
mainSolve [] = return ()
mainSolve (fp:fs) = do
  inst <- fromFile fp
  putStrLn $ show inst
  putStrLn $ show $ solution inst
  mainSolve fs

main = do 
  (dir:_) <- getArgs
  contents <- getDirectoryContents dir
  mainSolve $ map (\x -> dir ++ x) $ filter (\x -> x /= "." && x /= "..") contents
