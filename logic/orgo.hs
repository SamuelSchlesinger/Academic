data Mo
  = H
  | C Mo Mo Mo Mo deriving (Show)

methane :: Mo
methane = C H H H H

ethane :: Mo -> Mo
ethane r = C r H H H

f :: a -> b

flist :: [a] -> [b]
flist xs = map xs 
