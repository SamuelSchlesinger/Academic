{-# LANGUAGE NoImplicitPrelude #-}

import Delude

-- simplifies filters and such
section a b list = filter ((>= a) && (<= b)) list

-- nice
fours = map 4 [1..100]

-- simplifies functional operations like scaling
f = sin + 5

-- allows for intuitive mathematical notation
h = f ** g

-- allows for some pretty sketchy things to happen, so be careful
g :: Float -> Float
g = 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4  
