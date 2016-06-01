{-# LANGUAGE NoImplicitPrelude #-}

import Delude

data Dif n = Dif { val :: n, dval :: n } deriving (Show)

dVar :: (Num n) => n -> Dif n
dVar n = Dif n 1 

instance (Num n) => Num (Dif n) where
    d + d' = Dif { val = val d + val d', dval = dval d + dval d' }
    d * d' = Dif { val = val d * val d', dval = (val d * dval d') + (val d' * dval d) }
    abs d = d { val = abs (val d), dval = signum (val d) }
    signum d = Dif { val = 0, dval = 0 }
    fromInteger n = Dif { val = fromInteger n, dval = 0 }
    negate d = d { val = negate (val d), dval = negate (dval d) }

instance (Fractional f) =>  Fractional (Dif f) where
    pi = Dif { val = pi, dval = 0 }
    exp x = Dif { val = exp (val x), dval = dval x * val x }
    log x = Dif { val = log (val x), dval = dval x * -- brain pooped exactly here  
