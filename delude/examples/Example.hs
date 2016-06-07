{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, ScopedTypeVariables #-}

import Delude

class Agent agent where
    location :: agent -> (Float, Float)
    health, strength, fitness :: agent -> Float
    fitness = strength + health

data Herb = Herb (Float, Float) Float

instance Agent Herb where
    location (Herb x _) = x
    health (Herb _ x) = x
    strength (Herb _ _) = 0

data Eater = Eater (Float, Float) Float Float
    
instance Agent Eater where
    location (Eater x _ _) = x
    health (Eater _ x _) = x
    strength (Eater _ _ x) = x

data Horde = Horde [Eater]

instance Agent Horde where
    location (Horde eaters) = sum (map location eaters)
