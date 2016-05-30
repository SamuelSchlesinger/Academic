{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies  
           , FlexibleInstances #-}

module Graph (Graph, adj, edges, vertices, reflexive,
              transitive, symmetric) where 

import qualified Data.Set as S

type Graph = ([Int], Int -> Int -> Bool)

adj :: Graph -> Int -> [Int]
adj (vs, e) v = filter (e v) vs

edges :: Graph -> [(Int, Int)]
edges (vs, e) = [(v, w) | v <- vs, w <- vs, e v w]

vertices :: Graph -> [Int]
vertices = fst

reflexive :: Graph -> Bool
reflexive (vs, e) = all id [e v v | v <- vs]

transitive :: Graph -> Bool
transitive (vs, e) = all id [e a c | a <- vs, b <- vs, e a b, c <- vs, e b c]

symmetric :: Graph -> Bool
symmetric (vs, e) = all id [e a b == e b a | a <- vs, b <- vs]

mkGraph :: [Int] -> (Int -> Int -> Bool) -> Graph
mkGraph ns e = (ns, e)

type GraphHom = (Int -> Int, (Int -> Int -> Bool) -> (Int -> Int -> Bool))

isValid :: GraphHom -> Graph -> Bool

implies :: Bool -> Bool -> Bool
implies True False = False
implies _    _     = True

isValid (fv, fe) g@(vs, e) = let e' = fe e in
                                 all id [e v w `implies` e' v' w' | v <- vs, w <- vs,
                                                               let v' = fv v, let w' = fv w]
