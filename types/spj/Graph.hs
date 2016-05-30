{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Coercion where
import Data.Map

-- Start
class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]
instance Graph G1 where
  type Vertex G1 = Int
  data Edge   G1 = MkEdge1 (Vertex G1) (Vertex G1) 
  -- ...definitions for methods...

newtype G2 = G2 (Map (Vertex G2) [Vertex G2])
instance Graph G2 where
  type Vertex G2 = String
  data Edge   G2 = MkEdge2 Int (Vertex G2) (Vertex G2)
  -- ...definitions for methods...
