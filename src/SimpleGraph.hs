module SimpleGraph
  ( makeGraph, makeNode, makeEdge
  , NodeLabel(NodeLabel), EdgeLabel(EdgeLabel)
  , outFlow, inFlow, totalFlow
  , xNodeList, xEdgeList, xg, labEdges
  ) where

import Data.Graph.Inductive.Graph (Node, LNode, LEdge, mkGraph, lsuc, lpre, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)

data NodeLabel = NodeLabel {name :: String} deriving(Show)
data EdgeLabel = EdgeLabel {edgeFlow :: Float} deriving(Show)


-- Graph constructor functions

makeGraph :: [LNode NodeLabel] -> [LEdge EdgeLabel] -> Gr NodeLabel EdgeLabel
makeGraph nodeList edgeList =
  mkGraph nodeList edgeList

makeNode :: Int -> String -> LNode NodeLabel
makeNode k name =
  (k, NodeLabel name)

makeEdge :: Int -> Int -> Float -> LEdge EdgeLabel
makeEdge from to flow =
  (from, to, EdgeLabel flow)

--
-- FLOWS
--
--
totalFlow :: Gr NodeLabel EdgeLabel -> Float
totalFlow g =
  sum (map (edgeFlow . edgeLabel) (labEdges g))

outEdges :: Gr NodeLabel EdgeLabel -> Node -> [(Node, EdgeLabel)]
outEdges g k =
   (lsuc g k)

{-\ outFlow xg 2 == 3.0 -}
outFlow :: Gr NodeLabel EdgeLabel -> Node -> Float
outFlow g k =
  sum ((map (\(a,b) -> edgeFlow b) (outEdges g k)))

inEdges :: Gr NodeLabel EdgeLabel -> Node -> [(Node, EdgeLabel)]
inEdges g k =
   (lpre g k)

{-\ inFlow xg 2 == 1.0 -}
inFlow :: Gr NodeLabel EdgeLabel -> Node -> Float
inFlow g k =
  sum ((map (\(a,b) -> edgeFlow b) (inEdges g k)))

-- TEST DATA

xg = makeGraph xNodeList xEdgeList
xNodeList = [n0, n1, n2, n3, n4, n5]
xEdgeList = [e0, e1, e2, e3, e4]

n0 = makeNode 0 "o"
n1 = makeNode 1 "a"
n2 = makeNode 2 "b"
n3 = makeNode 3 "c"
n4 = makeNode 4 "d"
n5 = makeNode 5 "e"

e0 = makeEdge 0 1 1.0
e1 = makeEdge 1 2 1.0
e2 = makeEdge 2 3 1.0
e3 = makeEdge 2 4 1.0
e4 = makeEdge 2 5 1.0


--
-- data SimpleEdge = SimpleEdge
--   { fromLabel :: String
--   , toLabel :: String
--   , flow :: Float } deriving (Show)

-- data SimpleNode = LNode NodeLabel

-- data SimpleEdgeList = List SimpleEdge

-- e0 = SimpleEdge "o" "a" 1.0
-- e1 = SimpleEdge "a" "b" 1.0
-- e2 = SimpleEdge "b" "c" 1.0
-- e3 = SimpleEdge "b" "d" 1.0
-- e4 = SimpleEdge "b" "e" 1.0
--
-- xel = [e0, e1, e2, e3, e4]
