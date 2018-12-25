module SimpleGraph
  ( SimpleGraph, SGNode, SGEdge, makeGraph, makeNode, makeEdge
  , NodeLabel(NodeLabel), EdgeLabel(EdgeLabel)
  , sourceNode, targetNode, nodeData, nodeMap
  , outFlow, inFlow, totalFlow, flowOnEdge
  , xg, labEdges
  ) where

import Data.Graph.Inductive.Graph (Node, LNode, LEdge, mkGraph, lsuc, lpre
   , toEdge, labNodes, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map.Strict as Map

data NodeLabel = NodeLabel {name :: String} deriving(Show)
data EdgeLabel = EdgeLabel {edgeFlow :: Float} deriving(Show)
type SGNode = LNode NodeLabel
type SGEdge = LEdge EdgeLabel


type SimpleGraph = Gr NodeLabel EdgeLabel

-- Graph constructor functions

makeGraph :: [SGNode] -> [SGEdge] -> SimpleGraph
makeGraph nodeList edgeList =
  mkGraph nodeList edgeList

makeNode :: Int -> String -> SGNode
makeNode k name =
  (k, NodeLabel name)

makeEdge :: Int -> Int -> Float -> SGEdge
makeEdge from to flow =
  (from, to, EdgeLabel flow)

nodeData :: SGNode -> (Int, String)
nodeData (node, label)  =
  (node, name label)

nodeMap :: SimpleGraph -> Map.Map Int String
nodeMap graph =
  Map.fromList (map nodeData (labNodes graph))
--
-- EDGES
--

sourceNode :: SGEdge -> Node
sourceNode  sge =
  (fst . toEdge) sge

targetNode :: SGEdge -> Node
targetNode  sge =
  (snd . toEdge) sge
--
-- FLOWS
--
--
totalFlow :: SimpleGraph -> Float
totalFlow g =
  sum (map (edgeFlow . edgeLabel) (labEdges g))

{-\ outFlow xg 2 == 3.0 -}
outFlow :: SimpleGraph -> Node -> Float
outFlow g k =
  sum ((map (\(a,b) -> edgeFlow b) (outEdges g k)))

{-\ inFlow xg 2 == 1.0 -}
inFlow :: SimpleGraph -> Node -> Float
inFlow g k =
  sum ((map (\(a,b) -> edgeFlow b) (inEdges g k)))

flowOnEdge :: SGEdge -> Float
flowOnEdge edge =
  (edgeFlow . edgeLabel) edge



outEdges :: SimpleGraph -> Node -> [(Node, EdgeLabel)]
outEdges g k =
   (lsuc g k)


inEdges :: SimpleGraph -> Node -> [(Node, EdgeLabel)]
inEdges g k =
   (lpre g k)


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
