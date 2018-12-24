module CSV(nnEdgeListFromString,  nodeListFromNNEdgeList, xinput , makeIndexMap ) where

-- sttp://web.engr.oregonstate.edu/~erwig/fgl/haskell/

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import SimpleGraph(SGNode, SGEdge, SimpleGraph)

foo :: Int
foo = 27

data NNEdge = NNEdge { from :: String, to:: String, flow:: Float} deriving(Show )


makeIndexMap :: [String] -> Map.Map String Int
makeIndexMap list =
  Map.fromList (zip list [1..(length list)])


{-|
*ghci > xinput =  "a,b,1.2\nc,d,4.6\nc,e,8,9\n"
*ghci > nnEdgeListFromString xinput
*ghci > nodeListFromNNEdgeList el
["d","b","c","a"]
-}
nnEdgeListFromString :: String -> [NNEdge]
nnEdgeListFromString input =
  case parseCSV input of
    Left _ -> []
    Right list -> map nnEdgeFromList list


{-|
*ghci > xinput =  "a,b,1.2\nc,d,4.6\nc,e,8,9\n"
*ghci > el nnEdgeListFromString xinput

-}
nodeListFromNNEdgeList :: [NNEdge] -> [String]
nodeListFromNNEdgeList nnEdgeList =
  unique ((map from nnEdgeList) ++ (map to nnEdgeList))

unique :: Eq a => [a] -> [a]
unique = foldl (\acc x -> if (elem x acc) then acc else x:acc) []

{-|
*ghci > nnEdgeFromList ["a", "b", "1.22"]
NNEdge {from = "a", to = "b", flow = 1.22}
-}
nnEdgeFromList :: [String] ->  NNEdge
nnEdgeFromList list =
    let
      from = (list !! 0)
      to = (list !! 1)
      flow = read (list !! 2) :: Float
    in
      NNEdge from to flow

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

{-|
*ghci > parseCSV "a,b,1\nc,d,2\n"
Right [["a","b","1"],["c","d","2"]]
-}
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

xinput =  "a,b,1.2\nc,d,4.6\n"
