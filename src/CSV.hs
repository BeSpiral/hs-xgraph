module CSV(nnEdgeListFromString, xinput) where

-- sttp://web.engr.oregonstate.edu/~erwig/fgl/haskell/

import Text.ParserCombinators.Parsec

foo :: Int
foo = 27

data NNEdge = NNEdge { from :: String, to:: String, flow:: Float} deriving(Show )


{-|
*ghci > xinput =  "a,b,1.2\nc,d,4.6\n"
*ghci > nnEdgeListFromString xinput
[NNEdge {from = "a", to = "b", flow = 1.2},NNEdge {from = "c", to = "d", flow = 4.6}]
-}
nnEdgeListFromString :: String -> [NNEdge]
nnEdgeListFromString input =
  case parseCSV input of
    Left _ -> []
    Right list -> map nnEdgeFromList list


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
