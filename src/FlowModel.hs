module FlowModel(foo) where

import SimpleGraph(SimpleGraph)

foo :: Int
foo = 27

-- efficiencyOfEdge :: Float -> SimplGraph 

-- efficiencyOfEdge : Float -> SimpleGraph -> Edge -> Float
-- efficiencyOfEdge totalFlow_ graph edge =
--     let
--         denominator =
--             (SG.outFlow edge.from graph) * (SG.inFlow edge.to graph)
--
--         numerator =
--             edge.label.flow * totalFlow_
--
--         logRatio =
--             (logBase 2) (numerator / denominator)
--     in
--         roundTo 3 (edge.label.flow * logRatio)
