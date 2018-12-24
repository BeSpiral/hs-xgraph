module FlowModel(efficiencyOfEdge) where

import SimpleGraph(
       SimpleGraph,SGEdge
      , sourceNode, targetNode
      , flowOnEdge, outFlow)


log2 :: Float -> Float
log2 x = (log x)/(log 2)

efficiencyOfEdge :: Float -> SimpleGraph -> SGEdge -> Float
efficiencyOfEdge totalFlow_ graph edge =
  let
    ef = flowOnEdge edge
    denominator = (outFlow graph (sourceNode edge)) * (outFlow graph (targetNode edge))
    numerator = ef * totalFlow_
    logRatio = log2 (numerator/denominator)
  in
    ef * logRatio

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
