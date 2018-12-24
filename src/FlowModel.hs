module FlowModel(
    sustainability, efficiency, resilience, alpha
  ) where

import SimpleGraph(
       SimpleGraph,SGEdge
      , sourceNode, targetNode
      , flowOnEdge, outFlow, inFlow, totalFlow)

import Data.Graph.Inductive.Graph (labEdges)

sustainability :: SimpleGraph -> Float
sustainability graph =
    let
        a = alpha graph
        aa = power a 1.288
    in
        -1.844 * aa * (logBase 2 aa)



efficiency :: SimpleGraph -> Float
efficiency graph =
    let
        totalFlow_ =
            totalFlow graph

        edges =
            labEdges graph
    in
        sum(map (efficiencyOfEdge totalFlow_ graph) edges)

resilience :: SimpleGraph -> Float
resilience graph =
    let
        totalFlow_ =
            totalFlow graph

        edges =
            labEdges graph
    in
        sum(map (resilienceOfEdge totalFlow_ graph) edges)


alpha :: SimpleGraph -> Float
alpha graph =
    let
        ratio =
            1 + ((resilience graph) / (efficiency graph))
    in
        1 / ratio


log2 :: Float -> Float
log2 x = (log x)/(log 2)

power :: Float -> Float -> Float
power a x = exp(x * (log a))

efficiencyOfEdge :: Float -> SimpleGraph -> SGEdge -> Float
efficiencyOfEdge totalFlow_ graph edge =
  let
    ef = flowOnEdge edge
    denominator = (outFlow graph (sourceNode edge)) * (inFlow graph (targetNode edge))
    numerator = ef * totalFlow_
    logRatio = log2 (numerator/denominator)
  in
    ef * logRatio

resilienceOfEdge :: Float -> SimpleGraph -> SGEdge -> Float
resilienceOfEdge totalFlow_ graph edge =
    let
      ef = flowOnEdge edge
      denominator = (outFlow graph (sourceNode edge)) * (inFlow graph (targetNode edge))
      numerator = ef * ef
      logRatio = log2 (numerator/denominator)
    in
      ef * logRatio
