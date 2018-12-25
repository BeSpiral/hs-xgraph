# hs-xgraph

Experimental Haskell graph package -- to mirror the corresponding Elm package, `sustainability_index_core`.

## Manual tests

The module `Test.hs` defines two sets of test data, the strings `ex1` and `ex2`.  Let's work with `ex2`:

```
*ghci > g = graphFromCSV ex2

*ghci > totalFlow g
456.19998

*ghci > efficiency g
621.70776

*ghci > resilience g
993.88025

*ghci > sustainability g
0.95643777

*ghci > alpha g
0.38481823
```
The results agree with those obtained using the Elm implementatin of this library at `sustainability_index_core`.

## References

[Monday Morning Haskell: Graphing it out](https://mmhaskell.com/blog/2017/6/21/graphing-it-out)

[Data.Graph.Inductive.Graph](http://hackage.haskell.org/package/fgl-5.7.0.1/docs/Data-Graph-Inductive-Graph.html)
