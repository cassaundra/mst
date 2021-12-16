module Lib
  ( prims,
    kruskals,
  )
where

import           Control.Monad     (join)
import           Data.Bifunctor    (Bifunctor (bimap))
import           Data.Foldable     (find, fold)
import           Data.Function     (on)
import qualified Data.Graph.Types  as G
import qualified Data.Graph.UGraph as G
import           Data.Hashable
import qualified Data.List         as L
import           Data.Maybe        (fromJust)
import qualified Data.Set          as S

-- XXX hack in order to let us use a set of graphs
-- will remove soon
instance (Ord v, Monoid v, Eq e) => Ord (G.UGraph v e) where
  compare = compare `on` fold . G.vertices

-- | Find an MST using Prim's algorithm.
-- The graph /must/ be connected in order for this function to work.
prims :: (Hashable v, Eq v, Ord e) => G.UGraph v e -> G.UGraph v e
prims graph = prims' (G.insertVertex firstVertex G.empty) (bestEdges graph)
  where
    -- The first vertex, chosen arbitrarily.
    firstVertex = head $ G.vertices graph

    -- Inner prims algorithm operating on a queue.
    prims' graph' queue =
      if G.order graph' < G.order graph -- do we have all the vertices yet?
        then prims' (G.insertEdge best graph') (L.delete best queue)
        else graph'
      where
        -- Find the next best edge.
        best = fromJust $ find (wouldExpand graph' . G.toPair) queue

-- | Find an MST using Kruskal's algorithm.
-- The graph /must/ be connected in order for this function to work.
kruskals :: (Ord e, Ord v, Monoid v, Hashable v) => G.UGraph v e -> G.UGraph v e
kruskals graph = kruskals' (bestEdges graph) (graphToVertexSet graph)
  where
    kruskals' [] gs = head $ S.toAscList gs
    kruskals' (edge : es) gs =
      let (uGraph, vGraph) = join bimap (findGraph gs) $ G.toPair edge
       in kruskals' es $
            if uGraph /= vGraph
              then -- join the two graphs
                S.insert (joinGraphs edge uGraph vGraph)
                  . S.delete uGraph
                  . S.delete vGraph
                  $ gs
              else gs

    -- Map a graph to a set of graphs with a single vertex.
    graphToVertexSet = S.fromList . map (`G.insertVertex` G.empty) . G.vertices

    -- Join two graphs connected by a given edge.
    joinGraphs edge = G.union . G.insertEdge edge

    -- Find the first graph that has an element.
    findGraph gs v = fromJust $ find (`G.containsVertex` v) gs

-- | Get the edges of a graph sorted by its weights.
bestEdges :: (Hashable v, Eq v, Ord e) => G.UGraph v e -> [G.Edge v e]
bestEdges = L.sortOn G.attribute . G.edges

-- | Check either adding an edge would expand the order of a connected graph.
-- wouldExpand :: G.UGraph v e -> (v, v) -> Bool
wouldExpand :: (G.Graph g, Hashable v, Eq v) => g v e -> (v, v) -> Bool
wouldExpand graph (a, b) = G.containsVertex graph a /= G.containsVertex graph b
