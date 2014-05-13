-- Improved Petri Net Model
-- ========================
-- Andrew Mundy <andrew.mundy@ieee.org> 2013
-- University of Manchester

module StateTransition
( Tree ( Empty, Node )
, Graph ( Graph )
, stateTree
, stateGraph
) where

-- Imports:
import qualified Data.Map as Map
import qualified Data.Set as Set
import Petri
import qualified PetriMarking as Marking

-- Data Structures
-- ===============
-- State Transition Tree
-- ---------------------
type Transition = ( Petri.Node, Marking.Marking )
data Tree = Empty
	  | Node { marking :: Marking.Marking		-- The marking represented by this node
 		 , branches :: Map.Map Petri.Node Tree	-- Derived markings and the transitions that lead to them
		 } deriving ( Show )
type VisitedMarkings = Set.Set Marking.Marking

-- State Transition Graphs
-- -----------------------
type GraphTransition = ( Marking.Marking, Petri.Node, Marking.Marking )
data Graph = Graph { flow :: Set.Set GraphTransition
		   , states :: Set.Set Marking.Marking
		   } deriving Show

-- Subsequent marking generation
-- -----------------------------
nextMarkings :: Petri.Net -> Marking.Marking -> [ Transition ]
nextMarkings n m = map (\t -> (t, Marking.fireTransition n m t)) $ Set.toList $ Marking.enabled n m

-- State Transition Tree Generation
-- --------------------------------
stateTree :: Petri.Net -> Marking.Marking -> VisitedMarkings -> Tree
stateTree n m v = let m's = nextMarkings n m
	 	      v' = Set.insert m v -- Set.union v $ Set.fromList $ map (\(_, m') -> m') m's
		      br = Map.fromList $ [ (t, stateTree n m' v') | (t, m') <- m's ]
		  in if Set.member m v then
		      Node { marking = m, branches = Map.empty }
		  else
		      Node { marking = m, branches = br }

-- State Transition Graph Generation
-- ---------------------------------
stateGraphTransitions :: Tree -> Set.Set GraphTransition
stateGraphTransitions (Node {marking = m, branches = bs}) =
			let local = Set.fromList $ map (\(t, (Node { marking=m' })) -> (m, t, m')) $ Map.toList bs
			    branched = foldl Set.union Set.empty $ map (\(_, n) -> stateGraphTransitions n) $ Map.toList bs
			in
			    Set.union local branched

stateGraphStates :: Tree -> VisitedMarkings
stateGraphStates (Node {marking = m, branches = bs}) =
			let branched = foldl Set.union Set.empty $ map (\(_, n) -> stateGraphStates n) $ Map.toList bs
			in  Set.insert m branched

stateGraph :: Petri.Net -> Marking.Marking -> Graph
stateGraph n m = let
			t = stateTree n m Set.empty
			ts = stateGraphTransitions t
			ss = stateGraphStates t
		 in Graph { flow = ts, states = ss }
