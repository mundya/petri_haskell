-- Improved Petri Net Model
-- ========================
-- Andrew Mundy <andrew.mundy@ieee.org> 2013
-- University of Manchester

module Petri
( EdgeDirection ( R, F )
, Node ( Place, Transition )
, Net ( Net )
, Arc
, isPlace
, isTransition
, places
, transitions
, preset
, postset
) where

-- Imports:
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Data Structures
-- ===============
-- Petri Net Representation
-- ------------------------
data EdgeDirection = R | F deriving (Eq, Ord)	-- R for RISING edges and F for FALLING edges
data Node =  Place String			-- Places are named
	  |  Transition String EdgeDirection	-- Transitions are labelled and have a direction (R/F)
		deriving ( Eq, Ord )

type Arc = ( Node, Node )			-- Petri Arcs map nodes to nodes (either P->T or T->P)

-- Petri nets represent the entire net, and use sets internally
data Net = Net { nodes :: Set.Set Node, arcs :: Set.Set Arc } deriving ( Show )

instance Show EdgeDirection where
	show R = "+"
	show F = "-"

instance Show Node where
	show ( Place n ) = n
	show ( Transition n d ) = n ++ show d

-- Petri Net Management
-- --------------------
-- Is the given node a place?
isPlace :: Node -> Bool
isPlace = not . isTransition

-- Is the given node a transition?
isTransition :: Node -> Bool
isTransition a = case a of
			Transition _ _ -> True
			otherwise -> False
			
-- The places for a given petri net
places :: Net -> Set.Set Node	
places ( Net { nodes = n } ) = Set.filter ( isPlace ) n

-- The transitions for a given petri net
transitions :: Net -> Set.Set Node
transitions ( Net { nodes = n } ) = Set.filter ( isTransition ) n

-- Transition Functions
-- --
-- Filter and map to retrieve desired places
transet :: ( Arc -> Bool ) -> ( Arc -> Node ) -> Net -> Set.Set Node
transet g h ( Net { arcs = fs } ) = Set.map h $ Set.filter g fs

-- The preset for a given net and transition
preset :: Net -> Node -> Set.Set Node
preset n t = if isTransition t then transet (\(u, v) -> and [ isPlace u, v == t ]) (\(u, _) -> u ) n else Set.empty

-- The postset for a given net and transition
postset :: Net -> Node -> Set.Set Node
postset n t = if isTransition t then transet (\(u, v) -> and [ isPlace v, u == t ]) (\(_, v) -> v ) n else Set.empty
