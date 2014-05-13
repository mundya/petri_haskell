-- Improved Petri Net Model
-- ========================
-- Andrew Mundy <andrew.mundy@ieee.org> 2013
-- University of Manchester

module PetriMarking
( Marking
, enabled
, fireTransition
, compressedMarking
) where

-- Imports:
import qualified Data.Map as Map
import qualified Data.Set as Set
import Petri

-- Data Structures
-- ===============
-- Marking Representation
-- ----------------------
type Marking = Map.Map Petri.Node Integer	-- Marking maps places to integral numbers of tokens
						-- Give support for multiple tokens per place

-- Marking Manipulation
-- --------------------
isEnabledVal :: ( Num a, Ord a ) => Maybe a -> Bool	-- Resolve a mapped marking into a bool representing whether a token is present
isEnabledVal Nothing = False		-- If there is no marking, assume there are no tokens
isEnabledVal a = a > Just 0		-- Otherwise, are the number of tokens > 0?

isEnabled :: Petri.Net -> Marking -> Petri.Node -> Bool	-- Is the given transition enabled for the given
							-- Petri net and marking?
		-- AND [ for all nodes in the preset, determine whether they have non-zero token count ]
isEnabled n m t = if length p == 0 then False else and p
			where p = map ( isEnabledVal ) $ map (\p -> Map.lookup p m) $ Set.toList $ preset n t

enabled :: Petri.Net -> Marking -> Set.Set Petri.Node	-- All enabled transitions for this net and marking
enabled n m = Set.filter (isEnabled n m ) $ Petri.transitions n

markingResolve :: Maybe Integer -> Integer
markingResolve Nothing = 0
markingResolve ( Just a ) = a

markingMod :: (Integer -> Integer) -> Marking -> Petri.Node -> ( Petri.Node, Integer )
markingMod f m n = (n, v) where v = f $ markingResolve $ Map.lookup n m 

markingIncrement :: Marking -> Petri.Node -> ( Petri.Node, Integer )
markingIncrement = markingMod (+1)

markingDecrement :: Marking -> Petri.Node -> ( Petri.Node, Integer )
markingDecrement = markingMod (+(-1))

markingRetain :: Marking -> Petri.Node -> ( Petri.Node, Integer )
markingRetain = markingMod (+0)

compressedMarking :: Marking -> Marking
compressedMarking m = Map.fromList $ filter (\(k, v) -> v > 0) $ Map.toList m

fireTransition :: Petri.Net -> Marking -> Petri.Node -> Marking	-- Generate a new marking by firing a transition
fireTransition n m t = if and [ Petri.isTransition t, isEnabled n m t ] then 
				compressedMarking $
				Map.fromList $ [ markingDecrement m p | p <- pres ]
					    ++ [ markingIncrement m p | p <- posts ]
					    ++ [ markingRetain m p | p <- other ]
		       else m
		       where
		       		pres  = Set.toList $ Petri.preset n t
				posts = Set.toList $ Petri.postset n t
				other = Set.toList $ Set.difference ( places n ) $ Set.union ( preset n t ) ( postset n t )
