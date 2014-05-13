-- Example Petri Net Unfolding
-- ===========================
-- Build a sample Petri Net (representing a MUTEX like structure) and unfold
-- it to form the state transition graph.
--
-- ( ) - Place
--  *  - Token
-- [=] - Transition
-- 
--  |       |
--  v       v
-- (*)     (*)  <- Start places
--  |       |
--  v       v   
-- [=]     [=]  <- Request 1, 2
--  |       |
--  v       v   
-- ( )     ( )
--  |       |
--  v       v   
-- [=]     [=]  <- Accept 1, 2
--  | \   / |
--  |  (*)  |   <- Mutex place
--  v /   \ v   
--  | ^   ^ |
--  |/     \|
-- [=]     [=]  <- Finished
--  |       |
--  v       v
--  |       |
--
-- Usage
-- -----
-- Run `ghci` then::
--
--  :l example
--  test
--
-- Will print out a representation of the STG for the above Petri Net

import qualified Data.Map as Map
import qualified Data.Set as Set
import Petri
import qualified PetriMarking as Marking
import StateTransition

test = do let p11:p12:p13:p14:_ = map Petri.Place $ map ("P1"++) $ map show [1..]
	  let p21:p22:p23:p24:_ = map Petri.Place $ map ("P2"++) $ map show [1..]
	  let tr1r = Petri.Transition "r1" Petri.R
	  let tr1f = Petri.Transition "r1" Petri.F
	  let tr2r = Petri.Transition "r2" Petri.R
	  let tr2f = Petri.Transition "r2" Petri.F
	  let ta1r = Petri.Transition "a1" Petri.R
	  let ta1f = Petri.Transition "a1" Petri.F
	  let ta2r = Petri.Transition "a2" Petri.R
	  let ta2f = Petri.Transition "a2" Petri.F
	  let pmutex = Petri.Place "pMUTEX"
	  let flow = Set.fromList [ (p11,tr1r), (tr1r,p12), (p12,ta1r), (ta1r,p13), (p13,tr1f), (tr1f,p14), (p14,ta1f), (pmutex,ta1r), (ta1f,pmutex)
				  , (p21,tr2r), (tr2r,p22), (p22,ta2r), (ta2r,p23), (p23,tr2f), (tr2f,p24), (p24,ta2f), (pmutex,ta2r), (ta2f,pmutex)
				  , (ta1f, p11), (ta2f, p21)
				  ]
	  let ns = Set.fromList [ tr1r, tr1f, tr2r, tr2f, ta1r, ta1f, ta2r, ta2f, p11, p12, p13, p14, p21, p22, p23, p24, pmutex ]
	  let pn1 = Petri.Net ns flow
	  let m1 = Map.fromList [ (p11, 1), (p21, 1), (pmutex, 1) ]
	  print $ StateTransition.stateGraph pn1 m1
