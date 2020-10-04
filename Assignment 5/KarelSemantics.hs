module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) w r = c == (getFacing r)                  -- am I facing in the c direction
test (Clear d) w (p, c, i) = isClear (neighbor (cardTurn d c ) p) w
test Beeper w (p, c, i) = hasBeeper p w
test Empty w (p,c,i) = (i==0)


-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w (p,c,i)  
            | test (Clear Front) w (p,c,i) == True =  OK w ((neighbor c p), c,i) --OK w (setPos (neighbor c p) (p,c,i)) --
            | test (Clear Front) w (p,c,i) == False = Error ("Blocked at: "++show (neighbor c p))-- Error ("Cannot move " ++ show (getFacing (p,c,i)) )

stmt PutBeeper _ w (p,c,i)
            | i == 0 = Error ("No beeper to put.")
            | i > 0 = OK (incBeeper p w) (decBag (p,c,i))

stmt (Turn d) _ w (p,c,i) = OK w (p, (cardTurn d c), i)

stmt (Block []) d w r = OK w r
-- stmt (Block [x]) d w r = stmt x d w r
stmt (Block (x:xs)) d w (p,c,i) = case (stmt x d w (p,c,i)) of
                                Done (np,nc,ni) -> Done (np,nc,ni) 
                                Error s -> Error s 
                                OK nw (np,nc,ni) -> stmt (Block xs) d nw (np,nc,ni)

stmt (If t s1 s2) d w r  = case (test t w r) of
                            True -> stmt s1 d w r 
                            False -> stmt s2 d w r
            
stmt (Call m) d w r = case lookup m d of
                        Nothing -> Error ("Undefined macro: " ++ m) 
                        Just st -> stmt st d w r

stmt (Iterate n s) d w r 
            | n == 0 = OK w r 
         --   | n > 0 = stmt (Iterate (n-1) s) d w r
            | n > 0 = case stmt s d w r of
                Done nr -> Done nr 
                Error ns -> Error ns 
                OK nw nr -> stmt (Iterate (n-1) s) d nw nr

stmt (While t s) d w r 
                | test t w r == False = OK w r 
                | test t w r == True = case stmt s d w r of
                    Done nr -> Done nr 
                    Error ns -> Error ns 
                    OK nw nr -> stmt (While t s) d nw nr
                    
            

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
