-- Author: Joshua Strozzi
module HW4 where

    import MiniMiniLogo
    import Render
    
    
    --
    -- * Semantics of MiniMiniLogo
    --
    
    -- NOTE:
    --  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
    --    functions for generating MiniMiniLogo programs. It contains the type
    --    definitions for Mode, Cmd, and Prog.
    --  * Render.hs contains code for rendering the output of a MiniMiniLogo
    --    program in HTML5. It contains the types definitions for Point and Line.
    
    -- | A type to represent the current state of the pen.
    type State = (Mode,Point)
    
    -- | The initial state of the pen.
    start :: State
    start = (Up,(0,0))
    
    -- | A function that renders the image to HTML. Only works after you have
    --   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
    --   produce an HTML file named MiniMiniLogo.html, which you can load in
    --   your browswer to view the rendered image.
    draw :: Prog -> IO ()
    draw p = let (_,ls) = prog p start in toHTML ls
    
    
    -- Semantic domains:
    --   * Cmd:  State -> (State, Maybe Line)
    --   * Prog: State -> (State, [Line])
    
    
    -- | Semantic function for Cmd. 
    -- | Takes command and does classic pattern matching to return
    -- | the state and any potential line drawn
    --   
    --   >>> cmd (Pen Down) (Up,(2,3))
    --   ((Down,(2,3)),Nothing)
    --
    --   >>> cmd (Pen Up) (Down,(2,3))
    --   ((Up,(2,3)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Up,(2,3))
    --   ((Up,(4,5)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Down,(2,3))
    --   ((Down,(4,5)),Just ((2,3),(4,5)))
    --
    cmd :: Cmd -> State -> (State, Maybe Line)
    cmd (Pen m) (p, (x, y)) = ((m, (x,y)), Nothing)
    cmd (Move x2 y2) (p, (x1, y1)) 
        | p == Up = ((p, (x2, y2)), Nothing)
        | p == Down = ((p, (x2, y2)), Just ((x1, y1), (x2, y2)))

    
    
    -- | Semantic function for Prog.
    -- | Look at prog : http://web.engr.oregonstate.edu/~walkiner/teaching/cs381-wi19/code/StackLang.hs
    -- | Look at sem : http://web.engr.oregonstate.edu/~walkiner/teaching/cs381-wi19/code/IntBool.hs 

    --
    --   >>> prog (nix 10 10 5 7) start
    --   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
    --
    --   >>> prog (steps 2 0 0) start
    --   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
    prog :: Prog -> State -> (State, [Line])
    prog [] st = (st, [])
    prog (x:xs) st = case (cmd x st) of          -- case statement using the cmd function
                        (nst, Nothing) -> prog xs nst       -- if cmd returns No line, recursively call prog with the new state and the rest of the list
                        (nst, Just l) -> let (fs, lines) = prog xs nst 
                            in (fs, (l:lines))
                            -- build a new tuple here, with fs as the final state and adding line to lines
    
    
    line:: Point -> Point -> Prog 
    line (x1, y1) (x2, y2) = [Pen Up, Move x1 y1, Pen Down, Move x2 y2]

    santa1::Prog 
    santa1 = [Pen Up, Move 26 25, Pen Down, Move 24 34, Move 18 38, Move 7 33, Move 4 25, Move 7 24, Move 9 28, Move 14 33, 
                Pen Up, Move 6 18, Pen Down, Move 4 19, Move 4 21, Move 7 24, Move 12 26, Move 22 26, Move 26 25, Move 28 23, Move 28 20, Move 26 19, Move 24 20, Move 22 21, Move 11 21, Move 9 20, Move 6 18,
                Pen Up, Move 4 25, Pen Down, Move 1 22, Move 1 19, Move 5 17, Move 6 17]

    santa2::Prog 
    santa2= [ Pen Up, Move 6 18, Pen Down, Move 6 17, Move 6 8, Move 10 2, Move 16 1, Move 22 2, Move 26 8, Move 26 19, 
            Pen Up, Move 9 20, Pen Down, Move 9 14, Move 12 11,Move 15 12, Move 17 12, Move 20 11, Move 24 14, Move 24 20,
            Pen Up, Move 12 8, Pen Down, Move 14 8, Move 16 10, Move 18 8, Move 20 8,
            Pen Up, Move 14 8, Pen Down, Move 15 7, Move 17 7, Move 18 8]

    santa3::Prog 
    santa3=[Pen Up, Move 15 9, Pen Down, Move 16 8, Move 17 9,
            Pen Up, Move 15 15, Pen Down, Move 14 14, Move 14 13, Move 15 12, Move 17 12, Move 18 13, Move 18 14, Move 17 15,
            Pen Up, Move 11 21, Pen Down, Move 10 20, Move 10 18, Move 12 19, Move 15 19, Move 15 21,
            Pen Up, Move 17 21, Pen Down, Move 17 19, Move 20 19, Move 22 18, Move 22 20, Move 21 21, 
            Pen Up, Move 10 16, Pen Down, Move 12 18, Move 15 18]
    
    santa4::Prog
    santa4=[Pen Up, Move 17 18, Pen Down, Move 20 18, Move 22 16,
            Pen Up, Move 11 17, Pen Down, Move 12 16, Move 13 16, Move 14 17, Move 14 18,
            Pen Up, Move 18 18, Pen Down, Move 18 17, Move 19 16, Move 20 16, Move 21 17] 

    --
    -- * Extra credit
    --
    
    -- | This should be a MiniMiniLogo program that draws an amazing picture.
    --   Add as many helper functions as you want.
    amazing :: Prog
    amazing = santa1++santa2 ++santa3++santa4
