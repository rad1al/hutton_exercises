{- Chapter 13.11 Exercises -}

{- 1. 

Define a parser comment :: Parser () for ordinary Haskell comments that begin with the
symbol -- and extend to the end of the current line, which is represented by the control
character '\n'.

-}

{- 2. 
                                                                        
                  expr                            expr                  
                ┌─ │  ─┐                        ┌─ │  ─┐                
              ┌─┘  │   └─┐                    ┌─┘  │   └─┐              
            ┌─┘    │     └─┐                ┌─┘    │     └─┐            
          ┌─┘      │       └─┐            ┌─┘      │       └─┐          
          ▼        ▼         └▼           ▼        ▼         └▼         
        expr       +         expr        expr      +         expr       
       ┌─┐─┐                  │           │                 ┌─┐─┐       
     ┌─┘ │ └─┐                │           │               ┌─┘ │ └─┐     
   ┌─┘   │   └─┐              │           │             ┌─┘   │   └─┐   
  ▼┘     ▼     ▼              ▼           ▼            ▼┘     ▼     ▼   
 expr    +    expr           term        term         expr    +    expr 
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
 term         term          factor      factor       term          term 
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
factor       factor          nat         nat         factor       factor
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  │            │              │           │           │             │   
  ▼            ▼              ▼           ▼           ▼             ▼   
 nat          nat             4           2          nat           nat  
  │            │                                      │             │   
  │            │                                      │             │   
  │            │                                      │             │   
  ▼            ▼                                      ▼             ▼   
  2            3                                      3             4   
                                                                        

-}

{- 3. -}

{- 4. -}

{- 5. -}

{- 6. -}

{- 7. -}

{- 8. -}



