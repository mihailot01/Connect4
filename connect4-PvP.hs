napraviTablu = [[0,0,0,0,0,0,0]| i<-[1..6]]

pretvori x 
    | x==0 = "  "
    | x==1 = " x"
    | x==2 = " o"
    | otherwise = ".i"

ispisiRed []=""
ispisiRed r = foldl1 (++) (map pretvori r) 
ispisi [] = "\\---------------/\n  0 1 2 3 4 5 6\n"
ispisi (h:t) = "|" ++ ispisiRed h ++ " |\n" ++ ispisi t  

ubaci2 br x i (h:t)
    | x==i = (br:t)
    | otherwise = (h:(ubaci2 br x (i+1) t))

ubaci br x (h:[])
   | h !! x == 0 = (ubaci2 br x 0 h):[]
ubaci br x (h1:h2:t)  -- = (h1:h2:t)
    | h1 !! x == 0 && h2 !! x /= 0 =  ((ubaci2 br x 0 h1):h2:t)
    | h1 !! x /=0 = [[-1]]
    | otherwise = (h1:ubaci br x (h2:t))

nadjii x (h:t)
    | h !! x /=0 = 0
    | otherwise = 1 + nadjii x t

izbroji i j di dj m igrac
    | i<0 || i>5 || j<0 || j>6 = 0
    | (m !! i) !! j == igrac = 1+ (izbroji (i+di) (j+dj) di dj m igrac)
    | otherwise = 0

akcija igrac x = do
  putStrLn (ispisi x)
  putStrLn ( "Na potezu je igrac "++ show (igrac+1) )  
  ind <- getLine
  let j=read ind :: Int
  let y = ubaci (igrac+1) j x
  if y== [[-1]]
    then do 
         putStrLn "!! Nemoguc potez !!"
         akcija igrac x
    else do
         let i = nadjii j y
         --putStrLn(show ((y !! i) !! j) )
         let ispod=izbroji i j 1 0 y (igrac+1)
         let ld= izbroji i j 0 (-1) y (igrac+1) + izbroji i j 0 1 y (igrac+1) - 1
         let d1= (izbroji i j 1 1 y (igrac+1)) + (izbroji i j (-1) (-1) y (igrac+1)) - 1
         let d2= (izbroji i j 1 (-1) y (igrac+1)) + (izbroji i j (-1) 1 y (igrac+1)) - 1
         --putStrLn (show ispod)
         if (ispod==4 || ld==4 || d1==4 || d2==4) then do
          putStrLn(ispisi y)
          putStrLn("IGRAC " ++ show (igrac+1) ++ " JE POBEDIO")
         else do
           akcija (mod (igrac+1) 2) y

play = do
  let x = napraviTablu
  akcija 0 x
  putStrLn("Kraj igre")