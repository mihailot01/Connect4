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

kraj i j y igrac
  | ispod>=4 || ld>=4 || d1>=4 || d2>=4 = True
  | otherwise = False
  where
  ispod=izbroji i j 1 0 y (igrac+1)
  ld= izbroji i j 0 (-1) y (igrac+1) + izbroji i j 0 1 y (igrac+1) - 1
  d1= (izbroji i j 1 1 y (igrac+1)) + (izbroji i j (-1) (-1) y (igrac+1)) - 1
  d2= (izbroji i j 1 (-1) y (igrac+1)) + (izbroji i j (-1) 1 y (igrac+1)) - 1

izbroji2 i j di dj t ind
  | ind == 4 = 0
  | (t !! i) !! j == 1 = -1 + izbroji2 (i+di) (j+dj) di dj t (ind+1)
  | (t !! i) !! j == 2 = 1 + izbroji2 (i+di) (j+dj) di dj t (ind+1)
  | otherwise = izbroji2 (i+di) (j+dj) di dj t (ind+1)
  
score t = ispod+desno+d1+d2
   where
   ispod = foldl1 (+) [izbroji2 i j 1 0 t 0 | i<-[0..2], j<-[0..6]]
   desno = foldl1 (+) [izbroji2 i j 0 1 t 0 | i<-[0..5], j<-[0..3]]
   d1 = foldl1 (+) [izbroji2 i j 1 1 t 0 | i<-[0..2], j<-[0..3]]
   d2 = foldl1 (+) [izbroji2 i j 1 (-1) t 0 | i<-[0..2], j<-[3..6]]

minimize j depth t
  | depth == 0 = (3,score t)
  | j==7 = (1, 10001) 
  | y==[[-1]] = ostalo
  | kraj i j y 0 = (j,-10000)
  | (snd tek) < (snd ostalo) = (j,snd tek)
  | otherwise = ostalo
  where
  ostalo=minimize (j+1) depth t
  y=ubaci 1 j t
  i= nadjii j y
  tek=maximize 0 (depth-1) y
  
maximize j depth t
  | depth == 0 = (3,score t)
  | j==7 = (1,-10001)
  | y==[[-1]] = ostalo
  | kraj i j y 1 = (j,10000)
  | snd tek > snd ostalo = (j,snd tek)
  | otherwise = ostalo
  where
  ostalo=maximize (j+1) depth t
  y=ubaci 2 j t
  i= nadjii j y
  tek=minimize 0 (depth-1) y

potez igrac x depth = do
  if igrac==0 then do
    putStrLn ( "Na potezu je igrac")
    ind <- getLine
    if (ind/="0" && ind/="1" && ind/="2" && ind/="3" && ind/="4" && ind/="5" && ind/="6") then do 
     putStrLn ("!! Neispravan ulaz !!")
     potez igrac x depth
    else do
     return (ind) --putStrLn ( "Na potezu je igrac")  
  else do
    putStrLn ( "SRKI je na potezu")
    --threadDelay 1000000
    return (show (fst (maximize 0 depth x)))

akcija igrac x turn depth = do
  putStrLn (ispisi x)
  ind <- potez igrac x depth
  let j=read ind :: Int
  let y = ubaci (igrac+1) j x
  if y== [[-1]]
    then do 
         putStrLn "!! Nemoguc potez !!"
         akcija igrac x (turn+1) depth
    else do
         let i = nadjii j y
         if (kraj i j y igrac) then do
          putStrLn(ispisi y)
          if igrac==0  then putStrLn("IGRAC JE POBEDIO")
          else putStrLn("SRKILE JE POBEDIO")
         else if turn == 41 then do
           putStrLn(ispisi y)
           putStrLn("NERESENO") 
         else do
           if turn==14 then
            akcija (mod (igrac+1) 2) y (turn+1) (depth+1)
           else 
            akcija (mod (igrac+1) 2) y (turn+1) depth
play = do
  let x = napraviTablu
  akcija 0 x 0 4
  putStrLn("Kraj igre")