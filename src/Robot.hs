module Robot ( readLDM
             , readLCR
             , run
             )where

import Control.Monad.State
import Parsing 
import Data.Either

type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)

sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }


data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

{-3-Parser para Element-}

--Definindo funções auxialiares
stringIntoElement :: [Char] -> Element
stringIntoElement "Empty" = Empty
stringIntoElement "Entry" = Entry
stringIntoElement "Wall" = Wall
stringIntoElement "Rock" = Rock
stringIntoElement "Earth" = Earth

intIntoElement :: Int -> Element
intIntoElement i = Material i

pElement :: Parser Char Element
pElement = pElementNum <|> pElementText
    where 
      pElementNum = intIntoElement <$> natural1
      pElementText = stringIntoElement <$> ((token "Empty") <|>  (token "Entry") <|> (token "Wall") <|>  (token "Rock") <|> (token "Earth"))                                                

type Line = [Element]

data Mine = Mine {
              linhas    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

{-7-Inatncia de Show para o tipo Mine-}
instance Show Mine where
  show m = showElements (elements m)
    where 
      showElements [] = "\n"
      showElements (l:ls) = (showLine l) ++ (showElements ls)
         where 
          showLine [] = "\n"
          showLine (e:es) = (show e) ++ (showLine es)
        
showMineBreakLines :: Mine -> IO()
showMineBreakLines m = putStrLn (show m) 


{- 1 - instancia da classe Show para o tipo Robot-}
instance Show Robot where
    show r = "Energy:" ++show (energy r) ++ "\\nPosition" ++ show (position r) ++ "\\nCollect:"++ show (collected r)

{- 2 - instancia de Show para o tipo Element-}
instance Show Element where
    show elem = case elem of
                           Empty -> show ' '
                           Entry -> show 'E'
                           Wall -> show '%'
                           Earth -> show '.'
                           Rock -> show '*'
                           Material q -> case q of
                                                 100 ->  show ':'
                                                 150 -> show ';'
                                                 q -> show '$'
                           


{-4-Função que valida uma Mina-}

getEntrances :: Mine -> [Point] 
getEntrances m = (getEntrancesVerticalLeft (elements m) 0) ++ (getEntrancesVerticalRight (elements m) 0) ++ (getEntrancesUp ((elements m)!!0) 0) ++ (getEntrancesDown ((elements m)!!((linhas m)-1)) 0 (columns m))
  where
    getEntrancesVerticalLeft [] _ = []
    getEntrancesVerticalLeft (l:ls) n 
                           |l!!0 == Entry = (n,0) : (getEntrancesVerticalLeft ls) (n+1)
                           |otherwise = (getEntrancesVerticalLeft ls) (n+1)
    getEntrancesVerticalRight [] _ = []
    getEntrancesVerticalRight (l:ls) n
                          |l!!(tam-1) == Entry = (n,tam-1) : (getEntrancesVerticalRight ls) (n+1)
                          |otherwise = (getEntrancesVerticalRight ls) (n+1)
                              where tam = length l
    getEntrancesUp [] _ = []                                                    
    getEntrancesUp (l:ls) n
                          |l==Entry = (0,n) : (getEntrancesUp ls (n+1))
                          |otherwise = getEntrancesUp ls (n+1)   
    getEntrancesDown [] _ _ = []                                                    
    getEntrancesDown (l:ls) n tam
                          |l==Entry = (tam-1,n) : (getEntrancesDown ls (n+1) tam)
                          |otherwise = getEntrancesDown ls (n+1) tam   

verifyLength::Mine -> Bool
verifyLength m = let
                    elems = elements m
                    l = linhas m
                    c = columns m
                  in
                    l == (length elems) && c == (length (head elems))
             
validMine :: Mine -> Bool
validMine m = (verifyLength m) && ((length (getEntrances m)) > 0)


{-5-Valor do tipo Mine correspondente a LDM-}
exampleMine :: Mine 
exampleMine = Mine{
  linhas = 15,columns = 15,
  elements = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
              [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Wall],
              [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],
              [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Rock,Rock,Rock,Earth,Wall],
              [Wall,Earth,Material 50,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],
              [Wall,Earth,Earth,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
              [Wall,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
              [Wall,Earth,Material 100,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
              [Wall,Earth,Earth,Empty,Earth,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Wall],
              [Wall,Earth,Earth,Rock,Earth,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Earth,Wall],
              [Wall,Earth,Earth,Earth,Earth,Empty ,Earth,Earth,Empty,Earth,Material 150,Material 150,Earth,Earth,Wall],
              [Wall,Earth,Rock,Earth,Earth,Empty ,Earth,Earth,Earth,Material 150,Material 150,Earth,Earth,Rock,Wall],
              [Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Material 1,Wall],
              [Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Empty,Empty,Empty,Earth,Wall],
              [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Entry,Wall]]           
}

{-6-Parser para o tipo Mine-}

pLine :: Parser Char Line
pLine = greedy1 (charToElement <$> (symbol 'E' <|> symbol ' ' <|> symbol '%' <|> symbol '.' <|> symbol '*' <|> symbol ':' <|> symbol ';' <|> symbol '$'))
    where
        charToElement e = case e of
            'E' -> Entry
            ' ' -> Empty
            '%' -> Wall
            '.' -> Earth
            '*' -> Rock
            ':' -> Material 100
            ';' -> Material 150
            '$' -> Material 1


pMine :: Parser Char Mine
pMine = Parser (\inp -> let r = runParser (elementsToMine <$> listOf1 pLine (symbol '\n')) inp in
                          case r of
                            []->[]
                            ((a,_):_)->case validMine a of 
                                    True -> r
                                    False -> [])
   where elementsToMine l = Mine{linhas = length l,columns = length (l!!0),elements = l}

                                                   

data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

{-8-Parser para o tipo Instrução-}
pInstr :: Parser Char Instr
pInstr = charToInstr <$> ((symbol 'L') <|> (symbol 'R') <|>  (symbol 'U') <|> (symbol 'D') <|>  (symbol 'C') <|> (symbol 'S'))
  where 
    charToInstr c = case c of 
      'L' -> L
      'R' -> R
      'U' -> U
      'D' -> D 
      'C' -> C
      'S' -> S 

{-Parser para um programa LCR-}
pProgram :: Parser Char [Instr]
pProgram = Parser (\inp -> let r = runParser (greedy1 pInstr) inp in
                                case r of 
                                  []->[]
                                  ((a,_):_)->case validProgram a of
                                                True -> r
                                                False -> [])
  where 
    validProgram p 
                |(isEntrance (head p)) && (isEntrance (p!!(length p - 1))) = True
                |otherwise = False
          where isEntrance i 
                          |i == C || i == S = False
                          |otherwise = True
{-a função validProgram verifica se a primeira e ultima instrucao são de locomoção (entrar e sair da Mina)-}


type Conf = (Robot, Mine)

type ConfM a = State Conf a

{-10-}
current :: ConfM Point {-State (Robot,Mine) Point-}
current = state (\(r,m)->(position r,(r,m)))
            

mine :: ConfM Mine
mine = state (\(r,m)->(m,(r,m)))


enoughEnergy :: Int -> ConfM Bool
enoughEnergy v = isMore <$> state (\(r,m)->(energy r,(r,m)))
  where 
    isMore r = r > v

incEnergy :: ConfM ()
incEnergy = do
              (r,m) <- get
              put (r{energy = (energy r) + 1},m)
              return ()

{-11-}

validEnergy ::(Robot,Mine) -> Int -> Bool
validEnergy (r,b) v = fst (runState (enoughEnergy v) (r,b)) 
                      

validMove :: Instr -> (Robot,Mine) -> Bool
validMove i (r,b) = let 
                       mina = fst(runState mine (r,b))
                       (x,y) = fst (runState current (r,b)) 
                       e = validEnergy (r,b) 1
                    in 
                      if(i==L) then not(validArea mina (x,y-1) [Wall] && e)
                      else if(i==R) then not (validArea mina (x,y+1) [Wall] && e)
                      else if (i==U) then not (validArea mina (x-1,y) [Wall] && e)
                      else not (validArea mina (x+1,y) [Wall] && e)
                        

validArea :: Mine -> Point -> [Element] -> Bool
validArea _ _ [] = False 
validArea m (x,y) (e:es) = (posit == e) || validArea m (x,y) es
             where posit =  ((elements m)!!x)!!y      

             

validCollect :: (Robot,Mine) -> Bool
validCollect (r,m) = let 
                        (x,y) = position r
                        materials = [Material 100,Material 150,Material 50,Material 1]
                      in
                        (validEnergy (r,m) 10) && ( 
                        (validArea m (x+1,y) materials) ||
                        (validArea m (x-1,y) materials) ||
                        (validArea m (x,y+1) materials) ||
                        (validArea m (x,y-1) materials) )
    

valid :: Instr -> ConfM Bool
valid C = state (\(r,m) -> (validCollect (r,m),(r,m)))
valid S = state (\(r,m) -> (True,(r,m)))
valid anothers = state (\(r,m) -> (validMove anothers (r,m),(r,m)))
                                        
{-12-}

updateMine :: Instr -> ConfM ()
updateMine i = do
                n <- get
                case fst(runState (valid i) n) of 
                  False -> incEnergy
                  True -> changeState i
                            
                           
changeState :: Instr -> ConfM ()
changeState i = case i of
                    S -> incEnergy
                    C -> do
                          (r,m) <- get
                          put ((r{energy = (energy r)-10,position = position r,collected = (collected r) + 1},removeMaterial m (position r) ))
                          return ()
                    U -> do
                          (r,m) <- get
                          put ((r {energy = (energy r)-1,position = incPosition U (position r),collected = (collected r)},m))
                          return ()
                    D -> do
                          (r,m) <- get
                          put ((r {energy = (energy r)-1,position = incPosition D (position r),collected = (collected r)},m))
                          return ()
                    L -> do
                          (r,m) <- get
                          put ((r {energy = (energy r)-1,position = incPosition L (position r),collected = (collected r)},m))
                          return ()
                    R -> do
                          (r,m) <- get
                          put ((r {energy = (energy r)-1,position = incPosition R (position r),collected = (collected r)},m))
                          return ()


incPosition:: Instr -> Point -> Point
incPosition i (x,y) = case i of
                      U -> (x-1,y)
                      D -> (x+1,y)
                      L -> (x,y-1)
                      R -> (x,y+1)


removeMaterial :: Mine -> Point -> Mine
removeMaterial m (x,y) 
                        | elem (((elements m)!!(x-1))!!y) materials = setEmpty m (x-1,y)
                        | elem (((elements m)!!(x+1))!!y) materials = setEmpty m  (x+1,y)
                        | elem (((elements m)!!(x))!!(y+1)) materials = setEmpty m (x,y+1) 
                        |otherwise = setEmpty m (x,y-1)
                        
                             where
                                materials = [Material 1,Material 50,Material 100,Material 150]
                              
setEmpty::Mine->Point->Mine
setEmpty mine (x,y) = let 
                      m = elements mine
                      (c,_:cs)= splitAt y (m!!x) 
                      (l,_:ls) = splitAt x m  
                      newLine = c++Empty:cs
                    in
                      Mine{linhas= (linhas mine) ,columns = (columns mine),elements = (l++newLine:ls) } 

{-13-}

exec :: Instr -> ConfM ()
exec i = do
           n <- get 
           r <- updateMine i
           return ()

{-14-}
initRobot :: Mine -> Robot
initRobot m 
          |not (validMine m) = error "Mina invalida"
          |otherwise = Robot{energy = 100,position = head (getEntrances m),collected = 0}
              
                
{-15-}     
                               

run :: [Instr] -> Mine -> Mine
run [] m = m
run (i:is) m = snd (snd (runState (make (i:is)) (initRobot m,m)))

make :: [Instr] -> ConfM ()
make [] = state (\(r,m)->((),(r,m)))
make (i:is) = do
                s <- get
                v <- exec i
                make is 
                return ()
                           
                              
{-16-}                                   

readLDM :: String -> IO (Either String Mine)
readLDM name = do
                n <- readFile ("app/" ++ name)
                case (runParser pMine n) of
                  [] -> return (Left "Parser error on input of the mine!")
                  ((a,_):_) -> return (Right a)

{-17-}
readLCR :: String -> IO (Either String [Instr])
readLCR name = do
                n <- readFile ("app/" ++ name)
                case (runParser pProgram n) of 
                  [] -> return (Left "Parser error on input of the program")
                  ((a,_):_) -> return (Right a)