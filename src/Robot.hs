module Robot ( readLDM
             , readLCR
             , run
             )where

import Control.Monad.State
import Parsing 


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
contLineBorder :: Line -> Int
contLineBorder [] = 0
contLineBorder (x:xs) 
                    |x == Entry = 1 + contLineBorder xs
                    |otherwise = contLineBorder xs

contColumBorder :: [Line] -> Int
contColumBorder [] = 0
contColumBorder (x:xs)
                     |((x!!0) == Entry) && (x!!(tam-1) == Entry) = 2 + contColumBorder xs
                     |((x!!0) == Entry) || (x!!(tam-1) == Entry) = 1 + contColumBorder xs
                     |otherwise = contColumBorder xs
 where
  tam = length (x:xs)

verifyLengths :: Mine -> Bool
verifyLengths m 
              |(linhas m == (length (elements m))) && (columns m == length (elements m!!0)) = True
              |otherwise = False

verifyEntrances :: Mine -> Bool
verifyEntrances m = 
                let 
                    v = elements m
                    n = length (elements m) 
                    r1 = contColumBorder (drop 1 (take (n-1) v))
                    r2 = contLineBorder (v!!0)
                    r3 = contLineBorder (v!!(n - 1))
                in 
                   (r1 + r2 + r3) >= 2
                       
                
validMine :: Mine -> Bool
validMine m = (verifyLengths m) && (verifyEntrances m) 



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


current :: ConfM Point
current = undefined

mine :: ConfM Mine
mine = undefined

enoughEnergy :: Int -> ConfM Bool
enoughEnergy = undefined

incEnergy :: ConfM ()
incEnergy = undefined

valid :: Instr -> ConfM Bool
valid = undefined


updateMine :: Instr -> ConfM ()
updateMine = undefined

exec :: Instr -> ConfM ()
exec = undefined

initRobot :: Mine -> Robot
initRobot = undefined

run :: [Instr] -> Mine -> Mine
run = undefined

readLDM :: String -> IO (Either String Mine)
readLDM = undefined

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined
