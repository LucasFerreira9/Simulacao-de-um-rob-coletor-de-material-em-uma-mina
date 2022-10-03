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

intIntoElement :: Int -> Element
intIntoElement i = Material i

pElement :: Parser Char Element
pElement = pElementNum <|> pElementText
    where 
      pElementNum = intIntoElement <$> natural1
      pElementText = stringIntoElement <$> ((token "Empty") <|>  (token "Entry") <|> (token "Wall") <|>  (token "Rock"))                                                

type Line = [Element]

data Mine = Mine {
              linhas    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
  show = undefined


{- 1 - instancia da classe Show para o tipo Robot-}
instance Show Robot where
    show r = "Energy:" ++show (energy r) ++ "\\nPosition" ++ show (position r) ++ "\\nCollect:"++ show (collected r)

{- 2 - instancia de Show para o tipo Element-}
instance Show Element where
    show elem = case elem of
                           Empty -> show " "
                           Entry -> show 'E'
                           Wall -> show '%'
                           Earth -> show '.'
                           Rock -> show '*'
                           Material q -> case q of
                                                 100 -> show ':'
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
contColumBorder (y:ys)
                     |((y!!0) == Entry) && (y!!(tam-1) == Entry) = 2 + contColumBorder ys
                     |((y!!0) == Entry) || (y!!(tam-1) == Entry) = 1 + contColumBorder ys
                     |otherwise = contColumBorder ys
 where
  tam = length y

verifyLengths :: Mine -> Bool
verifyLengths m 
              |(linhas m == (length (elements m))) && (columns m == length (elements m!!0)) = True
              |otherwise = False

verifyEntrances :: Mine -> Bool
verifyEntrances m = (contColumBorder (elements m) + contLineBorder ((elements m)!!0) + contLineBorder ((elements m)!!((length (elements m)) - 1))) >= 2
                
validMine :: Mine -> Bool
validMine m = (verifyLengths m) && (verifyEntrances m) 






pLine :: Parser Char Line
pLine = undefined

pMine :: Parser Char Mine
pMine = undefined

data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

pInstr :: Parser Char Instr
pInstr = undefined

pProgram :: Parser Char [Instr]
pProgram = undefined

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
