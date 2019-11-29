module Snake where

import System.Random
import System.IO.Unsafe

tamanho :: Posicao
tamanho = (600,600)

type Posicao = (Int,Int)
type PosicaoComida = Posicao
type Cobra = [Posicao]
data Estado = Estado {posicaoComida :: PosicaoComida, cobra :: Cobra, ultimoMove :: Movimenta} deriving Show
data Movimenta = C | B | D | E deriving (Show, Eq)

-- Main
recomecar :: Estado
recomecar = Estado p [(300,300)] C
    where (x,y) = novaComida
          p = (x*20,y*20)
--Definir posição da comida
novaComida :: PosicaoComida -- ^ Posicao comida
novaComida = (unsafePerformIO (geraComida 29), unsafePerformIO (geraComida 29))

geraComida :: Int -> IO Int
geraComida x = randomRIO(1,x)

-- Mexer a cabeça da cobra
movimentar :: Estado -> Movimenta -> Estado
movimentar est C = if posComida == (x,y+20) then comer est posComida else est {cobra = (x,y+20):(init h), ultimoMove = C }
    where posComida = posicaoComida est
          h@((x,y):t) = cobra est
movimentar est B = if posComida == (x,y-20) then comer est posComida else est {cobra = (x,y-20):(init h), ultimoMove = B }
    where posComida = posicaoComida est
          h@((x,y):t) = cobra est
movimentar est D = if posComida == (x+20,y) then comer est posComida else est {cobra = (x+20,y):(init h), ultimoMove = D }
    where posComida = posicaoComida est
          h@((x,y):t) = cobra est
movimentar est E = if posComida == (x-20,y) then comer est posComida else est {cobra = (x-20,y):(init h), ultimoMove = E }
    where posComida = posicaoComida est
          h@((x,y):t) = cobra est

--Comer comida
comer :: Estado -> PosicaoComida -> Estado 
comer est (x,y) = Estado novaComida ((x,y):l) lastMove
    where l = cobra est
          lastMove = ultimoMove est

--Ver se a peça para onde se quer movimentar 
podeAndar :: Estado -> Movimenta -> Bool
podeAndar est a = case a of C -> elem (x,y+20) posCobra
                            B -> elem (x,y-20) posCobra
                            D -> elem (x+20,y) posCobra
                            E -> elem (x-20,y) posCobra
    where posCobra = cobra est
          (x,y) = head posCobra

-- Faz o movimento
move :: Estado -> Movimenta -> Estado
move est a = if not (podeAndar est a) then movimentar est a else est

moverSemToque :: Estado -> Estado
moverSemToque est = move est movi
    where movi = ultimoMove est
