module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

estadoInicial :: [Picture] -> EstadoGloss
estadoInicial (poligno:comida:[]) = (0,0,[estado])
        where estado = createComida recomecar (poligno:comida:[])

type EstadoGloss = (Float,Float,[Picture])

poligno :: Picture -- Cobra inicial
poligno = Color white (Polygon [(0,0),(20,0),(20,20),(20,0)])

comida :: Picture -- Comida
comida = Color red (Polygon [(0,0),(20,0),(20,20),(0,20)])

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (x,y,(poligno:comida:[])) = Pictures [Translate x y poligno,comida]

createComida :: Estado -> [Picture] -> Picture
createComida est (poligno:comida:[]) = Pictures [Translate (fromIntegral c) (fromIntegral d) poligno,Translate (fromIntegral a) (fromIntegral b) comida]
    where (a,b) = posicaoComida est
          (c,d) = head (cobra est)

reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,y,li) = (x,y+20,li)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,y,li) = (x,y-20,li)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y,li) = (x-20,y,li)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y,li) = (x+20,y,li)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n (x,y,li) = (x,y,li)

fr :: Int
fr = 20

dm :: Display
dm = InWindow "Snake" (600, 600) (300, 300)

main :: IO ()
main = do 
          play dm              -- janela onde irá correr o jogo
               black           -- côr do fundo da janela
               fr              -- frame rate
               (estadoInicial [poligno,comida])  -- estado inicial
               desenhaEstadoGloss   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo

