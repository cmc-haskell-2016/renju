module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix

data State = Black | Red
                     deriving (Show, Eq)
data W a = W a | Tie | None

type Win = W State
               
type Cell = Maybe State 

type Field = Matrix Cell

data World = World
          { field :: Field        -- матрица значений
          , state :: State        -- чей ход
          , win :: Win    -- флаг конца игры
          , pic :: [Picture]      -- загруженные изображения
          }

--размер ячейки
sizeCell :: Float
sizeCell = 40.0

--размерность матрицы (размерность сетки + 1) 
sizeField :: Int
sizeField = 10

--положение центра игровой доски
offsetX :: Float
offsetX = 0

offsetY :: Float
offsetY = (-50)

--заполнение матрицы разерности n
matrixFiling :: Int -> Matrix Cell
matrixFiling n = matrix n n $ \ _ -> Nothing

--возвращает список - строку матрицы 
getRow :: Int -> Matrix Cell -> [Cell] 
getRow n m = [m ! (i,j) | i <- [n]
                          , j <- [1 .. (ncols m)]]


main :: IO ()
main    
 = do
   pic1 <- loadBMP "img/1.bmp"
   pic2 <- loadBMP "img/2.bmp"
   pic3 <- loadBMP "img/3.bmp"
   pic4 <- loadBMP "img/4.bmp"
   pic5 <- loadBMP "img/5.bmp"
   pic6 <- loadBMP "img/texture.bmp"
   go (World (matrixFiling sizeField) Black None [pic1,pic2,pic3,pic4,pic5,pic6])

--запуск игры
go :: World -> IO ()
go world = play (InWindow "Game rejnzu" (500,500) (0,0)) 
               white 
               0
               world 
               convert 
               handle
               update

--ничего не делает
update ::Float -> World -> World
update _ w = w 

--переводит внутреннее представление мира в картинку
convert :: World -> Picture
convert (World m _ w p)  = Pictures $
                           drawPic w p ++
                           mainDrawField m

--отрисовка img
drawPic :: Win -> [Picture] -> [Picture]
drawPic (W Black) p  = [Translate offsetX (offsetY + 1) $ p !! 5
                               ,Translate offsetX (offsetY + 270) $ p !! 0
                               ,Translate offsetX (offsetY + 220) $ p !! 2]

drawPic (W Red)   p  = [Translate offsetX (offsetY + 1) $ p !! 5
                               ,Translate offsetX (offsetY + 270) $ p !! 0
                               ,Translate offsetX (offsetY + 220) $ p !! 1]

drawPic (Tie)    p  = [Translate offsetX (offsetY + 1) $ p !! 5
                               ,Translate offsetX (offsetY + 270) $ p !! 0
                               ,Translate offsetX (offsetY + 220) $ p !! 3]

drawPic       _           p  = [Translate offsetX (offsetY + 1) $ p !! 5
                               ,Translate (offsetX + 10) (offsetY + 220) $ p !! 4
                               ,Translate offsetX (offsetY + 270) $ p !! 0]

--отрисовка одной ячейки (позиции - положение квадратика относительно центра) и левой верхней фишечки, если она есть
drawCell :: Float -> Float -> Cell -> [Picture]
drawCell pos_x pos_y Nothing      =      [Translate 
                                                  pos_x 
                                                  pos_y $ 
                                                  rectangleWire sizeCell sizeCell]

drawCell pos_x pos_y (Just Black) =      [Translate 
                                                  pos_x 
                                                  pos_y $ 
                                                  rectangleWire sizeCell sizeCell
                                         ,Translate 
                                                  (pos_x - (sizeCell / 2)) 
                                                  (pos_y + (sizeCell / 2)) $ 
                                                   Color black $ circleSolid (sizeCell / 2)] 

drawCell pos_x pos_y (Just Red)   =      [Translate 
                                                  pos_x 
                                                  pos_y $ 
                                                  rectangleWire sizeCell sizeCell 
                                         ,Translate 
                                                  (pos_x - (sizeCell / 2)) 
                                                  (pos_y + (sizeCell / 2)) $ 
                                                   Color red $ circleSolid (sizeCell / 2)]
                  

--отрисовка строки
--pos_x pos_y положение самого первого квадратика строки относительно центра

drawRow :: Float -> Float -> [Cell] -> [Picture]

drawRow pos_x pos_y [(Just Black)]    =     [Translate 
                                                   (pos_x - (sizeCell / 2)) 
                                                   (pos_y + (sizeCell / 2)) $
                                                   Color black $ circleSolid (sizeCell / 2)]

drawRow pos_x pos_y [(Just Red)]      =     [Translate 
                                                   (pos_x - (sizeCell / 2)) 
                                                   (pos_y + (sizeCell / 2)) $
                                                   Color red   $ circleSolid (sizeCell / 2)]
drawRow _     _     [Nothing]         =     [Blank]

drawRow pos_x pos_y l                 =     (drawCell pos_x pos_y $ head l) 
                                                ++ (drawRow (pos_x + sizeCell) pos_y $ tail l)

--отрисовка всей сетки
mainDrawField :: Field -> [Picture]
mainDrawField m  =                            drawField 
                                                        (offsetX - sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        (offsetY + sizeCell * fromIntegral (((nrows m) - 1) `div` 2)) 
                                                        ((nrows m) - 1) 
                                                        m

--n количество оставшихся на отрисовку строк 

drawField :: Float -> Float -> Int -> Matrix Cell -> [Picture]
drawField pos_x pos_y 0 m          =     drawLastRow 
                                                        (pos_x - (sizeCell / 2)) 
                                                        (pos_y + (sizeCell / 2)) 
                                                        $ Main.getRow 1 m
drawField pos_x pos_y n m          =     (drawRow pos_x pos_y $ Main.getRow 1 m) 
                                                ++ (drawField 
                                                        pos_x (pos_y - sizeCell) 
                                                        (n - 1) 
                                                        (submatrix 2 (nrows m) 1 (ncols m) m)) 

--отрисовка последних фишичек
drawLastRow :: Float -> Float -> [Cell] -> [Picture]
drawLastRow _ _ []                 =      [Blank]
drawLastRow pos_x pos_y l          =      (drawLastCell 
                                                         pos_x 
                                                         pos_y $ 
                                                         head l) 
                                                     ++ (drawLastRow (pos_x + sizeCell)   pos_y  $ tail l)

--отрисовка фишки последней строки (не рисуе квадратик)
drawLastCell :: Float -> Float -> Cell -> [Picture]
drawLastCell _ _ Nothing                  =       [Blank]
drawLastCell pos_x pos_y (Just Black)     =       [Translate 
                                                         pos_x 
                                                         pos_y $ 
                                                         Color black $ circleSolid (sizeCell / 2)]
drawLastCell pos_x pos_y (Just Red)       =       [Translate 
                                                         pos_x 
                                                         pos_y $ 
                                                         Color red $ circleSolid (sizeCell / 2)]

--обработка внешних событий
handle :: Event -> World -> World
handle       _                  (World m s (W Red) p) = World m s (W Red) p
handle       _                  (World m s (W Black) p) = World m s (W Black) p
handle (EventKey (MouseButton LeftButton) _ _ (x,y)) w = checkWorld (mainNumberRow (x,y),mainNumberCol (x,y)) w
handle _ w = w

--обрабочик мира
checkWorld :: (Int,Int) -> World -> World
checkWorld (0,_) m = m
checkWorld (_,0) m = m
checkWorld coord (World m s l p) | m ! coord == Nothing  =  World
                                                           (putIn coord s m)
                                                           (inverseState s)
                                                            (gameRules coord s (putIn coord s m))
                                                            p
                                 | otherwise             =  World m s l p

--получение номера столбца
mainNumberCol :: (Float,Float) -> Int
mainNumberCol x                          = numberCol
                                           x
                                           (offsetX - sizeCell - fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberCol :: (Float,Float) -> Float -> Int
numberCol (x,_) n | x < n || x > (n + fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 + 1 + div (round (x - offsetX)) (round sizeCell)

--получение номера строки
mainNumberRow :: (Float,Float) -> Int
mainNumberRow x = numberRow
                          x
                         (offsetY + sizeCell + fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberRow :: (Float,Float) -> Float -> Int
numberRow (_,y) n | y > n || y < (n - fromIntegral(sizeField) * sizeCell) = 0
                  | otherwise                                             = div sizeField 2 - div (round ( y - offsetY)) (round sizeCell)

--заполнение ячейки матрицы
putIn :: (Int,Int) -> State -> Matrix Cell -> Matrix Cell
putIn (a,b) Black m = setElem (Just Black) (a,b) m
putIn (a,b) Red m   = setElem (Just Red) (a,b) m 

--инвертирование состояния
inverseState :: State -> State
inverseState Black = Red
inverseState Red = Black

--получение окрестности 5 для строки
getNeighRow :: (Int, Int) -> Matrix Cell -> [Cell]
getNeighRow (a,b) m = [m ! (i,j) | i <- [a]
                          , j <- [(b - 4) .. (b + 4)]
                          , j >= 1 && j <= (ncols m)]

--получение окрестности 5 для столбца
getNeighCol :: (Int, Int) -> Matrix Cell -> [Cell]
getNeighCol (a,b) m = [m ! (i,j) | i <- [(a - 4) .. (a + 4)]
                          , j <- [b]
                          , i >= 1 && i <= (nrows m)]

--получение окрестности 5 по диагонали влево

getNeighDiagLeft :: (Int, Int) -> Matrix Cell -> [Cell]
getNeighDiagLeft (a,b) m = diagLeft (a,b) m (a + 4) (b + 4)

diagLeft :: (Int, Int) -> Matrix Cell -> Int -> Int -> [Cell]
diagLeft (a,b) m i j 
                     | i == (a - 5) = []
                     | i >= 1 && i <= (nrows m) && j>=1 && j <= (ncols m) = m ! (i,j) : (diagLeft (a,b) m (i - 1) (j - 1))
                     | otherwise = diagLeft (a,b) m (i - 1) (j - 1)

--получение окрестности 5 по диагонали вправо

getNeighDiagRight :: (Int, Int) -> Matrix Cell -> [Cell]
getNeighDiagRight (a,b) m = diagRight (a,b) m (a + 4) (b - 4)

diagRight :: (Int, Int) -> Matrix Cell -> Int -> Int -> [Cell]
diagRight (a,b) m i j | i == (a - 5) = []
                      | i >= 1 && i <= (nrows m) && j>=1 && j <= (ncols m) = m ! (i,j) : (diagRight (a,b) m (i - 1) (j + 1))
                      | otherwise = diagRight (a,b) m (i - 1) (j + 1)

--по [Cell] определяет, есть ли выигрыш

winner :: [Cell] -> Bool
winner l = winFunc l 0 

winFunc :: [Cell] -> Int -> Bool
winFunc _ 4 = True
winFunc (x : (y : xs)) ac | x == y && (x /= Nothing) = winFunc (y : xs) (ac + 1) 
                          | otherwise = winFunc (y:xs) 0 
winFunc _ _ = False

--Определяет, есть ли выигрыш в игре
--None - никто, Tie - полностью заполнена, иначе победитель (W Black |W Red)

gameRules :: (Int,Int) -> State -> Matrix Cell -> Win
gameRules (x,y) s m | winner ( getNeighDiagRight (x,y) m ) ||
                    winner ( getNeighDiagLeft (x,y) m ) || 
                    winner ( getNeighRow (x,y) m ) ||
                    winner ( getNeighCol (x,y) m ) 
                    = (W s)
                  | fullBoard (toList m) = Tie
                  | otherwise = None

--заполнена ли доска

fullBoard :: [Cell] -> Bool
fullBoard [] = True
fullBoard (Nothing : _) = False
fullBoard l = fullBoard (tail l)



