{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module TatamiGrid ( put,
                     moveLeft,
                     moveRight,
                     moveUp,
                     moveDown,
                     Grid(..),
                     GridWithAPointer(..),
                     putTatamiDown,
                     putTatamiUp,
                     putTatamiLeft,
                     putTatamiRight,
                     cover
             ) where

import Data.Char (isLetter)

stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


instance (Show a) => Show (GridWithAPointer a) where
     show (GridWithAPointer(Grid above, left, atPointer, right, Grid below)) =
         unlines (map showRow above) ++ rowWithPointer ++ "\n" ++ unlines (map showRow below)
         where 
             
             showRow row = unwords [padRight s | s <- map show row]
             padRight s = s ++ replicate (colWidth - visibleLength s) ' '
             colWidth = maximum[maximum (map visibleLength col) | col <- transpose g]
             strGridAbove = map (map show) above
             strMiddle = map show (reverse (left)) ++ [highlight atPointer] ++ (map show right)
             strGridBelow = map (map show) below
             g = strGridAbove ++ [strMiddle] ++ strGridBelow
             rowWithPointer 
                 | null left = highlight atPointer ++ replicate (1 + colWidth - visibleLength (show atPointer)) ' ' ++ showRow right
                 | otherwise = showRow (reverse(left)) ++ " " ++ highlight atPointer ++ replicate (1 + colWidth - visibleLength (show atPointer)) ' ' ++ showRow right
             highlight x = "\ESC[44m" ++ show x ++ "\ESC[0m"
             

put :: a -> GridWithAPointer a -> GridWithAPointer a
put putVal (GridWithAPointer(above, left, atPointer, right, below)) = GridWithAPointer (above, left, putVal, right, below)

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer(above, left, atPointer, right, below))
    | null left = GridWithAPointer(above, left, atPointer, right, below)
    |otherwise = GridWithAPointer(above, newLeft, newAtPointer, newRight, below)
    where
        newLeft = safeTail(left)
        newAtPointer = head(left)
        newRight = atPointer:right

moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer(above, left, atPointer, right, below))
    | null right = GridWithAPointer(above, left, atPointer, right, below)
    |otherwise = GridWithAPointer(above, newLeft, newAtPointer, newRight, below)
    where
        newLeft = atPointer:left
        newAtPointer = head(right)
        newRight = safeTail(right)

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    |null above = GridWithAPointer(Grid above, left, atPointer, right, Grid below)
    |otherwise = GridWithAPointer(Grid newAbove, newLeft, newAtPointer, newRight, Grid newBelow)
    where
        newAbove = reverse(safeTail(reverse(above)))
        rowWithNewPointer = head(reverse(above))
        newLeft = reverse(take (length left) rowWithNewPointer)
        newAtPointer = rowWithNewPointer !! (length left)
        newRight = drop (length(left) + 1) rowWithNewPointer
        newBelow = addToBelow:below
        addToBelow = reverse(left) ++ (atPointer:right)
        

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    | null below = GridWithAPointer(Grid above, left, atPointer, right, Grid below)
    |otherwise = GridWithAPointer(Grid newAbove, newLeft, newAtPointer, newRight, Grid newBelow)
    where
        newAbove = above ++ [addToAbove]
        addToAbove = reverse(left) ++ (atPointer:right)
        rowWithNewPointer = head(below)
        newLeft = reverse(take (length left) rowWithNewPointer)
        newAtPointer = rowWithNewPointer !! (length left)
        newRight = drop (length(left) + 1) rowWithNewPointer
        newBelow = safeTail(below)
        


putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp n g@(GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    | not(atPointer == 0) || not((valUnderPointer (moveUp g)) == 0) = g
    | otherwise = moveDown (put n (moveUp(put n g)))

putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown n g@(GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    | not(atPointer == 0) || not((valUnderPointer (moveDown g)) == 0) = g
    | otherwise = moveUp (put n (moveDown(put n g)))

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight n g@(GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    | not(atPointer == 0) || not((valUnderPointer (moveRight g)) == 0) = g
    |otherwise = moveLeft (put n (moveRight(put n g)))

putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft n g@(GridWithAPointer(Grid above, left, atPointer, right, Grid below))
    | not(atPointer == 0) || not((valUnderPointer (moveLeft g)) == 0) = g
    |otherwise = moveRight (put n (moveLeft(put n g)))




cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover g@(GridWithAPointer(Grid above, left, atPointer, right, Grid below)) 
    | odd(length(below !! 0)) && odd(length (below) + length (above) + 1) = error "odd number of cells, can't cover grid with 2x1 tatamis"
    | otherwise = fillFrames 0 (moveToTopLeft g)
    where
        width = length (head below)
        height = length above + 1 + length below
        
        fillFrames frameNum g'
            | frameNum * 2 >= width && (frameNum*2 - width == 1) = g'
            | frameNum * 2 >= height = g'
            | frameNum == 0 = fillFrames 1 (fillFrame 0 height width g')
            | otherwise = fillFrames (frameNum + 1) (fillFrame (getCurrentTatamiNumber g') (height - 2 * frameNum) (width - 2 * frameNum) (moveToFrameStart frameNum g'))

getCurrentTatamiNumber :: GridWithAPointer Integer -> Integer
getCurrentTatamiNumber (GridWithAPointer(Grid above, left, atPointer, right, Grid below)) =
    maximum (atPointer : left ++ right ++ concat above ++ concat below)

moveToTopLeft :: GridWithAPointer a -> GridWithAPointer a
moveToTopLeft g@(GridWithAPointer(Grid above, left, _, _, _))
    | null above && null left = g
    | not (null above) = moveToTopLeft (moveUp g)
    | otherwise = moveToTopLeft (moveLeft g)

moveToFrameStart :: Int -> GridWithAPointer a -> GridWithAPointer a
moveToFrameStart depth g = moveDown' depth (moveRight' depth (moveToTopLeft g))
    where
        moveRight' 0 g' = g'
        moveRight' n g' = moveRight' (n-1) (moveRight g')
        moveDown' 0 g' = g'
        moveDown' n g' = moveDown' (n-1) (moveDown g')


valUnderPointer :: GridWithAPointer Integer -> Int
valUnderPointer (GridWithAPointer (Grid above, left, atPointer, right, Grid below)) = fromIntegral(atPointer)



fillFrame :: Integer -> Int -> Int -> GridWithAPointer Integer -> GridWithAPointer Integer
fillFrame startingInt height width g@(GridWithAPointer (Grid above, left, atPointer, right, Grid below)) 
    | width == 1 = putTatamiDown (startingInt+2) (moveDown(moveDown(putTatamiDown (startingInt+1) g)))
    | height == 1 = putTatamiRight (startingInt+2) (moveRight(moveRight(putTatamiRight (startingInt+1) g)))
    | height == 2 && width == 2 = putTatamiRight (startingInt+2) (moveDown(putTatamiRight (startingInt+1) g))
    | otherwise = run 1 g
        where
            run n g
                | n - 2 == (width `div` 2)*2 - 1 + ((height `div` 2)*2 - 1) && even(width) && even(height) = moveDown (moveDown g)
                | n - 2 == (width `div` 2)*2 - 1 + ((height `div` 2)*2 - 1) && odd(width + height) = moveDown g
                | n - 1 > (width `div` 2)*2 - 1 + ((height `div` 2)*2 - 1) = g
                | n - 2 == (width `div` 2)*2 - 1 + (height `div` 2) - 1 && odd(height) = run (n + 1) (moveUp(moveLeft (putTatamiLeft (startingInt + (fromIntegral n)) g)))
                | n - 2 == (width `div` 2)*2 - 1 + (height `div` 2) - 1 && odd(width) = run (n + 1) (moveLeft(moveLeft (putTatamiLeft (startingInt + (fromIntegral n)) g)))
                | n - 1 > (width `div` 2)*2 - 1 + (height `div` 2) - 1 = run (n + 1) (moveUp (moveUp (putTatamiUp (startingInt + (fromIntegral n)) g)))
                | n - 2 == (width `div` 2) - 1 + (height `div` 2) - 1 && odd(width + height) = run (n + 1) (moveDown(moveDown(putTatamiDown (startingInt + (fromIntegral n)) g)))
                | n - 2 == (width `div` 2) - 1 + (height `div` 2) - 1 && even(width) && even(height) = run (n + 1) (moveLeft(moveDown(putTatamiDown (startingInt + (fromIntegral n)) g)))
                | n - 1 > (width `div` 2) - 1 + (height `div` 2) - 1 = run (n + 1) (moveLeft (moveLeft (putTatamiLeft (startingInt + (fromIntegral n)) g)))
                | n - 2 == (width `div` 2) - 1 && (odd(width)) = run (n + 1) (moveDown(moveRight(putTatamiRight (startingInt + (fromIntegral n)) g)))
                | n - 1 > (width `div` 2) - 1 = run (n + 1) (moveDown (moveDown (putTatamiDown (startingInt + (fromIntegral n)) g)))
                | n == 1 = run (n + 1) (moveRight((putTatamiDown (startingInt + (fromIntegral n)) g)))
                | otherwise = run (n + 1) (putTatamiRightMoveRightOfIt (startingInt + (fromIntegral n)) g)
                
            
            putTatamiRightMoveRightOfIt n g = moveRight(moveRight(putTatamiRight n g))
