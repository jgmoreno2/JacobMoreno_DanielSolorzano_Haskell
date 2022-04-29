import Data.Char

mkBoard :: Int -> [[Int]]
mkBoard n = mkBoardHelper n []

mkBoardHelper :: Int -> [[Int]] -> [[Int]]
mkBoardHelper n board
    | length board < n = mkBoardHelper n [addRow n]++board
    | otherwise = board

addRow :: Int -> [Int]
addRow n = addRowHelper n []

addRowHelper :: Int -> [Int] -> [Int]
addRowHelper n tmpRow
    | length tmpRow < n = addRowHelper n [0]++tmpRow
    | otherwise = tmpRow

mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

size :: [[Int]] -> Int
size = length

row :: Int -> [[Int]] -> [Int]
row n = rowHelper 0 n []

rowHelper :: Int -> Int -> [Int] -> [[Int]] -> [Int]
rowHelper x y rowTmp board
    | length rowTmp >= length board = rowTmp
    | otherwise = rowHelper newX y newRow board where
        newX = x + 1
        fromColumn = board!!x
        newRow = fromColumn!!y:rowTmp

column :: Int -> [[Int]] -> [Int]
column n board = board!!(n-1)

mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
mark x y board player = boardTwo where
    xIndex = x - 1
    yIndex = y - 1
    tmpRow = row x board
    (rowStart,_:rowEnd) = splitAt yIndex tmpRow
    tmpRowTwo = rowStart ++ player : rowEnd
    (boardStart,_:boardEnd) = splitAt xIndex board
    boardTwo = boardStart ++ tmpRowTwo : boardEnd

getTileAt :: Int -> Int -> [[Int]] -> Int
getTileAt x y board = tile where
        fromColumn = board!!x
        tile = fromColumn!!y

isEmpty :: Int -> Int -> [[Int]] -> Bool
isEmpty x y board = getTileAt x y board == 0

isMarked :: Int -> Int -> [[Int]] -> Bool
isMarked x y board = not (isEmpty x y board)

isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
isMarkedBy x y board player = getTileAt x y board == player

marker :: Int -> Int -> [[Int]] -> Int
marker = getTileAt

isFull :: [[Int]] -> Bool
isFull board = isFullColumnHelper 0 board True

isFullColumnHelper :: Int -> [[Int]] -> Bool -> Bool
isFullColumnHelper x board fullTracker
    | not fullTracker = False
    | x >= length board = True
    | otherwise = isFullColumnHelper newX board newTracker where
        newX = x + 1
        newTracker = isFullRowHelper 0 (board!!x) True

isFullRowHelper :: Int -> [Int] -> Bool -> Bool
isFullRowHelper y tmpRow fullTracker
    | not fullTracker = False
    | y >= length tmpRow = True
    | otherwise = isFullRowHelper newY tmpRow newTracker where
        newY = y + 1
        element = tmpRow!!y
        newTracker = element /= 0

isWonBy :: [[Int]] -> Int -> Bool
isWonBy board player  = result where
    winningRow = getWinningRow 0 0 board player []
    result = not (null winningRow)

getWinningRow :: Int -> Int -> [[Int]] -> Int -> [Int] -> [Int]
getWinningRow x y board player winner
    | not (null winner) = winner
    | x >= length board && y >= length board = []
    | y >= length board = getWinningRow (x+1) 0 board player []
    | otherwise = getWinningRow x (y+1) board player winningRow where
        winningRow
          | isMarkedBy x y board player && x < length board - 4 && checkRight x y board player = [x, y, x+1, y, x+2, y, x+3, y, x+4, y]
          | isMarkedBy x y board player && x > 3 && y < length board - 4 && checkAboveLeft x y board player = [x, y, x-1, y+1, x-2, y+2, x-3, y+3, x-4, y+4]
          | isMarkedBy x y board player && x < length board - 4 && y < length board - 4 && checkAboveRight x y board player = [x, y, x+1, y+1, x+2, y+2, x+3, y+3, x+4, y+4]
          | isMarkedBy x y board player && y < length board - 4 && checkBelow x y board player = [x, y, x, y+1, x, y+2, x, y+3, x, y+4]
          | otherwise = []

checkRight :: Int -> Int -> [[Int]] -> Int -> Bool
checkRight x y board player = isMarkedBy (x+1) y board player && isMarkedBy (x+2) y board player && isMarkedBy (x+3) y board player && isMarkedBy (x+4) y board player

checkAboveLeft :: Int -> Int -> [[Int]] -> Int -> Bool
checkAboveLeft x y board player = isMarkedBy (x-1) (y+1) board player && isMarkedBy (x-2) (y+2) board player && isMarkedBy (x-3) (y+3) board player && isMarkedBy (x-4) (y+4) board player

checkAboveRight :: Int -> Int -> [[Int]] -> Int -> Bool
checkAboveRight x y board player = isMarkedBy (x+1) (y+1) board player && isMarkedBy (x+2) (y+2) board player && isMarkedBy (x+3) (y+3) board player && isMarkedBy (x+4) (y+4) board player

checkBelow :: Int -> Int -> [[Int]] -> Int -> Bool
checkBelow x y board player = isMarkedBy x (y+1) board player && isMarkedBy x (y+2) board player && isMarkedBy x (y+3) board player && isMarkedBy x (y+4) board player

isDraw :: [[Int]] -> Bool
isDraw board = not (isWonBy board mkPlayer || isWonBy board mkOpponent) && isFull board

isGameOver :: [[Int]] -> Bool
isGameOver board = isWonBy board mkPlayer || isWonBy board mkOpponent || isDraw board

boardToStr :: (Int -> Char) -> [[Int]] -> String
boardToStr playerToChar board = boardToStrHelper playerToChar board 0 0 ""

boardToStrHelper :: (Int -> Char) -> [[Int]] -> Int -> Int-> String -> String
boardToStrHelper playerToChar board x y currString
    | y == 0 = boardToStrHelper playerToChar board 0 1 (getFirstRows "" (length board) 0 0 board)
    | y <= length board && x == 0 = boardToStrHelper playerToChar board 1 y (currString ++ ['\n', intToDigit (mod y 10), '|', ' ', playerToChar (getTileAt x (y-1) board)])
    | y <= length board && x < length board = boardToStrHelper playerToChar board (x+1) y (currString ++ [' ', playerToChar (getTileAt x (y-1) board)])
    | y <= length board = boardToStrHelper playerToChar board 0 (y+1) currString
    | otherwise = currString

getFirstRows :: String -> Int -> Int -> Int -> [[Int]] -> String
getFirstRows currString len x y board
    | y == 0 && x == 0 = getFirstRows " x 1" len 1 0 board
    | y == 0 && x < len = getFirstRows (currString ++ [' ', intToDigit (mod (x+1) 10)]) len (x+1) 0 board
    | y == 0 = getFirstRows (currString ++ "\ny --") len 1 1 board
    | y == 1 && x < len = getFirstRows (currString ++ "--") len (x+1) 1 board
    | otherwise = currString

convertTile :: Int -> Char
convertTile player
    | player == mkPlayer = 'X'
    | player == mkOpponent = 'O'
    | otherwise = '.'

main :: IO ()
main = putStrLn (boardToStr convertTile (mkBoard 15))