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
    winningRow = getWinningRow x y board player
    result = not (null winningRow)

getWinningRow :: Int -> Int -> [[Int]] -> Int -> [Int]
getWinningRow x y board player
    | x >= length board && y >= length board = []
    | otherwise = result where
        winningRow = []
        if isMarkedBy x y board player && x < (length board) - 4 && checkRight x y board player
            then winningRow = [x, y, x+1, y, x+2, y, x+3, y, x+4, y]
        else winningRow = winningRow
    
checkRight :: Int -> Int -> [[Int]] -> Int -> Bool
checkRight x y board player = True