module Main where
import Board

playerToChar :: Int -> Char
playerToChar = convertTile

readXY :: [[Int]] -> Int -> IO(Int, Int)
readXY board player = do
    putStrLn (playerToChar player : "'s turn: enter x y")
    ints <- readInts
    if length ints == 2 && head ints > 0 && ints!!1 > 0 && head ints <= size board && ints!!1 <= size board && isEmpty (head ints-1) ((ints!!1)-1) board then return (head ints, ints!!1) else do
        putStrLn "Invalid input."
        readXY board player

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

game :: [[Int]] -> Int -> IO()
game board player = do
    putStrLn (boardToStr playerToChar board)
    (x, y) <- readXY board player
    let nextBoard = mark x y board player in
        if isWonBy nextBoard player
        then putStrLn (boardToStr playerToChar nextBoard ++ ['\n', playerToChar player] ++ " wins!")
        else if isFull nextBoard
        then putStrLn (boardToStr playerToChar nextBoard ++ "\nRound ended in draw!")
        else game nextBoard (nextPlayer player)

main :: IO()
main = do
    let board = mkBoard 15 in
        game board mkPlayer