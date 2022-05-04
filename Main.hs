-- Jacob Moreno and Daniel Solorzano

module Main where
import Board

playerToChar :: Int -> Char
playerToChar = convertTile

readXY :: [[Int]] -> Int -> IO(Int, Int) -- gets x y coords from console
readXY board player = do
    putStrLn (playerToChar player : "'s turn: enter x y")
    ints <- readInts -- gets list of IO ints
    if length ints == 2 && head ints > 0 && ints!!1 > 0 && head ints <= size board && ints!!1 <= size board && isEmpty (head ints-1) ((ints!!1)-1) board then return (head ints, ints!!1) else do
        putStrLn "Invalid input."
        readXY board player

readInts :: IO [Int] -- reads two integers from console and turns it into a IO list of Ints
readInts = fmap (map read.words) getLine -- reads line from console and maps it to list of ints

game :: [[Int]] -> Int -> IO() -- handles controller functionality
game board player = do
    putStrLn (boardToStr playerToChar board) -- print the board for every turn
    (x, y) <- readXY board player -- get x y coords of player's move
    let nextBoard = mark x y board player in -- creates board to be updated, marking the spot that the player designated with the player's tile
        if isWonBy nextBoard player -- if player won the game with this move, print win message
        then putStrLn (boardToStr playerToChar nextBoard ++ ['\n', playerToChar player] ++ " wins!")
        else if isFull nextBoard -- if board is full, print draw message
        then putStrLn (boardToStr playerToChar nextBoard ++ "\nRound ended in draw!")
        else game nextBoard (nextPlayer player) -- recall game and alternate player's turn

main :: IO()
main = do
    let board = mkBoard 15 in -- create board of size 15 and call game to initiate game
        game board mkPlayer