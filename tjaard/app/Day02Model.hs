module Day02Model where

data GameSet = GameSet { red :: Int, green :: Int, blue :: Int }
data Game = Game { gameNumber :: Int, sets :: [GameSet], possible :: Bool, power :: Int }

showGameSet :: GameSet -> String
showGameSet gameSet =
    " | Set RGB: " ++ show (red gameSet) ++ "," ++ show (green gameSet) ++ "," ++ show (blue gameSet)

showGame :: Game -> String
showGame game =
    "Game " ++ show (gameNumber game) ++ " (" ++ show (possible game) ++ ")" ++ concatMap showGameSet (sets game)