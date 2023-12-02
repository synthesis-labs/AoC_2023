module Day02ShowFns where
import Day02 (GameSet (red, green, blue), Game (gameNumber, possible, sets))

showGameSet :: GameSet -> String
showGameSet gameSet =
    " | Set RGB: " ++ show (red gameSet) ++ "," ++ show (green gameSet) ++ "," ++ show (blue gameSet)

showGame :: Game -> String
showGame game =
    "Game " ++ show (gameNumber game) ++ " (" ++ show (possible game) ++ ")" ++ concatMap showGameSet (sets game)