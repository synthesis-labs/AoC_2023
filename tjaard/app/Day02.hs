module Day02Regex where
import Data.List.Split (splitOn)
import Day02Model
import Utils (captureGroups)

reds, greens, blues :: Int; reds = 12; greens = 13; blues = 14

colorCount :: String -> String -> Int
colorCount setStr color = if null cg then 0 else read $ head cg
  where cg = captureGroups setStr ("(\\d+) " ++ color)

parseGame :: String -> Game
parseGame str = Game { gameNumber = read _gameNumber, sets = _sets, possible = _possible, power = _power }
  where
    [_gameNumber, _setStr] = captureGroups str "Game (\\d+): (.*)"
    _setStrs = splitOn ";" _setStr
    _sets = [GameSet { red = colorCount s "red", green = colorCount s "green", blue = colorCount s "blue" } | s <- _setStrs ]
    _possible = all (\set -> red set <= reds && green set <= greens && blue set <= blues) _sets
    _power = maximum (map red _sets) * maximum (map green _sets) * maximum (map blue _sets)

run :: IO ()
run = do
  contents <- lines <$> readFile "./data/day02"
  let games = [parseGame x | x <- contents]
  let possibleSum = sum (map gameNumber (filter possible games))
  let powerSum = sum (map power games)
  putStrLn $ "Answer Q1 => " ++ show possibleSum
  putStrLn $ "Answer Q2 => " ++ show powerSum  