(ns scaffold.day2
  (:require [scaffold.readfile :as readfile])
  (:require [scaffold.common :as c])
  )
(require '[clojure.string :as s])

(def input (readfile/getInput 2))
; (def input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def games (s/split input #"\n"))

(defn getGameId [str] (read-string (last (re-find #"Game (\d+):" str))))
(defn getColour [colour, s] (map read-string (map last (re-seq (re-pattern  (apply str "(\\d+) " colour)) s))))
(defn getMaxColour [colour, s] (reduce max (getColour colour s)))

(def maxedGames (map vector (map getGameId games)
(map #(getMaxColour "blue" %) games)
(map #(getMaxColour "red" %) games)
(map #(getMaxColour "green" %) games)
))

(defn returnPossibleGameId [[gameId blue red green]]  (if (and (<= blue 14) (<= red 12) (<= green 13))
 gameId
 0))

(def partOneAnswer (c/sum (map returnPossibleGameId maxedGames)))
(println partOneAnswer)

(defn power [[_ blue red green]] (* blue red green))

(def partTwoAnswer (->> maxedGames 
     (map power)
     (c/sum)))
(println partTwoAnswer)
