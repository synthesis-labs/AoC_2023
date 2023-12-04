(ns scaffold.day4
  (:require [scaffold.readfile :as readfile])
  (:require [scaffold.common :as c]))
(require '[clojure.string :as s])
(require '[clojure.math :as math])

(def input (readfile/getInput 4))

; (def input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
; Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
; Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
; Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
; Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
; Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def games (s/split input #"\n"))

(def gamesSplit (map #(s/split % #"\|") games))

(defn getGameNumbers [s] (map #(read-string (first %)) (vec (re-seq #"\s*(\d+\s*)" s))))
(defn getBothGameNumbers [s] 
  (let 
    [[gameNumber & games] (getGameNumbers (first s))] 
    (list gameNumber games (getGameNumbers (last s)))))

(def gamesNumbers (map getBothGameNumbers gamesSplit))

(defn checkNumberIn [arr num] (some? (some #{num} arr)))
(defn valNumberIn [arr num] (if (checkNumberIn arr num) 
                                                    1 
                                                    0))
(defn countNumbersIn [arr1 [arr2Head & rest]]  (if (= (count rest) 0) 
                                                (valNumberIn arr1 arr2Head) 
                                                (+ (valNumberIn arr1 arr2Head) (countNumbersIn arr1 rest))))

(def partOneAnswer 
   (c/sum (map 
     #(int (math/pow 2 
             (- 
                (countNumbersIn (nth % 1) (nth % 2))
               1))) 
     gamesNumbers)))
(println partOneAnswer)

(defn getRange [idx count] (range (+ idx 1) (+ 1 count idx)))

(defn getIndexes [g idx] (getRange idx (countNumbersIn (nth (nth g idx) 1) (nth (nth g idx) 2))))
(def getIndexesMemo (memoize getIndexes))
(countNumbersIn (nth (nth gamesNumbers 1) 1) (nth (nth gamesNumbers 1) 2))
(getIndexes gamesNumbers 1)

(declare countCardsMemo)
(defn countCards [g idx] (+ 1 (apply + 
                               (map 
                                 #(countCardsMemo g %) 
                                 (getIndexes g idx)
                                 ))))

(def countCardsMemo (memoize countCards))


(def partTwoAnswer (time (apply + (map #(countCards gamesNumbers %) (range (count gamesNumbers))))))
(println partTwoAnswer)

