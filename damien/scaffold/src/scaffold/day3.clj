(ns scaffold.day3
  (:require [scaffold.readfile :as readfile]
            [clojure.string :as s])
  (:require [scaffold.common :as c])
  )
(require '[clojure.string :as s])

; (def input (readfile/getInput 3))


(def input 
 "467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..")

(def inputArr (s/split-lines input))

(def maxX (-> inputArr
              (first)
              (count)
              ))

(def maxY (-> inputArr
              (count)
              ))


; (defn checkLineNumbers [acc [head & rest]] (println head) (if (c/digit? head) 
;                                                 (checkLineNumbers (str acc head) rest)  
;                                                 acc))



; (checkLineNumbers "" "467..114")

; (defn checkNumbers [idx s] )

; (map-indexed checkNumbers inputArr)

(defn sym? [c] (and (not (c/digit? c))
                        (not= c \.)))


(defn getC [x y] (-> inputArr 
                     (nth y)
                     (nth x)
                     ))

(sym? \.)

(defn digitAt? [x y] 
  (if (not (or (< x 0) (< y 0) (>= x maxX) (>= y maxY)))
    (c/digit? (getC x y))
    false))

(digitAt? 0 0)

(defn ourFunction [x y c] (if (and 
                                (sym? c) 
                                (or 
                                  (digitAt? (- x 1) (- y 1))
                                  (digitAt? x (- y 1))
                                  (digitAt? (+ x 1) (- y 1))
                                  (digitAt? (+ x 1) y)
                                  (digitAt? (+ x 1) (+ y 1))
                                  (digitAt? x (+ y 1))
                                  (digitAt? (- x 1) (+ y 1))
                                  (digitAt? (- x 1) y)
                                  ))
                            (str "yes" x y)
                            (str "nah" x y)
                            ))

(defn mapyThing [idy s] (map-indexed #(ourFunction %1 idy %2) s))

(map-indexed mapyThing inputArr)
