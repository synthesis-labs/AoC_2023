(ns scaffold.day1
  (:require [scaffold.readfile :as readfile]))
(require '[clojure.string :as s])
(def input (readfile/getInput 1))

(def firsts (map #(first (re-seq #"\d" %)) (s/split input #"\n" )))
(def lasts (map #(last (re-seq #"\d" %)) (s/split input #"\n" )))

(def partOneAnswer (reduce + (map read-string (map #(s/join [%1 %2]) firsts lasts))))
(println partOneAnswer)

(def regex #"\d|one|two|three|four|five|six|seven|eight|nine")
(def regex2 (re-pattern (s/reverse "d\\|one|two|three|four|five|six|seven|eight|nine")))
(def firstsPart2 (map #(first (re-seq regex %)) (s/split input #"\n" )))
(def lastsPart2 (map #(s/reverse (first (re-seq regex2 (s/reverse %)))) (s/split input #"\n" )))

(defn parseNum [s]
  (case s
    "one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"
    (str (read-string s))
    )
  )

(firstsPart2)

(def partTwoAnswer (reduce + 
                           (map read-string 
                                (map #(s/join [%1 %2]) 
                                     (map parseNum firstsPart2) 
                                     (map parseNum lastsPart2)))))
(println partTwoAnswer)
