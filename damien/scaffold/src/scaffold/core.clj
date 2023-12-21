(ns scaffold.core
  (:gen-class)
  (:require [scaffold.day3 :as day3]))


; (use '[clojure.string :only (join split replace)])

; (defn getInput [day] 
;   (let [sessionCookie (replace (slurp "./session.txt") #"\n" "")]
;     (let [result (client/get (join ["https://adventofcode.com/2022/day/" (str day) "/input"] )
;                         {:headers {"Cookie" sessionCookie}})]
;       (get result :body)
;       )))
;
; (def input (getInput 1))
;
; (def inputSplit (split input #"\n\n"))
;
; (def inputByElfStr (map #(split % #"\n") inputSplit))
;
; (def inputByElfInt (map #(map read-string %) inputByElfStr))
;
; (def elvesSum (map #(reduce + %) inputByElfInt))
;
; (def partOneAnswer (apply max elvesSum))
; (println partOneAnswer)
;
; (def partTwoAnswer (reduce + (take 3 (reverse (vec (sort elvesSum))))))
; (println partTwoAnswer)
