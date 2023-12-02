(ns scaffold.readfile
  (:require [clj-http.client :as client]))

(use '[clojure.string :only (join split replace)])

(defn getInput [day] 
  (let [sessionCookie (replace (slurp "./session.txt") #"\n" "")]
    (let [result (client/get (join ["https://adventofcode.com/2023/day/" (str day) "/input"] )
                        {:headers {"Cookie" sessionCookie}})]
      (get result :body)
      )))
