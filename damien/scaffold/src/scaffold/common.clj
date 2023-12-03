(ns scaffold.common)

(defn sum [vals] (reduce + vals))
(defn digit? [c] (and (>= 0 (compare \0 c))
                      (>= 0 (compare c \9))))
