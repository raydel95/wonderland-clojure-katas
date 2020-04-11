(ns magic-square.puzzle
  (:require [clojure.set :refer [difference]]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn valid3? [numbers sum t]
  (let [clean (filter identity t)
        s (apply + clean)]
    (case (count clean)
      3  (= sum s)
      2  (numbers (- sum s))
      true)))

(defn valid? [numbers sum [a b c d e f g h i]]
  (every? (partial valid3? numbers sum)
          [[a b c]
           [d e f]
           [g h i]
           [a d g] ;;col
           [b e h]
           [c f i]
           [a e i] ;;diagonal
           [c e g]]))

(defn solve [numbers sum square]
  (cond (empty? numbers) square
        (valid? numbers sum square)
        (->> numbers
             (map #(solve (difference numbers #{%}) sum (conj square %)))
             (some identity))))

(defn magic-square [values]
  (let [c (int (Math/sqrt (count values)))
        sum (/ (apply + values) c)]
    (mapv vec (partition c (solve (set values) sum [])))))
