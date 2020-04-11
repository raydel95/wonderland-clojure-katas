(ns tiny-maze.solver)

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

(defn solve [maze [x y :as pos]]
  (when-let [v (get-in maze pos)]
    (let [nmaze (assoc-in maze pos :x)]
      (cond
        (= v :E) nmaze
        (or (= 0 v) (= :S v))
        (some identity (map (fn [[i j]] (solve nmaze [(+ x i) (+ y j)])) dirs))))))

(defn solve-maze [maze]
  (let [pos (some #(when (not= -1 (second %)) %)
        (map-indexed
         (fn [idx v] [idx (.indexOf v :S)]) maze))]
    (solve maze pos)))








