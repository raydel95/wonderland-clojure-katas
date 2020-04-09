(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :refer [difference union]]))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])

(defn safe [config]
  (not-any? #(or (= % #{:fox :goose}) (= % #{:goose :corn})) config))

(defn movement [[left boat right]]
  (cond (boat :you) [[(union left (difference boat #{:boat})) #{:boat} right]
                     [left #{:boat} (union right (difference boat #{:boat}))]]

        (left :you) (map (fn [x] [(difference left #{x} #{:you}) (conj #{:boat :you} x) right]) left)

        (right :you) (map (fn [x] [left (conj #{:boat :you} x) (difference right #{x} #{:you})]) right)))

(defn config [mem seq]
  (let [newconfig (filter #(and (not (mem %)) (safe %)) (movement seq))]
    (reduce (fn [acc nw] (assoc acc nw seq)) {} newconfig)))

(defn path [mem result]
  (take-while #(not= % :root) (iterate mem result)))

(defn solve* [mem sequences]
  (into {} (map (partial config mem) sequences)))

(defn solve [sequence]
  (let [result [#{} #{:boat} #{:goose :corn :fox :you}]]

    (loop [mem {sequence :root} nseqs [sequence]]

      (if (mem result)
        (reverse (path mem result))
        (let [clos (solve* mem nseqs)]
          (recur (merge clos mem) (keys clos)))))))

(defn river-crossing-plan []
  (solve (first start-pos)))

