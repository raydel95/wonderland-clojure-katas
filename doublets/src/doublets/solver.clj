(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.set :refer [intersection]]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(def dict (set words))

(def alph (map #(char (+ (int \a) %)) (range 0 26)))

(defn closure* [mem word]
  (let [newords (for [c (range 0 (count word))
                      n alph
                      :let [m (assoc word c n)]
                      :when (dict (apply str m))
                      :when (not (mem m))]
                  m)]
    (reduce (fn [acc nw] (assoc acc nw word)) {} newords)))

(defn closure [mem wordvec]
  (apply merge (map (partial closure* mem) wordvec)))

(defn path [mem word]
  (take-while #(not= % :root) (iterate mem word)))

(defn doublets2
  "regular solution"
  [word1 word2]
  (let [wv1 (vec word1) wv2 (vec word2)]
    (loop [mem {wv1 :root} dwords [wv1]]
      (cond (mem wv2) (map (partial apply str) (reverse (path mem wv2)))
            (not dwords) []
            :else (let [clos (closure mem dwords)]
                    (recur (merge clos mem) (keys clos)))))))

(defn doublets
  "meet in the middle solution"
  [word1 word2]
  (let [wv1 (vec word1) wv2 (vec word2)]
    (loop [mem1 {wv1 :root}
           dwords1 [wv1]
           mem2 {wv2 :root}
           dwords2 [wv2]]
      (let [iset (first (intersection (-> mem1 keys set) (-> mem2 keys set)))]
        (cond iset (map (partial apply str) (concat (reverse (path mem1 iset))
                                                         (rest (path mem2 iset))))
              (or (not dwords1) (not dwords2))  []
              :else (let [clos1 (closure mem1 dwords1)
                          clos2 (closure mem2 dwords2)]
                      (recur (merge clos1 mem1) (keys clos1)
                             (merge clos2 mem2) (keys clos2))))))))


