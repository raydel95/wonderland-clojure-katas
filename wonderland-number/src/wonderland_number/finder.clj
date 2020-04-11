(ns wonderland-number.finder)

(defn valid? [n] (apply = (map (comp set str)
                      (cons n (map #(* % n) (range 2 7))))))

(defn wonderland-number []
  (first (filter valid? (range 100000 200000))))

(wonderland-number)
