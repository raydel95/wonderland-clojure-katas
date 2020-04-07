(ns alphabet-cipher.coder)

(defn oper [op c1 c2] (let [a (int \a)
                         f (fn [v] (- (int v) a))]
                     (char (+ a (+ (mod (+ (op (f c1) (f c2)) 26) 26))))))

(defn process [f keyword message] (apply str (map (partial oper f)  message (cycle keyword))))

(defn movek [s ph k c]
  (last (take-while identity
                    (iterate
                     #(when (and (> % 0)
                                 (not= (get s %) c))
                        (ph (dec %))) k))))

(defn phi [s]
  (reduce (fn [[vphi k] c]
            (let [k (movek s vphi k c)
                  inck (if (= (get s k) c)
                         (inc k) k)]
              [(conj vphi inck)  inck])) [[0] 0] (rest s)))

(defn encode [keyword message]
  (process + keyword message))

(defn decode [keyword message]
  (process - keyword message))

(defn decipher [cipher message]
  (let [sol (process - message cipher)
        pref (- (count sol) (last (phi sol)))]
    (apply str (take pref sol))))
