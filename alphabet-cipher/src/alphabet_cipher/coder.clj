(ns alphabet-cipher.coder)

(def base (int \a))
(def alphabet-count 26)

(defn char->int [c] (- (int c) base))

(defn oper
  "calculate op(+ or -) between two characters mod the alphabet size"
  [op c1 c2]
  (-> (op (char->int c1) (char->int c2))
      (+ alphabet-count)
      (mod alphabet-count)
      (+ base)
      (char)))

(defn process
  "find in the cipher table the encrypted message if op is +
   and if op is - find the cipher or the code depending in the order of the args"
  [op keyword message]
  (apply str (map (partial oper op) message (cycle keyword))))

(defn- phi* [s ph k c]
  (last (take-while identity
                    (iterate
                     #(when (and (> % 0)
                                 (not= (get s %) c))
                        (ph (dec %))) k))))

(defn phi
  "returns kmp phi vector"
  [s]
  (reduce (fn [[vphi k] c]
            (let [k (phi* s vphi k c)
                  inck (if (= (get s k) c)
                         (inc k) k)]
              [(conj vphi inck)  inck])) [[0] 0] (rest s)))

(defn encode [keyword message]
  (process + keyword message))

(defn decode [keyword message]
  (process - keyword message))

(defn decipher
  "solution is the prefix of the decoded key with size 'n - phi[n - 1]'
    where n is the size of the decoded key"
  [cipher message]
  (let [sol (process - message cipher)
        prefix (- (count sol) (last (phi sol)))]
    (apply str (take prefix sol))))
