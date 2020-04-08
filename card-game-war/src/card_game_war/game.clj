(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn rrank [r]
  (.indexOf ranks r))

(defn play-round [[_ rank1] [_ rank2]]
  (cond (> (rrank rank1) (rrank rank2)) :player1
        (< (rrank rank1) (rrank rank2)) :player2
        :else :tie))

(defn play-game [player1-cards player2-cards]
  (loop [acc [] [card1 & deck1] player1-cards  [card2 & deck2] player2-cards]
    (cond
      (not card1) :player2
      (not card2) :player1
      :else
      (let [result (play-round card1 card2)]
        (cond
          (= result :tie) (recur (concat acc [card1 card2] (take 3 deck1) (take 3 deck2))
                                 (drop 3 deck1)
                                 (drop 3 deck2))
          (= result :player1) (recur [] (concat deck1 acc [card1 card2]) deck2)
          (= result :player2) (recur []  deck1 (concat deck2 acc [card2 card1])))))))


