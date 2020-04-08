(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is true))
  (testing "queens are higher rank than jacks"
    (is (> (rrank :queen) (rrank :jack))))
  (testing "kings are higher rank than queens"
    (is (> (rrank :king) (rrank :queen))))
  (testing "aces are higher rank than kings"
    (is (> (rrank :ace) (rrank :king)))
    (testing "same rank is a :tie"
      (is (= :tie (play-round [:ace 4] [:spade 4]))))
    (testing "win player card higher card:"
      (is (= :player1 (play-round [:club 8] [:spade 2]))))))

(deftest test-play-game
  (testing "win player1"
    (is (= :player1 (play-game [[:spade 1]
                                [:club 2]
                                [:club  4]
                                [:heart 4]
                                [:spade 5]
                                [:diamond 6]
                                [:heart 9]
                                [:spade 8]]
                               [[:spade 2]
                                [:spade 3]
                                [:diamond 4]
                                [:club 5]
                                [:heart 4]
                                [:club 7]
                                [:club 8]
                                [:club 3]]))))
  (testing "win player2 (same test as above swapped)"
    (is (= :player2 (play-game [[:spade 2]
                                [:spade 3]
                                [:diamond 4]
                                [:club 5]
                                [:heart 4]
                                [:club 7]
                                [:club 8]
                                [:club 3]]
                               [[:spade 1]
                                [:club 2]
                                [:club  4]
                                [:heart 4]
                                [:spade 5]
                                [:diamond 6]
                                [:heart 9]
                                [:spade 8]]))))

  (testing "real cards tested game"
    (is (= :player2 (play-game '([:club :king]
                                 [:club :jack]
                                 [:spade 7]
                                 [:club :queen]
                                 [:spade 6]
                                 [:heart 7]
                                 [:heart 3]
                                 [:spade :jack]
                                 [:heart 9]
                                 [:club 2]
                                 [:diamond 8]
                                 [:diamond 4]
                                 [:diamond 7]
                                 [:club 7]
                                 [:heart :jack]
                                 [:club :ace]
                                 [:spade 3]
                                 [:spade 9]
                                 [:diamond :queen]
                                 [:club 8]
                                 [:diamond 2]
                                 [:diamond 6]
                                 [:heart 10]
                                 [:spade 8]
                                 [:diamond 10] [:spade :queen])
                               '([:spade :king]
                                 [:spade 4]
                                 [:spade 10]
                                 [:heart 8]
                                 [:club 4]
                                 [:club 3]
                                 [:diamond 9]
                                 [:heart :queen]
                                 [:heart 4]
                                 [:club 10]
                                 [:diamond :ace]
                                 [:diamond 3]
                                 [:club 9]
                                 [:heart :king]
                                 [:heart 5]
                                 [:diamond :king]
                                 [:diamond 5]
                                 [:club 5]
                                 [:heart 2]
                                 [:club 6]
                                 [:spade 5]
                                 [:heart 6]
                                 [:spade 2]
                                 [:spade :ace]
                                 [:heart :ace]
                                 [:diamond :jack]))))))

