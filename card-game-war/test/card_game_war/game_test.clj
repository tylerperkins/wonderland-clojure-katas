(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


(def c00 (card 2 :spades))
(def c01 (card 3 :spades))
(def c50 (card :king :hearts))
(def c51 (card :ace :hearts))


(deftest test-card-defs
  (testing "Cards have ordered values associated with their suits and ranks."
    (are [rel val-1         val-2]  (rel val-1 val-2)
          =   c00           {:rank 2    :suit :spades :value 0}
          =   c51           {:rank :ace :suit :hearts :value 51}
          <   (:value c00)  (:value c01)
          <   (:value c00)  (:value (card 2 :clubs))
      ))

  (testing "Deck should have 52 distinct cards with values 0 through 51."
    (is (= (count deck) 52))
    (is (= (count (set deck)) 52))
    (is (->> deck
             (map :value)
             sort
             ((juxt first last))
             (= [0 51])))
    )

  (testing "Should deal the required hands."
    (let [hand-size   5
          too-many    18
          players     [:player-1 :player-2 :player-3]
          state       (deal hand-size players)
          dealt-cards (reduce concat (vals state))
          ]
      (is (every? (fn [[_ hand]] (= hand-size (count hand))) state))
      (is (= (count dealt-cards)
             (count (set dealt-cards))
             (* hand-size (count players))))
      (is (thrown? AssertionError (deal too-many players))))
    )
  )


(deftest test-play-round

  (letfn [(beats [card-1 card-2]
            (> (:value card-1) (:value card-2)))
          ]
    (testing "queens are higher rank than jacks"
      (is (beats (card :queen :spades) (card :jack :spades))))
    (testing "kings are higher rank than queens"
      (is (beats (card :king :spades) (card :queen :spades))))
    (testing "aces are higher rank than kings"
      (is (beats (card :ace :spades) (card :king :spades))))
    (testing "if the ranks are equal, clubs beat spades"
      (is (beats (card :ace :clubs) (card :ace :spades))))
    (testing "if the ranks are equal, diamonds beat clubs"
      (is (beats (card 2 :diamonds) (card 2 :clubs))))
    (testing "if the ranks are equal, hearts beat diamonds"
      (is (beats (card 10 :hearts) (card 10 :diamonds)))))

  (testing "the highest rank wins the cards in the round"
    (let [state-0   {:player-1 [c00 c51]          :player-2 [c50 c01]}
          state-1   {:player-1 [c51]              :player-2 [c01 c50 c00]}
          state-2   {:player-1 [c51 c01]          :player-2 [c50 c00]}
          state-3   {:player-1 [c01 c51 c50]      :player-2 [c00]}
          state-4   {:player-1 [c51 c50 c00 c01]  :player-2 []}
          play-1-2  (constantly [:player-1 :player-2])
          play-2-1  (constantly [:player-2 :player-1])
          ]
      (is (= (play-round play-2-1 {:state state-0})
             {:state state-1
              :winner :player-2
              :plays (array-map :player-2 c50 :player-1 c00)}))
      (is (= (play-round play-1-2 {:state state-1})
             {:state state-2
              :winner :player-1
              :plays (array-map :player-1 c51 :player-2 c01)}))
      (is (= (play-round play-1-2 {:state state-2})
             {:state state-3
              :winner :player-1
              :plays (array-map :player-1 c51 :player-2 c50)}))
      (is (= (play-round play-2-1 {:state state-3})
             {:state state-4
              :winner :player-1
              :plays (array-map :player-2 c00 :player-1 c01)}))
      (is (= (play-round play-2-1 {:state state-4}) nil))
      ))
  )


(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [state-0   {:player-1 [c00 c51]          :player-2 [c50 c01]}
          state-end {:player-1 [c51 c50 c01 c00]  :player-2 []}
          {{:keys [player-1 player-2]} :state}
                    (->> state-0
                         (play-game (comp shuffle keys))
                         last)
          ]
      (is (= (set player-1) (set (:player-1 state-end))))
      (is (= (set player-2) (set (:player-2 state-end)))))
    ))


(deftest test-main
  (testing "A game should give desired results if shuffling is not random"
    (let [out-writer (java.io.StringWriter.)
          ]
      (binding [*out* out-writer
                shuf  identity
                ]
        (-main "3" "Foo" "Bar"))
      (is (= (.toString out-writer)
             (str "  Foo played the 2 of spades.\n"
                  "  Bar played the 5 of spades.\n"
                  "Bar won! Cards remaining: Foo 2, Bar 4\n"
                  "  Foo played the 3 of spades.\n"
                  "  Bar played the 6 of spades.\n"
                  "Bar won! Cards remaining: Foo 1, Bar 5\n"
                  "  Foo played the 4 of spades.\n"
                  "  Bar played the 7 of spades.\n"
                  "Bar won! Cards remaining: Foo 0, Bar 6\n"))))
    ))
