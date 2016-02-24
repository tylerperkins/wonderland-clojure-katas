(ns card-game-war.game
  (:require [clojure.edn :as edn]
            [clojure.string :as string]))


(def suits [:spades :clubs :diamonds :hearts])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def ^:private suit-values
  (apply hash-map (interleave suits (range))))
(def ^:private rank-values
  (apply hash-map (interleave ranks (range))))


(defn card
  "Returns a map representing a playing card. Besides :rank and :suit, it
  includes key :value, whose unique value defines the order of every card in
  the deck.
  "
  [rank suit]
  (let [s (suit-values suit)
        r (rank-values rank)
        ]
    {:rank rank, :suit suit, :value (+ (* s (count ranks)) r)}))


(def deck
  (for [suit suits, rank ranks]
    (card rank suit)))


(def ^:dynamic shuf
  "A shuffle function that can be redefined using clojure.core/binding.
  This is done by test-main so that results are predictable.
  "
  shuffle)


(defn deal
  "Returns the game's initial \"state\" map. Its keys are the given players,
  which is a collection of keywords, each associated with a seq of hand-size
  cards, all drawn from a single shuffled deck. The total number of cards
  drawn must not exceed the size of the deck or an AssertionError will be
  thrown.
  "
  [hand-size players]
  (assert (<= 0 (* hand-size (count players)) (count deck))
          (str "The product of hand size and the number of players "
               "must be non-negative and no larger than "
               (count deck)
               "."))
  (into {} (map vector players (partition hand-size (shuf deck)))))


(defn play-round
  "Given a player-order function indicating the order in which players
  reveal their top cards, this function transforms the given map containing
  a :state key and associated state map into a new map containing the new
  :state array map resulting from a single round of play.

  The player-order function must take a state (keyword-to-card-seq map) and
  return a seq of keywords indicating the order in which cards are to be
  revealed in this round. On return from play-round, the order of the resulting
  :state array map's keys will match the seq returned by player-order.

  If only one player has any cards left, then nil is returned, since that
  player has won the game.

  Supplimental information about this round is also included. Keyword :winner
  maps to the player (a keyword) who won the round. Keyword :plays maps to an
  array map from the player keywords to the card each player played, in
  player-order.
  "
  [player-order {old-state :state}]
  (when (->> old-state
             (filter (comp not-empty second))
             count
             (< 1))
    (let [players     (player-order old-state)
          plays       (->> old-state
                           ((apply juxt players))
                           (map first)
                           (map vector players)
                           (filter second)
                           (apply concat)
                           (apply array-map))
          [winner _]  (apply max-key (comp :value second) plays)
          new-state   (as-> old-state s
                            (map (fn [[player hand]] [player (rest hand)]) s)
                            (into {} s)
                            (update s winner concat (vals plays)))
          ]
      {:state new-state, :winner winner, :plays plays})))


(defn play-game
  "Given a player-order function (as in play-round, above) and an initial
  (player-keyword-to-card-seq) state map, this function returns a lazy seq of
  maps returned by play-round indicating the result of each round. The last
  such map indicates the winner of the game.
  "
  [player-order init-state]
  (->> {:state init-state}
       (iterate (partial play-round player-order))          ; Evolve the state.
       rest                                                 ; Omit init-state.
       (take-while identity)))                              ; Go until a win.


(defn -main
  "
NAME
       game.clj - Simulates the card game War.

SYNOPSIS
       lein run HAND_SIZE PLAYER_1 ... PLAYER_N
       lein run

DESCRIPTION
      This program simulates War by virtually dealing HAND_SIZE cards to each
      of the players indicated by the given names, which must be legal as
      Clojure symbols. The cards come from a single shuffled deck, so
      (HAND_SIZE * N) must be no bigger than 52. The result of each play is
      printed until only one player has cards, who is the winner.

      When called with no arguments, this doc string is printed.

SEE ALSO
      README.md in this directory for rules of the game. Additionally, the
      order in which players reveal their top card is random, and these cards
      are appended to the winner's hand in that order.
  "
  [& [hand-size-str & player-names]]
  (if (nil? hand-size-str)

    ;;  Called with no args. Print usage info (just the doc string above).
    (println (:doc (meta (var -main))))

    ;;  Else play a game, printing cards played and winner of each round.
    (let [players   (map keyword player-names)
          hand-size (edn/read-string hand-size-str)
          game      (->> players                            ; Note that game is
                         (deal hand-size)                   ; a lazy seq, so
                         (play-game (comp shuf keys)))      ; play-round is not
          ]                                                 ; called until
      (doseq [{:keys [state winner plays]} game             ; <-- here.
              ]
        (doseq [[player {:keys [rank suit]}] plays
                ]
          (println (str "  "
                        (name player)
                        " played the "
                        (if (keyword? rank) (name rank) (str rank))
                        " of "
                        (name suit)
                        ".")))
        (->> state
             player                                         ; Note crazy order:
             count                                          ;
             (str (name player) " ")                        ; player defined
             (for [player players])                         ; <-- here!
             (string/join ", ")
             (println (name winner) "won! Cards remaining:"))))))

