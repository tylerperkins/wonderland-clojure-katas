(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))


(deftest solver-test

  (testing "Words are linked iff they differ by one character."
    (are [w1        w2          result]     (= result (linked w1 w2))
          ""        ""          false
          "head"    "he"        false
          "he"      "head"      false
          "head"    "head"      false
          "head"    "heal"      true
          "book"    "look"      true
          "breed"   "bread"     true
          "breed"   "bleed"     true
      )
    )

  (testing "Succeeds when word links found."
    (are [w1        w2          result]     (= result (doublets w1 w2))
          "head"    "tail"      ["head" "heal" "teal" "tell" "tall" "tail"]
          "door"    "lock"      ["door" "boor" "book" "look" "lock"]
          "bank"    "loan"      ["bank" "bonk" "book" "look" "loon" "loan"]
          "wheat"   "bread"     ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
      )
    )

  (testing "Succeeds when no word links found."
    (are [w1        w2]       (empty? (doublets w1 w2))
          "ye"      "freezer"
          "ye"      "yo"
      )
    )

  )
