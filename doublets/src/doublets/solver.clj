(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            graph))


(def words
  "A set (not just a seq) of strings that serve as nodes in a graph of
  \"linked\" words.
  "
  (-> "words.edn"
      io/resource
      slurp
      edn/read-string
      set))


(defn linked
  "A function answering whether or not the given words are adjacent. The string
  arguments are considered adjacent iff they are the same length and differ by
  exactly one character.
  "
  [word1 word2]
  (and (= (count word1) (count word2))
       (->> (map not= word1 word2)
            (filter identity)
            count
            (= 1))))


(defn doublets
  "Returns a vector of successively linked words from first-word to last-word.
  See file README.md.
  "
  [first-word last-word]
  (first (graph/shortest-paths linked words first-word last-word)))

