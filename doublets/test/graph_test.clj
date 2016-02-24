 (ns graph-test
   (:require [clojure.test :refer :all]
             graph))


(defn adj-fn
  "An adjacency function whose nodes are keywords.
  "
  [node1 node2]
  (#{#{:n0 :n1} #{:n0 :n2} #{:n1 :n3}
     #{:n2 :n3} #{:n2 :n4} #{:n5 :n6}}
    #{node1 node2}))


(deftest shortest-paths-test

  (testing "Paths of nodes should be extended for each adjacent node, if present."
    (are [paths                                       nodes
          result-paths                                result-nodes
          ] (let [new-paths (@#'graph/append-paths adj-fn nodes paths)
                  new-nodes (apply disj nodes (map last new-paths))
                  ]
              (and (= (set new-paths) (set result-paths))
                   (= new-nodes result-nodes)))

          [[:n0]]                                     #{:n1 :n2 :n3 :n4 :n5 :n6}
          [[:n0 :n1] [:n0 :n2]]                       #{:n3 :n4 :n5 :n6}  ; Grow ...

          [[:n0 :n1] [:n0 :n2]]                       #{:n3 :n4 :n5 :n6}
          [[:n0 :n1 :n3] [:n0 :n2 :n3] [:n0 :n2 :n4]] #{:n5 :n6}          ; ... grow

          [[:n0 :n1 :n3] [:n0 :n2 :n3] [:n0 :n2 :n4]] #{:n5 :n6}
          []                                          #{:n5 :n6}          ; ... done!

          []                                          #{:n5 :n6}
          []                                          #{:n5 :n6}          ; Still done.

          []                                          #{:n5 :n6}
          []                                          #{:n5 :n6}          ; No error.
      ))

  (testing "All min-length paths between given nodes should be found."
    (are [start target paths]  (= paths
                                  (set (graph/shortest-paths
                                         adj-fn
                                         #{:n0 :n1 :n2 :n3 :n4 :n5 :n6}
                                         start
                                         target)))
          ; Never have path to or from an unknown node ...
          :xxxx :n0    #{}
          :n0   :xxxx  #{}

          ; ... unless target == start, whether node is known or not.
          :n0   :n0    #{[:n0]}
          :xxxx :xxxx  #{[:xxxx]}

          ; No path from :n0 to :n6.
          :n0   :n6    #{}

          ; Two paths from :n0 to :n3 ...
          :n0   :n3    #{[:n0 :n1 :n3] [:n0 :n2 :n3]}

          ; ... in both directions.
          :n3   :n0    #{[:n3 :n1 :n0] [:n3 :n2 :n0]}

          ; Longer paths. Intermediate paths are not returned.
          :n4   :n1    #{[:n4 :n2 :n3 :n1] [:n4 :n2 :n0 :n1]}
      ))
  )
