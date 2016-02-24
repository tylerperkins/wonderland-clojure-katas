 (ns graph)


(defn- append-if-adjacent
  "Among the given paths (vectors of nodes), finds those whose last node is
  adjacent to the given node. The given node is appended to each such path,
  and a seq of these extended paths is returned.
  "
  [adjacent paths node]
  (->> paths
       ; Choose only paths whose last node is adjacent to the given node.
       (filter (comp (partial adjacent node) last))
       ; Append the adj. node to each.
       (map #(conj % node))))


(defn- append-paths
  "Returns a seq of the given paths extended by the given nodes, when such a
  node is adjacent to the last node of such a path. None of the given paths
  whose last node is NOT adjacent to one of the given nodes is returned. The
  resulting paths are then exacty one node longer than those in the given
  paths. It is possible for more than one resulting path to be extended by the
  same node. So the following inequality always holds:
  (<= 0 num_returned_paths (* (count nodes) (count paths))
  "
  [adjacent nodes paths]
  (mapcat (partial append-if-adjacent adjacent paths) nodes))


(defn shortest-paths
  "Given an adjacency function, a set of nodes, a starting node and a target
  node, returns a seq of all the minimal paths (vectors of nodes) starting with
  start-node and ending with target-node. The adjacency function must take two
  nodes and return a truthy value iff the first is \"adjacent\" to the second,
  however you define what it means to be a node and whether or not two are
  adjacent. If there is no path between start-node and target-node, nil is
  returned.
  "
  [adjacent all-nodes start-node target-node]
  (loop [nodes all-nodes          ; Set of nodes that no path yet contains.
         paths [[start-node]]     ; Seq of vectors of nodes. Each such path
                                  ;   starts with start-node.
         ]
    (let [complete-paths (filter (comp (partial = target-node) last) paths)
          ; complete-paths are just the paths that have reached target-node.
          ]
      (if (or (not-empty complete-paths) (empty? paths))

        ; Done. We have reached target-node at least once, or no path has
        ; succeeded. Notice in the latter case that if paths is empty, so is
        ; complete-paths, and we return the empty seq ().
        ;
        complete-paths

        ; Else, we must try to append a node to the paths we have so far.
        ;
        (let [unvisited-nodes (apply disj nodes (map last paths))
              ; Here, we're attempting to append only nodes that don't already
              ; appear in any path in paths. Otherwise, any path constructed to
              ; intersect another path or loop back on its way to target-node
              ; would necessarily be longer than than others reaching
              ; target-node.
              ]
          (recur unvisited-nodes
                 (append-paths adjacent unvisited-nodes paths)))))))
                 ; All paths are the same length, since each has grown by 1
                 ; node on each recur, or has been removed.

