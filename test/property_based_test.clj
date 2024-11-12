(ns property-based-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [avl-tree-set :refer [avl-empty avl-add avl-balanced? avl-delete
                         avl-merge avl-equals?]]))

(defspec monoid-neutral-element-prop 100
  (prop/for-all [v (gen/set gen/string)]
                (let [tree (reduce avl-add (avl-empty) v)]
                  (and
                   (avl-equals? tree (avl-merge (avl-empty) tree))
                   (avl-equals? tree (avl-merge tree (avl-empty)))))))

(defspec monoid-associative-prop 100
  (prop/for-all [v1 (gen/set gen/small-integer)
                 v2 (gen/set gen/small-integer)
                 v3 (gen/set gen/small-integer)]
                (let [tree1 (reduce avl-add (avl-empty) v1)
                      tree2 (reduce avl-add (avl-empty) v2)
                      tree3 (reduce avl-add (avl-empty) v3)
                      merged1 (avl-merge (avl-merge tree1 tree2) tree3)
                      merged2 (avl-merge tree1 (avl-merge tree2 tree3))]
                  (avl-equals? merged1 merged2)
                  (avl-balanced? merged1)
                  (avl-balanced? merged2))))

(defspec add-and-delete-prop 100
  (prop/for-all [set (gen/set gen/small-integer)
                 v (gen/set gen/small-integer)]
                (let [tree (reduce avl-add (avl-empty) set)
                      added (avl-add tree (first v))
                      deleted (avl-delete tree (first v))]
                  (avl-equals? tree deleted)
                  (avl-balanced? added)
                  (avl-balanced? deleted))))

