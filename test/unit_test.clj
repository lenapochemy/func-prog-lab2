(ns unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [avl-tree-set :refer [->Node avl-empty avl-add avl-node-height
                                  avl-update-node-height avl-contains?
                                  avl-balance avl-balanced? avl-delete
                                  avl-right-fold avl-left-fold
                                  avl-right-rotate avl-left-rotate
                                  avl-map avl-size avl-filter avl-merge]]))

(def node2 (->Node 2 nil nil 1))
(def node10 (->Node 10 nil nil 1))
(def node7 (->Node 7 nil node10 2))
(def tree-node3 (->Node 3 node2 node7 3))

(def tree (-> (avl-empty)
              (avl-add 0)
              (avl-add 15)
              (avl-add 20)
              (avl-add 25)
              (avl-add 23)
              (avl-add 28)))

(deftest avl-empty-test
  (testing "Empty avl tree"
    (is (nil? (avl-empty)))))

(deftest avl-node-height-test
  (testing "Node height"
    (is (zero? (avl-node-height nil)))
    (is (= 3 (avl-node-height (->Node 4 nil nil 3))))
    (is (= 0 (avl-node-height (avl-empty))))))

(deftest avl-update-node-height-test
  (testing "Update node height"
    (is (zero? (avl-update-node-height nil)))
    (is (= 3 (:height (avl-update-node-height tree-node3))))
    (is (= 2 (:height (avl-update-node-height node7))))))

(deftest avl-contains-test
  (testing "Contains element"
    (is (true? (avl-contains? tree-node3 3)))
    (is (true? (avl-contains? tree-node3 2)))
    (is (true? (avl-contains? tree-node3 7)))
    (is (true? (avl-contains? tree-node3 10)))
    (is (false? (avl-contains? tree-node3 5)))
    (is (false? (avl-contains? nil 6)))))

(deftest avl-add-test
  (testing "Adding elements on tree"
    (let [tree (-> (avl-empty)
                   (avl-add 3)
                   (avl-add 2)
                   (avl-add 7))]
      (is (= true (avl-contains? tree 3)))
      (is (= true (avl-contains? tree 2)))
      (is (= true (avl-contains? tree 7)))
      (is (= false (avl-contains? tree 5)))
      (is (= true (avl-balanced? tree))))
    (is (= 5 (:value (:left (:right (avl-add tree-node3 5))))))))

(deftest avl-delete-test
  (testing "Deleting and contains elements tree"
    (let [tree (-> (avl-empty)
                   (avl-add 3)
                   (avl-add 2)
                   (avl-add 7)
                   (avl-delete 2))]
      (is (= true (avl-contains? tree 3)))
      (is (= false (avl-contains? tree 2)))
      (is (= true (avl-contains? tree 7)))
      (is (= false (avl-contains? tree 5)))
      (is (= true (avl-balanced? tree))))))

(deftest avl-right-rotate-test
  (testing "Right rotate"
    (let [node-a (->Node 'a' nil nil 1)
          node-b (->Node 'b' nil nil 1)
          node-c (->Node 'c' nil nil 1)
          node-q (->Node 'q' node-a node-b 2)
          node-p (->Node 'p' node-q node-c 3)
          rotated (avl-right-rotate node-p)]
      (is (= 'q' (:value rotated)))
      (is (= 'p' (:value (:right rotated))))
      (is (= 'a' (:value (:left rotated))))
      (is (= 'c' (:value (:right (:right rotated)))))
      (is (= 'b' (:value (:left (:right rotated))))))))

(deftest avl-left-rotate-test
  (testing "Left rotate"
    (let [node-a (->Node 'a' nil nil 1)
          node-b (->Node 'b' nil nil 1)
          node-c (->Node 'c' nil nil 1)
          node-p (->Node 'p' node-b node-c 2)
          node-q (->Node 'q' node-a node-p 3)
          rotated (avl-left-rotate node-q)]
      (is (= 'p' (:value rotated)))
      (is (= 'c' (:value (:right rotated))))
      (is (= 'q' (:value (:left rotated))))
      (is (= 'b' (:value (:right (:left rotated)))))
      (is (= 'a' (:value (:left (:left rotated))))))))

(deftest avl-map-test
  (testing "Map"
    (let [mapped (avl-map tree-node3 inc)]
      (is (= 4 (:value mapped)))
      (is (= 3 (:value (:left mapped))))
      (is (= 8 (:value (:right mapped))))
      (is (= 11 (:value (:right (:right mapped))))))))

(deftest avl-balance-test
  (testing "Balance"
    (let [node-1 (->Node 1 nil nil 1)
          node-3 (->Node 3 nil nil 1)
          node-5 (->Node 5 nil nil 1)
          node-7 (->Node 7 nil nil 1)
          node-4 (->Node 4 node-3 node-5 2)
          node-6 (->Node 6 node-4 node-7 3)
          node-2 (->Node 2 node-1 node-6 4)
          balanced-tree (avl-balance node-2)]
      (is (= 4 (:value balanced-tree)))
      (is (= 2 (:value (:left balanced-tree))))
      (is (= 6 (:value (:right balanced-tree))))
      (is (= 1 (:value (:left (:left balanced-tree)))))
      (is (= 5 (:value (:left (:right balanced-tree)))))
      (is (= 3 (:value (:right (:left balanced-tree)))))
      (is (= 7 (:value (:right (:right balanced-tree))))))))

(deftest avl-size-test
  (testing "Size"
    (is (zero? (avl-size nil)))
    (is (= 4 (avl-size tree-node3)))
    (let [tree (-> (avl-empty)
                   (avl-add 3)
                   (avl-add 2)
                   (avl-add 7)
                   (avl-delete 2))]
      (is (= 2 (avl-size tree))))))

(deftest avl-filter-test
  (testing "Filter"
    (let [filtered (avl-filter tree even?)]
      (is (= 3 (avl-size filtered)))
      (is (= 20 (:value filtered)))
      (is (= 0 (:value (:left filtered))))
      (is (= 28 (:value (:right filtered))))
      (is (nil? (:right (:right filtered))))
      (is (nil? (:left (:right filtered))))
      (is (nil? (:right (:left filtered))))
      (is (nil? (:left (:left filtered)))))))

(deftest avl-right-fold-test
  (testing "Right fold"
    (is (= 111 (avl-right-fold 0 tree +)))
    (is (= 0 (avl-right-fold 1 tree *)))
    (is (= 45 (avl-right-fold 45 nil +)))
    (is (nil? (avl-right-fold nil tree +)))))

(deftest avl-left-fold-test
  (testing "Left fold"
    (is (= 111 (avl-left-fold 0 tree +)))
    (is (= 0 (avl-left-fold 1 tree *)))
    (is (= 45 (avl-left-fold 45 nil +)))
    (is (nil? (avl-left-fold nil tree +)))))

(deftest avl-merge-test
  (testing "Merge"
    (let [node1 (-> (avl-empty)
                    (avl-add 0)
                    (avl-add 15)
                    (avl-add 20))
          node2 (-> (avl-empty)
                    (avl-add 25)
                    (avl-add 23)
                    (avl-add 28))
          merged (avl-merge node1 node2)]
      (is (true? (avl-balanced? merged)))
      (is (= 23 (:value merged)))
      (is (= 15 (:value (:left merged))))
      (is (= 0 (:value (:left (:left merged)))))
      (is (= 20 (:value (:right (:left merged)))))
      (is (nil? (:left (:left (:left merged)))))
      (is (nil? (:right (:left (:left merged)))))
      (is (nil? (:left (:right (:left merged)))))
      (is (nil? (:right (:right (:left merged)))))
      (is (nil? (:left (:right merged))))
      (is (= 25 (:value (:right merged))))
      (is (= 28 (:value (:right (:right merged)))))
      (is (nil? (:left (:right (:right merged)))))
      (is (nil? (:right (:right (:right merged))))))))