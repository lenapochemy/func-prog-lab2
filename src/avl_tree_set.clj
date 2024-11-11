(ns avl-tree-set)

(defrecord Node [value left right height])

(defrecord AVL-Tree [root])

(defn node-height [node]
  (if (nil? node) 0 (:height node)))

(defn update-node-height [node]
  (let [new-height (inc (max (node-height (:left node))
                             (node-height (:right node))))]
    (assoc node :height new-height)))

(defn balance-factor [node]
  (- (node-height (:left node)) (node-height (:right node))))

(defn right-rotate [node]
  (let [left-node (:left node)
        new-node (update-node-height (assoc node :left (:right left-node)))]
    (update-node-height (assoc left-node :right new-node))))

(defn left-rotate [node]
  (let [right-node (:right node)
        new-node (update-node-height (assoc node :right (:left right-node)))]
    (update-node-height (assoc right-node :left new-node))))

(defn balance [node]
  (let [bf (balance-factor node)]
    (cond
      (> bf 1) (if (>= (balance-factor (:left node)) 0)
                 (right-rotate node)
                 (right-rotate (assoc node :left (left-rotate (:left node)))))
      (< bf -1) (if (<= (balance-factor (:right node)) 0)
                  (left-rotate node)
                  (left-rotate (assoc node :right (right-rotate (:right node)))))
      :else (update-node-height node))))

(defn add [node value]
  (if (nil? node)
    (->Node value nil nil 1)
    (let [diff (compare value (:value node))]
      (cond
        (zero? diff) (node)
        (neg? diff) (balance (assoc node :left (add (:left node) value)))
        ;; pos?
        :else (balance (assoc node :right (add (:right node) value)))))))

(defn min-value-node [node]
  (if (nil? (:left node))
    node
    (recur (:left node))))

(defn max-value-node [node]
  (if (nil? (:right node))
    node
    (recur (:right node))))

(defn avl-delete [node value]
  (if (nil? node) nil
      (let [diff (compare value (:value node))]
        (cond
          (neg? diff) (balance (assoc node :left (avl-delete (:left node) value)))
          (pos? diff) (balance (assoc node :right (avl-delete (:right node) value)))
      ;; zero?
          :else (cond
                  (nil? (:left node)) (:right node)
                  (nil? (:right node)) (:left node)
                  :else (let [min-node (min-value-node (:right node))]
                          (-> min-node
                              (assoc :right (avl-delete (:right node) (:value min-node)))
                              (assoc :left (:left node))
                              (balance))))))))

(defn avl-contains? [node value]
  (if (nil? node) false
      (let [diff (compare value (:value node))]
        (cond
          (neg? diff) (recur (:left node) value)
          (pos? diff) (recur (:right node) value)
          :else true))))

(defn empty-avl []
  nil)

(defn size [node]
  (if (nil? node) 0
      (inc (+ (size (:left node)) (size (:right node))))))

(defn balanced? [node]
  (if (nil? node) true
      (let [bf (balance-factor node)]
        (cond
          (> bf 1) false
          (< bf -1) false
          :else (and (balanced? (:left node)) (balanced? (:right node)))))))

(defn avl-map [node func]
  (when node
    (let [left (avl-map (:left node) func)
          right (avl-map (:right node) func)
          new-value (func (:value node))]
      (assoc node :value new-value :left left :right right))))

(defn left-fold [acc node func]
  (if (nil? node) acc
      (-> acc
          (left-fold (:left node) func)
          (func (:value node))
          (left-fold (:right node) func))))

(defn right-fold [acc node func]
  (if (nil? node) acc
      (-> acc
          (right-fold (:right node) func)
          (func (:value node))
          (right-fold (:left node) func))))

(defn avl-merge [node1 node2]
  (cond
    (nil? node1) node2
    (nil? node2) node1
    :else (left-fold node1 node2 add)))

(defn avl-filter [node func]
  (if (nil? node) nil
      (let [left (avl-filter (:left node) func)
            right (avl-filter (:right node) func)]
        (if (func (:value node))
          (balance (assoc node :left left :right right))
          (avl-merge left right)))))

(defn -main []
  ;; ;; test for right rotate
  ;; (def node-a (->Node 'a' nil nil 1))
  ;; (def node-b (->Node 'b' nil nil 1))
  ;; (def node-q (->Node 'q' node-a node-b 2))
  ;; (def node-c (->Node 'c' nil nil 1))
  ;; (def node-p (->Node 'p' node-q node-c 3))
  ;; (right-rotate node-p)

  ;; ;; test for left rotate 
  ;; (def in-right (right-rotate node-p))
  ;; (left-rotate in-right)

  ;; test for balance
  ;; (def node-a (->Node 'a' nil nil 1))
  ;; (def node-b (->Node 'b' nil nil 1))
  ;; (def node-c (->Node 'c' nil nil 1))
  ;; (def node-d (->Node 'd' nil nil 1))

  ;; (def node-s (->Node 's' node-b node-c 2))
  ;; (def node-q (->Node 'q' node-s node-d 3))
  ;; (def node-p (->Node 'p' node-a node-q 4))

  ;; (balance node-p)

  ;; ;; test for add
  (def node-10 (->Node 10 nil nil 1))
  (def node-2 (->Node 2 nil nil 1))
  (def node-7 (->Node 7 nil node-10 2))
  (def node-3 (->Node 3 node-2 node-7 3))

  ;; ;; (add node-3 5)

  (def added-node (add node-3 5))

  ;; ;; test for delete
  ;; (avl-delete added-node 7)

  ;; ;; test for contains?
  ;; (avl-contains? added-node 3)
  ;; (avl-contains? added-node 14)
  ;; (avl-contains? added-node 2)
  ;; (avl-contains? added-node 10)
  ;; (avl-contains? added-node 5)

  ;; (size node-3)

  ;; ;; (balanced? (->Node 2 nil added-node 1))
  ;; (min-value-node added-node)
  ;; (max-value-node added-node)
  ;; (avl-map added-node inc)

  (left-fold 0 added-node +)
  (print added-node)

  (def node-20 (->Node 20 nil nil 1))
  (def node-0 (->Node 0 nil nil 1))
  (def node-15 (->Node 15 node-0 node-20 2))
  (def node-23 (->Node 23 nil nil 1))
  (def node-28 (->Node 28 nil nil 1))
  (def node-25 (->Node 25 node-23 node-28 2))

  (avl-merge node-15 node-25)
  (avl-merge node-25 node-15)

  (print node-23)
  (left-fold 0 node-15 +)

  (left-fold node-15 node-23 add)

  (def new-tree (avl-merge node-15 node-25))
  (avl-filter new-tree (fn [x] (and (< 19 x) (< x 25)))))


