(ns avl-tree-set)

(defrecord Node [value left right height])

(defn node-height [node]
  (if (nil? node) 0 (:height node)))

(defn update-node-height [node]
  (if (nil? node) 0
      (let [new-height (inc (max (node-height (:left node))
                                 (node-height (:right node))))]
        (assoc node :height new-height))))

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
  (if (nil? node) nil
      (let [left (avl-map (:left node) func)
            right (avl-map (:right node) func)
            new-value (func (:value node))]
        (assoc node :value new-value :left left :right right))))

(defn left-fold [acc node func]
  (cond
    (nil? node) acc
    (nil? acc) nil
    :else (-> acc
              (left-fold (:left node) func)
              (func (:value node))
              (left-fold (:right node) func))))

(defn right-fold [acc node func]
  (cond
    (nil? node) acc
    (nil? acc) nil
    :else (-> acc
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

(defn -main [])


