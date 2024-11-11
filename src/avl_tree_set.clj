(ns avl-tree-set)

(defrecord Node [value left right height])

(defn avl-node-height [node]
  (if (nil? node) 0 (:height node)))

(defn avl-update-node-height [node]
  (if (nil? node) 0
      (let [new-height (inc (max (avl-node-height (:left node))
                                 (avl-node-height (:right node))))]
        (assoc node :height new-height))))

(defn avl-balance-factor [node]
  (- (avl-node-height (:left node)) (avl-node-height (:right node))))

(defn avl-right-rotate [node]
  (let [left-node (:left node)
        new-node (avl-update-node-height (assoc node :left (:right left-node)))]
    (avl-update-node-height (assoc left-node :right new-node))))

(defn avl-left-rotate [node]
  (let [right-node (:right node)
        new-node (avl-update-node-height (assoc node :right (:left right-node)))]
    (avl-update-node-height (assoc right-node :left new-node))))

(defn avl-balance [node]
  (let [bf (avl-balance-factor node)]
    (cond
      (> bf 1) (if (>= (avl-balance-factor (:left node)) 0)
                 (avl-right-rotate node)
                 (avl-right-rotate (assoc node :left (avl-left-rotate (:left node)))))
      (< bf -1) (if (<= (avl-balance-factor (:right node)) 0)
                  (avl-left-rotate node)
                  (avl-left-rotate (assoc node :right (avl-right-rotate (:right node)))))
      :else (avl-update-node-height node))))

(defn avl-add [node value]
  (if (nil? node)
    (->Node value nil nil 1)
    (let [diff (compare value (:value node))]
      (cond
        (zero? diff) (node)
        (neg? diff) (avl-balance (assoc node :left (avl-add (:left node) value)))
        ;; pos?
        :else (avl-balance (assoc node :right (avl-add (:right node) value)))))))

(defn avl-min-value-node [node]
  (if (nil? (:left node))
    node
    (recur (:left node))))

(defn avl-delete [node value]
  (if (nil? node) nil
      (let [diff (compare value (:value node))]
        (cond
          (neg? diff) (avl-balance (assoc node :left (avl-delete (:left node) value)))
          (pos? diff) (avl-balance (assoc node :right (avl-delete (:right node) value)))
      ;; zero?
          :else (cond
                  (nil? (:left node)) (:right node)
                  (nil? (:right node)) (:left node)
                  :else (let [min-node (avl-min-value-node (:right node))]
                          (-> min-node
                              (assoc :right (avl-delete (:right node) (:value min-node)))
                              (assoc :left (:left node))
                              (avl-balance))))))))

(defn avl-contains? [node value]
  (if (nil? node) false
      (let [diff (compare value (:value node))]
        (cond
          (neg? diff) (recur (:left node) value)
          (pos? diff) (recur (:right node) value)
          :else true))))

(defn avl-empty []
  nil)

(defn avl-size [node]
  (if (nil? node) 0
      (inc (+ (avl-size (:left node)) (avl-size (:right node))))))

(defn avl-balanced? [node]
  (if (nil? node) true
      (let [bf (avl-balance-factor node)]
        (cond
          (> bf 1) false
          (< bf -1) false
          :else (and (avl-balanced? (:left node)) (avl-balanced? (:right node)))))))

(defn avl-map [node func]
  (if (nil? node) nil
      (let [left (avl-map (:left node) func)
            right (avl-map (:right node) func)
            new-value (func (:value node))]
        (assoc node :value new-value :left left :right right))))

(defn avl-left-fold [acc node func]
  (cond
    (nil? node) acc
    (nil? acc) nil
    :else (-> acc
              (avl-left-fold (:left node) func)
              (func (:value node))
              (avl-left-fold (:right node) func))))

(defn avl-right-fold [acc node func]
  (cond
    (nil? node) acc
    (nil? acc) nil
    :else (-> acc
              (avl-right-fold (:right node) func)
              (func (:value node))
              (avl-right-fold (:left node) func))))

(defn avl-merge [node1 node2]
  (cond
    (nil? node1) node2
    (nil? node2) node1
    :else (avl-left-fold node1 node2 avl-add)))

(defn avl-filter [node func]
  (if (nil? node) nil
      (let [left (avl-filter (:left node) func)
            right (avl-filter (:right node) func)]
        (if (func (:value node))
          (avl-balance (assoc node :left left :right right))
          (avl-merge left right)))))

(defn -main [])


