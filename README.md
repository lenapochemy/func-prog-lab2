# Лабораторная работа №2

Русакова Елена Дмитриевна

ИСУ 367519

группа P3317

вариант **avl-set**

## Требования:

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:
    - API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
    - Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

## Реализация

+ Объявление структуры
```clojure
(defrecord Node [value left right height])
```

+ Создание пустого дерева

Нужно для реализации моноида
```clojure
(defn avl-empty []
  nil)
```

+ Добавление нового узла
```clojure
(defn avl-add [node value]
  (cond
    (nil? node) (->Node value nil nil 1)
    (nil? value) node
    :else  (let [diff (compare value (:value node))]
             (cond
               (zero? diff) node
               (neg? diff) (avl-balance (assoc node :left (avl-add (:left node) value)))
               :else (avl-balance (assoc node :right (avl-add (:right node) value)))))))

```

+ Удаление узла 
```clojure
(defn avl-delete [node value]
  (if (nil? node) nil
      (let [diff (compare value (:value node))]
        (cond
          (neg? diff) (avl-balance (assoc node :left (avl-delete (:left node) value)))
          (pos? diff) (avl-balance (assoc node :right (avl-delete (:right node) value)))
          :else (cond
                  (nil? (:left node)) (:right node)
                  (nil? (:right node)) (:left node)
                  :else (let [min-node (avl-min-value-node (:right node))]
                          (-> min-node
                              (assoc :right (avl-delete (:right node) (:value min-node)))
                              (assoc :left (:left node))
                              (avl-balance))))))))
```

+ Правый и левый повороты дерева

Нужны для балансировки дерева
```clojure
(defn avl-right-rotate [node]
  (let [left-node (:left node)
        new-node (avl-update-node-height (assoc node :left (:right left-node)))]
    (avl-update-node-height (assoc left-node :right new-node))))

(defn avl-left-rotate [node]
  (let [right-node (:right node)
        new-node (avl-update-node-height (assoc node :right (:left right-node)))]
    (avl-update-node-height (assoc right-node :left new-node))))

```

+ Балансировка дерева
```clojure
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
```

+ Отображение (map)

Заданная функция применятеся к каждому узлу дерева
```clojure
(defn avl-map [node func]
  (if (nil? node) nil
      (let [left (avl-map (:left node) func)
            right (avl-map (:right node) func)
            new-value (func (:value node))]
        (assoc node :value new-value :left left :right right))))
```

+ Свертки
```clojure
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
```
 
+ Фильтр

Оставляет в дереве только узлы, удовлетворяющие условию заданной функции
```clojure
(defn avl-filter [node func]
  (if (nil? node) nil
      (let [left (avl-filter (:left node) func)
            right (avl-filter (:right node) func)]
        (if (func (:value node))
          (avl-balance (assoc node :left left :right right))
          (avl-merge left right)))))
```

+ Объединение двух деревьев

Нужно для реализации моноида
```clojure
(defn avl-merge [node1 node2]
  (cond
    (nil? node1) node2
    (nil? node2) node1
    :else (avl-left-fold node1 node2 avl-add)))
```

## Тесты

### Unit testing
Тесты проверяют раюоту всех основных функций


### Property based testing
Тесты проверяют свойства моноида (нейтральный элемент и ассоциативность, относительно объединения двух деревьев)

``` clojure
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
```

## Вывод
Во время выполнения лабораторной работы, я продолжила знакомство с языком программирования Clojure, реализовала на нем свое АВЛ-дерево. 
