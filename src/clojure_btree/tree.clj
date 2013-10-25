(ns clojure-btree.tree)

(defrecord Node [value left right])


(defn add-value-to-node
    ([node value]
        (if (nil? node)
            (Node. value nil nil)
            (let [nodeValue (:value node)]
                (cond
                    (= nodeValue value) node
                    (< nodeValue value) (assoc node :right (add-value-to-node (:right node) value))
                    (> nodeValue value) (assoc node :left (add-value-to-node (:left node) value))
                )
            )
        ))
    ([node value & more]
        (reduce add-value-to-node (add-value-to-node node value) more))
)

(defn count-nodes
    "Counts all the child nodes of the argument, including itself"
    [node]
    (if (nil? node)
        0
        (+ 1 (count-nodes (:left node)) (count-nodes (:right node)))
    ))

(defn count-nodes-on-left
    "Counts all the nodes under :left of the root"
    [node]
    (if (or (nil? node) (nil? (:left node)))
        0
        (count-nodes (:left node))
    ))

(defn count-nodes-on-right
    "Counts all the nodes under :right of the root"
    [node]
    (if (or (nil? node) (nil? (:right node)))
        0
        (count-nodes (:right node))
    ))