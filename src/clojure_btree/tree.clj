(ns clojure-btree.tree)

(defrecord Node [value left right])


(defn add-value-to-node
    [node value]
    (if (nil? node)
        (Node. value nil nil)
        (let [nodeValue (:value node)]
            (cond
                (= nodeValue value) node
                (< nodeValue value) (assoc node :right (add-value-to-node (:right node) value))
                (> nodeValue value) (assoc node :left (add-value-to-node (:left node) value))
            )
        )
    )
)