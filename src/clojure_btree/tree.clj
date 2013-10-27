(ns clojure-btree.tree)

(defrecord Node [value left right])


(defn add-value-to-node
    ([node value]
        (if (nil? node)
            (Node. value nil nil)
            (let [nodeValue (:value node)
                  comparison (compare nodeValue value)]
                (cond
                    (neg? comparison) (assoc node :right (add-value-to-node (:right node) value))
                    (pos? comparison) (assoc node :left (add-value-to-node (:left node) value))
                    :else node
                )
            )
        ))
    ([node value & more]
        (reduce add-value-to-node (add-value-to-node node value) more))
)

(defn make-tree
    "Creates a tree with one or more values, added in order"
    [& values]
    (apply add-value-to-node nil values))

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

(defn traverse-inorder
    "Flattens a tree into a list in order"
    [node]
    (if (nil? node)
        '()
        (concat 
            (traverse-inorder (:left node)) 
            (list (:value node)) 
            (traverse-inorder (:right node)))))

(defn traverse-breadth-first
    "Flattens a tree into a list breadth-first"
    [& nodes]
    (if (or (empty? nodes) (nil? nodes) (= nodes '(nil)))
        '()
        (concat 
            (map :value nodes)
            (apply traverse-breadth-first 
                (filter identity (mapcat (juxt :left :right) nodes)))
        )
    ))

(defn traverse-preorder
    "Flattens a tree into a list in preorder"
    [node]
    (if (nil? node)
        '()
        (concat 
            (list (:value node)) 
            (traverse-preorder (:left node)) 
            (traverse-preorder (:right node)))))

(defn tree-depth
    "Calculates the depth of the tree, starting at 1 for the root node"
    [node]
    (if (nil? node)
        0
        (+ 1 (max (tree-depth (:left node))
                  (tree-depth (:right node))))))

(defn find-node
    "Returns the node with the given value"
    [node value]
    (if (nil? node) 
        nil
        (let [current-value (:value node)]
            (cond 
                (< value current-value) (find-node (:left node) value)
                (> value current-value) (find-node (:right node) value)
                :else node
            ))))    

(defn find-min-node
    "Returns the node with the smallest value in the tree"
    [node]
    (if (nil? node)
        nil
        (if-let [left (:left node)]
            (find-min-node left)
            node)))

(defn find-max-node
    "Returns the node with the largest value in the tree"
    [node]
    (if (nil? node)
        nil
        (if-let [right (:right node)]
            (find-max-node right)
            node)))

(defn find-min-value
    "Returns the smallest value in the tree"
    [node]
    (if-let [smallest-node (find-min-node node)]
        (:value smallest-node)
        nil))    
            
(defn find-max-value
    "Returns the largest value in the tree"
    [node]
    (if-let [largest-node (find-max-node node)]
        (:value largest-node)
        nil))

(defn remove-value
    "Removes a value from the tree"
    [node value]
    (let [current-value (:value node)]
        (cond
            (nil? node) nil
            (< value current-value) (assoc node :left (remove-value (:left node) value))
            (> value current-value) (assoc node :right (remove-value (:right node) value))
            :else (let [left (:left node)
                        right (:right node)]
                (cond
                    (and (nil? left) (nil? right)) nil ; remove current node
                    (nil? left) right ; replace current with right
                    (nil? right) left ; replace current with left
                    :else (let [smallest-value (find-min-value right)]
                        (assoc node :value smallest-value
                                    :right (remove-value right smallest-value)))
            )))))