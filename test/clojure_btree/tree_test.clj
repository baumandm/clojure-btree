(ns clojure-btree.tree-test
  (:require [clojure.test :refer :all]
            [clojure-btree.tree :refer :all]))

(deftest test-add-value-to-node
    (testing "with empty node"
        (is (= 
            (->Node 99 nil nil)
            (add-value-to-node nil 99))))

    (testing "with same value"
        (is (=
            (->Node 99 nil nil)
            (add-value-to-node nil 99 99))))

    (testing "with a node that has a larger value"
        (is (= 
            (->Node 100 (->Node 99 nil nil) nil)
            (add-value-to-node (->Node 100 nil nil) 99))))

    (testing "with a node that has a smaller value"
        (is (= 
            (->Node 100 nil (->Node 101 nil nil))
            (add-value-to-node (->Node 100 nil nil) 101))))

    (testing "with a two values at once"
        (is (= 
            (->Node 101 nil (->Node 102 nil nil))
            (add-value-to-node nil 101 102))))

    (testing "with a three values at once"
        (is (= 
            (->Node 101 (->Node 100 nil nil) (->Node 102 nil nil))
            (add-value-to-node nil 101 102 100))))

    (testing "with a four values at once"
        (is (= 
            (->Node 101 (->Node 100 (->Node 99 nil nil) nil) (->Node 102 nil nil))
            (add-value-to-node nil 101 102 100 99))))

    (testing "with a four values and an initial node"
        (is (= 
            (->Node 101 (->Node 100 (->Node 99 nil nil) nil) (->Node 106 (->Node 102 nil nil) nil))
            (add-value-to-node (->Node 101 nil nil) 106 102 100 99))))
)

(deftest test-make-tree
    (testing "with nil"
        (is (->Node nil nil nil) (make-tree nil)))
    (testing "with one value"
        (is (->Node 6 nil nil) (make-tree 6)))
    (testing "with two values"
        (is (->Node 6 nil (->Node 7 nil nil)) (make-tree 6 7))))

(deftest test-count-nodes
    (testing "with empty tree"
        (is (= 0 (count-nodes nil))))

    (testing "with one node"
        (is (= 1 (count-nodes (add-value-to-node nil 5)))))

    (testing "with two nodes"
        (is (= 2 (count-nodes (add-value-to-node (add-value-to-node nil 5) 6))))))

(deftest test-count-nodes-on-left-nil
    (testing "with nil"
        (is (= 0 (count-nodes-on-left nil))))

    (testing "with 6 nodes"
        (is (= 3 (count-nodes-on-left (add-value-to-node nil 5 4 9 8 1 2))))))

(deftest test-count-nodes-on-right
    (testing "with nil"
        (is (= 0 (count-nodes-on-right nil))))

    (testing "with 4 nodes"
        (is (= 3 (count-nodes-on-right (add-value-to-node nil 1 2 3 4))))))

(deftest test-traverse-inorder
    (testing "with nil"
        (is (= '() (traverse-inorder nil))))
    (testing "with single node"
        (is (= '(1) (traverse-inorder (->Node 1 nil nil)))))
    (testing "with two nodes in order"
        (is (= '(1 2) (traverse-inorder (add-value-to-node nil 1 2)))))
    (testing "with two nodes out of order"
        (is (= '(1 2) (traverse-inorder (add-value-to-node nil 2 1)))))
    (testing "with three nodes unbalanced"
        (is (= '(1 2 3) (traverse-inorder (add-value-to-node nil 1 2 3)))))
    (testing "with three nodes balanced"
        (is (= '(1 2 3) (traverse-inorder (add-value-to-node nil 2 1 3)))))
    (testing "with many nodes"
        (is (= '(1 2 3 4 5 6 9) (traverse-inorder (make-tree 1 6 4 5 9 3 2)))))
    )

(deftest test-traverse-preorder
    (testing "with nil"
        (is (= '() (traverse-preorder nil))))
    (testing "with single node"
        (is (= '(1) (traverse-preorder (->Node 1 nil nil)))))
    (testing "with two nodes in order"
        (is (= '(1 2) (traverse-preorder (add-value-to-node nil 1 2)))))
    (testing "with two nodes out of order"
        (is (= '(2 1) (traverse-preorder (add-value-to-node nil 2 1)))))
    (testing "with three nodes unbalanced"
        (is (= '(1 2 3) (traverse-preorder (add-value-to-node nil 1 2 3)))))
    (testing "with three nodes balanced"
        (is (= '(2 1 3) (traverse-preorder (add-value-to-node nil 2 1 3)))))
    (testing "with many nodes"
        (is (= '(1 6 4 3 2 5 9) (traverse-preorder (make-tree 1 6 4 5 9 3 2)))))
    )

(deftest test-tree-depth
    (testing "with nil"
        (is (= 0 (tree-depth nil))))
    (testing "with single node"
        (is (= 1 (tree-depth (->Node 1 nil nil)))))
    (testing "with two nodes in order"
        (is (= 2 (tree-depth (add-value-to-node nil 1 2)))))
    (testing "with two nodes out of order"
        (is (= 2 (tree-depth (add-value-to-node nil 2 1)))))
    (testing "with three nodes unbalanced"
        (is (= 3 (tree-depth (add-value-to-node nil 1 2 3)))))
    (testing "with three nodes balanced"
        (is (= 2 (tree-depth (add-value-to-node nil 2 1 3)))))
    (testing "with many nodes"
        (is (= 5 (tree-depth (make-tree 1 6 4 5 3 2)))))
    )

(deftest test-find-node
    (testing "with nil"
        (is (nil? (find-node nil 1))))
    (testing "with one node"
        (is (= (->Node 1 nil nil) (find-node (make-tree 1) 1))))
    (testing "with one node that doesn't match"
        (is (nil? (find-node (make-tree 1) 5))))
    (testing "with two nodes"
        (is (= (->Node 5 nil nil) (find-node (make-tree 1 5) 5))))
    (testing "with three nodes"
        (is (= (->Node 1 nil nil) (find-node (make-tree 3 5 1) 1))))
    (testing "with many nodes"
        (is (= (->Node 3 (->Node 1 nil (->Node 2 nil nil)) (->Node 4 nil nil)) (find-node (make-tree 5 10 6 5 3 4 9 1 7 2) 3))))
    )

(deftest test-find-min-node
    (testing "with nil"
        (is (nil? (find-min-node nil))))
    (testing "with one node"
        (is (= (->Node 10 nil nil) (find-min-node (make-tree 10)))))
    (testing "with many nodes"
        (is (= (make-tree 1 2) (find-min-node (make-tree 5 10 6 5 3 4 9 1 7 2)))))
    )

(deftest test-find-min-value
    (testing "with nil"
        (is (nil? (find-min-value nil))))
    (testing "with one node"
        (is (= 10 (find-min-value (make-tree 10)))))
    (testing "with many nodes"
        (is (= 1 (find-min-value (make-tree 5 10 6 5 3 4 9 1 7 2)))))
    )

(deftest test-find-max-value
    (testing "with nil"
        (is (nil? (find-max-value nil))))
    (testing "with one node"
        (is (= 10 (find-max-value (make-tree 10)))))
    (testing "with many nodes"
        (is (= 10 (find-max-value (make-tree 5 10 6 5 3 4 9 1 7 2)))))
    )

(deftest test-remove-value
    (testing "with nil"
        (is (nil? (remove-value nil 1))))
    (testing "with one value"
        (is (nil? (remove-value (make-tree 1) 1))))
    (testing "with unknown value"
        (is (= (->Node 1 nil nil) (remove-value (make-tree 1) 2))))
    (testing "with remove left"
        (is (= (->Node 2 nil nil) (remove-value (make-tree 2 1) 1))))
    (testing "with remove right"
        (is (= (->Node 2 (->Node 1 nil nil) nil) (remove-value (make-tree 2 1 3) 3))))
    (testing "with remove balanced"
        (is (= (->Node 3 (->Node 1 nil nil) nil) (remove-value (make-tree 2 1 3) 2))))
    (testing "with remove complex"
        (is (= (make-tree 5 10 6 9 7 4 1 2) (remove-value (make-tree 5 10 6 5 3 4 9 1 7 2) 3))))
    (testing "with remove complex-2"
        (is (= (make-tree 5 10 6 9 7 3 1 2) (remove-value (make-tree 5 10 6 5 3 4 9 1 7 2) 4))))
    (testing "with remove complex-3"
        (is (= (make-tree 5 10 6 9 7 3 4 2) (remove-value (make-tree 5 10 6 5 3 4 9 1 7 2) 1))))
    (testing "with remove complex-4"
        (is (= (make-tree 6 10 9 7 3 1 4 2) (remove-value (make-tree 5 10 6 5 3 4 9 1 7 2) 5))))
    (testing "with remove multiple"
        (is (= (make-tree 7 10 9 3 1 4 2) 
               (remove-value (remove-value (make-tree 5 10 6 5 3 4 9 1 7 2) 5) 6))))
    )

(deftest test-traverse-breadth-first
    (testing "with nil"
        (is (= '() (traverse-breadth-first nil))))
    (testing "with one node"
        (is (= '(10) (traverse-breadth-first (make-tree 10)))))
    (testing "with three balanced nodes"
        (is (= '(2 1 3) (traverse-breadth-first (make-tree 2 1 3)))))
    (testing "with two nodes"
        (is (= '(10 20) (traverse-breadth-first (make-tree 10) (make-tree 20)))))
    (testing "with many nodes"
        (is (= '(6 4 9 3 5 2) (traverse-breadth-first (make-tree 6 4 5 3 2 9)))))
    (testing "with many nodes - b"
        (is (= '(5 4 10 3 9 12 1 6 11 13 2 7 8) (traverse-breadth-first (make-tree 5 4 10 3 9 12 1 6 11 13 2 7 8)))))
    )


(deftest test-with-strings
    (testing "create tree"
        (is (= (->Node "hello" nil (->Node "world" nil nil)) (make-tree "hello" "world"))))
    (testing "add-value-to-node"
       (is (= (->Node "hello" nil (->Node "world" nil nil)) (add-value-to-node (make-tree "hello") "world"))))
    (testing "traverse breadth-first with four nodes"
        (is (= '("James" "Allison" "Walt" "Arya") (traverse-breadth-first (make-tree "James" "Allison" "Arya" "Walt")))))
    )