(ns clojure-btree.tree-test
  (:require [clojure.test :refer :all]
            [clojure-btree.tree :refer :all]))

(deftest test-add-value-to-node
    (testing "with empty node"
        (is (= 
            (->Node 99 nil nil)
            (add-value-to-node nil 99))))

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