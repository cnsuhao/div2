(ns sas.test.quadtree
  (:use [sas.quadtree] :reload)
  (:use [clojure.test]))

;; declare some vars needed for binding during the test
(declare tree pt0 pt1 pt2 pt3 pt4)

;; tests the simple wrapper on QuadTreeNode's consrtuctor
(deftest quadtree-constructor
  (is (= (class tree) sas.quadtree.QuadTreeNode) "Creation helper"))

;; make sure which-quadrent is calculating the correct key (not that it really matters as long as it is consistent)
(deftest which-quadrent-test
  (testing "testing each region"
    (are [r x y] (= r (which-quadrent tree x y))
      :ne 75 25
      :se 75 75
      :nw 25 25
      :sw 25 75))
  (is (= :ne (which-quadrent tree (sas.quadtree.QuadTreePoint. 75 25))) "testing point as parameter"))

;; test the bounds checker
(deftest out-of-bounds-test
  (testing "coordinates in bounds"
    (are [x y] (not (out-of-bounds tree x y))
       50  50
        0   0
      100 100))
  
  (testing "only one coordinate out"
    (are [x y] (out-of-bounds tree x y)
      110  50
      -10  50
       50 110
       50 -10))
  
  (testing "coordinates completely bounds"
    (are [x y] (out-of-bounds tree x y)
      -10 -10
      110 110
      -10 110
      110 -10)))
  

;; Tests out a long series of inserts
(deftest insert-test
  (testing "out of bounds"
    (is (thrown-with-msg? IllegalArgumentException #"QuadTree" 
          (insert tree pt0)) "make sure out of bounds points are filter"))
  
  (testing "empty at start"
    (is (nil? (-> tree :data))))
  
  (testing "first point"
    (set! tree (insert tree pt1))
    (is (= pt1 (-> tree :data))))
  
  (testing "duplicating first point"
    (set! tree (insert tree pt1))
    (is (= pt1 (-> tree :data))))
  
  (testing "second point, checking for splits"
	  (set! tree (insert tree pt2))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw] pt1
       [tree :se] pt2
	     [tree] :sas.quadtree/branches ))
  
  (testing "third point"
	  (set! tree (insert tree pt3))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se :nw]  pt2
       [tree :se :se]  pt3
       [tree] :sas.quadtree/branches
       [tree :se] :sas.quadtree/branches))
       
       
  (testing "repeat third point"
	  (set! tree (insert tree pt3))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se :nw]  pt2
       [tree :se :se]  pt3
       [tree] :sas.quadtree/branches
       [tree :se] :sas.quadtree/branches))
	 
  (testing "last point"
	  (set! tree (insert tree pt4))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se :nw]  pt2
       [tree :se :se]  pt3
       [tree :sw]  pt4
       [tree] :sas.quadtree/branches
       [tree :se] :sas.quadtree/branches)))

;; tests the method for fetching a node based on point
(deftest query-test
  (are [pth pt] (= (query tree pt) (get-in (first pth) (rest pth)))
     [tree :nw]  pt1
     [tree :se :nw]  pt2
     [tree :se :se]  pt3
     [tree :sw]  pt4))

;; delets all previously added points
(deftest delete-test
  (testing "out of bounds"
    (is (thrown-with-msg? IllegalArgumentException #"QuadTree" 
          (insert tree pt0)) "make sure out of bounds points are filter"))
  
  (testing "double check last point"
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se :nw]  pt2
       [tree :se :se]  pt3
       [tree :sw]  pt4
       [tree] :sas.quadtree/branches
       [tree :se] :sas.quadtree/branches))
  
  (testing "delete last point"
	  (set! tree (delete tree pt4))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se :nw]  pt2
       [tree :se :se]  pt3
       [tree :sw]  nil
       [tree] :sas.quadtree/branches
       [tree :se] :sas.quadtree/branches))
  
  (testing "delete 3rd point"
	  (set! tree (delete tree pt3))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree :nw]  pt1
       [tree :se]  pt2
       [tree :sw]  nil
       [tree] :sas.quadtree/branches))
  
  (testing "delete 2rd point"
	  (set! tree (delete tree pt2))
	  (are [pth rslt] (= rslt (:data (get-in (first pth) (rest pth))))
       [tree]  pt1))
  
  (testing "delete 1st point"
    (set! tree (delete tree pt1))
    (is (= nil (-> tree :data))))
   
  (testing "duplicate (or deleting where there are no points"
    (set! tree (delete tree pt1))
    (is (= nil (-> tree :data)))))
  

;; checks a bunch of helpers which profide information
(deftest info-helpers-test
  (is (= 3 (get-depth tree)) "depth test")
  (is (= 4 (get-leaf-count tree)) "leaf count test")
  (is (= #{pt1 pt2 pt3 pt4} (apply hash-set (list-all tree))) "listing all points")
  
  (is (= [(:ne tree) (:se tree) (:sw tree) (:nw tree)] (get-branches tree)) "get all branches")
  (is (= (apply-to-branches tree :data) (map :data (get-branches tree)))  "apply functions to branches")
  (is (= (apply-to-branches* tree :data) (map :data (get-branches tree)))  "apply functions to branches"))

(defn test-ns-hook 
  "The controller to run all of our tests"
  []
  
  (testing "QuadTree Helpers "
    (binding [tree (quadtree 0 0 100 100)]
      (testing "(constructor) -> " (quadtree-constructor))
      (testing "(quadrent calculation) -> " (which-quadrent-test))
      (testing "(out of bounds) -> " (out-of-bounds-test))))
  
  (testing "QuadTree "
    (with-bindings 
      {#'tree (quadtree 10 10 80 80)
       #'pt0 (sas.quadtree.QuadTreePoint. -10 -10)
       #'pt1 (sas.quadtree.QuadTreePoint. 12 12)
       #'pt2 (sas.quadtree.QuadTreePoint. 50 50)
       #'pt3 (sas.quadtree.QuadTreePoint. 75 75)
       #'pt4 (sas.quadtree.QuadTreePoint. 12 75) }

       (testing "(insert) -> " (insert-test))
       (testing "(query) -> " (query-test))
       (testing "(info) -> " (info-helpers-test))
       (testing "(delete) -> " (delete-test))
  )))
