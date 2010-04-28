
(ns
  #^{:author "Takahiro Hozumi"
     :doc "Code observation tool."}
  hozumi.eyewrap.test-eyewrap
  (:use [hozumi.eyewrap] :reload-all)
  (:use [clojure.test]))

(deftest test-elem? 
  (are [expected x] (= expected (elem? x))
       true 1
       true 'a
       true :a
       true "string"
       false '()
       false '(1 2)
       false []       
       false [1 2]
       false {}
       false {:a 1}
       false #{}
       false #{:a :b}))
       
(deftest test-func?
  (are [expected x] (= expected (func? x))
       true map
       true (fn [] 1)
       true :a
       true 'a
       false 1
       false (java.util.ArrayList.)))

(deftest test-macro?
  (are [expected sym] (= expected (macro? sym))
       true 'cond
       true 'for
       false 'map
       false 'fklajfe
       false ':a))

(deftest test-conv-to
  (are [expected type coll] (= expected (conv-to type coll))
       [1 2]    [] '(1 2)
       [1 2]    [10] '(1 2)
       [1 2]    [] (seq '(1 2))
       [1 2]    [] [1 2]
       '(1 2)   () [1 2]
       '(1 2)   '(1) [1 2]
       '(1 2)   '() '(1 2)
       '(1 2)   '() (seq '(1 2))
       {:a 1}   {} (seq {:a 1})
       {:a 1}   {:b 2} (seq {:a 1})
       #{:a :b} #{} (seq #{:a :b})))

(defmacro m-identity [x] x)

(deftest test-macroexpand-all
  (are [expected exp] (= expected (macroexpand-all exp))
       '(- (+ 1 2) 3) '(-> 1 (+ 2) (- 3))
       '(. System currentTimeMillis) '(System/currentTimeMillis)
       '(new Widget "red") '(Widget. "red")
       '(. Math PI) '(Math/PI)
       '(. rnd nextInt) '(.nextInt rnd)
       '(. (. person getAddress) getZipCode) '(.. person getAddress getZipCode)
       '(+ 1 1) '(+ 1 1)
       '(:a {:a 1}) '(:a {:a 1})
       '(if (= 1 0) :a (if (= 1 1) :b nil)) '(cond (= 1 0) :a (= 1 1) :b)
       '(a 1) '(a 1)
       '(list? '(-> 1 inc)) '(list? '(-> 1 inc))
       :a  '(m-identity :a)
       1   '(m-identity 1)
       [1] '(m-identity [1])
       {:a 1} '(m-identity {:a 1}))
  (testing "don't throw Exception"
    (is (not= nil (macroexpand-all '(condp = x :a 1 :b 2))))))


(deftest test-allocate-id
  (let [mem (atom (mem-init))]
    (is (= {:maxid 1, :result {:child {1 {}}}, :parent-table {1 nil}}
	   (swap! mem allocate-id nil)))
    (is (= {:maxid 2, :result {:child {1 {:child {2 {}}}}}, :parent-table {1 nil, 2 1}}
	   (swap! mem allocate-id 1)))
    (is (= {:maxid 3, :result {:child {1 {:child {2 {:child {3 {}}}}}}},:parent-table {1 nil, 2 1, 3 2}}
	   (swap! mem allocate-id 2)))))

(deftest test-update-mem
  (let [mem (atom (mem-init))]
    (is (= {:maxid 1, :result {:child {1 {}}}, :parent-table {1 nil}}
	   (swap! mem allocate-id nil)))
    (is (= {:maxid 2,
	    :result {:child {1 {:child {2 {:id 2, :form 1, :out 1}}}}},
	    :parent-table {1 nil, 2 1}}
	   (swap! mem update-mem '1 1 1)))
    (is (= {:maxid 3,
	    :result {:child {1 {:child {2 {:id 2, :form 1, :out 1}
					3 {:id 3, :form 2, :out 2}}}}},
	    :parent-table {1 nil, 2 1, 3 1}}
	   (swap! mem update-mem '2 2 1)))
    (is (= {:maxid 3,
	    :result {:child {1 {:id 1, :form '(+ 1 2), :out 3,
				:child {2 {:id 2, :form 1, :out 1}
					3 {:id 3, :form 2, :out 2}}}}},
	    :parent-table {1 nil, 2 1, 3 1}}
	   (swap! mem update-mem-existing-id '(+ 1 2) 3 1)))))

(deftest test-get-idpath
  (are [expected parent-table id] (= expected (get-idpath parent-table id))
       [] {} nil
       [1] {1 nil} 1
       [1 2] {1 nil, 2 1} 2
       [1 3] {1 nil, 2 1, 3 1} 3
       [1 2 4] {1 nil, 2 1, 3 1, 4 2} 4
       [1 2 4 5] {1 nil, 2 1, 3 1, 4 2, 5 4} 5))




