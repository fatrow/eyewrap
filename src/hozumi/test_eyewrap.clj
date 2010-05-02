
(ns
  #^{:author "Takahiro Hozumi"
     :doc "Code observation tool."}
  hozumi.test-eyewrap
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
       :a  `(m-identity :a)
       1   `(m-identity 1)
       [1] `(m-identity [1])
       {:a 1} `(m-identity {:a 1}))
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

(deftest test-cap
  (do (is (= 3 (cap (+ 1 2))))
      (is (= 89 (cap (+ (- 1 2) (* 3 (+ 4 2) 5)))))
      (testing "head of list is fn"
	(is (= 4 (cap ((fn [x] (* x x)) 2)))))
      (testing "tracing defn"
	(cap ppp (defn- touch [coll target-index]
		 (-> [(coll target-index)]
		     (into (subvec coll 0 target-index))
		     (into (subvec coll (inc target-index))))))
	(is (= [3 1 2 4 5] (touch [1 2 3 4 5] 2)))
	(is (= [:b :a :c] (touch [:a :b :c] 1)))
	(ppp 1)
	(ppp)
	(ppp :pp)
	(ppp :internal)
	(ppp :c)
	(ppp :help))
      (testing "function in coll"
	(is (= [:c :a 14 81] (touch [:a (+ 2 (* 3 4)):c (* 9 9)] 2)))
	(is (= [{:b 81} {:a 6} 12 :c] (touch [{:a (+ 1 (+ 2 3))} (* 3 4) {:b (* 9 9)} :c] 2))))
      (testing "infinity lazy seq"
	(is (= (seq [1 2]) (cap (take 2 (cycle [1 2 3]))))))
      (testing "multiple body fn"
	(is (= 3 (cap ((fn [x] (println (inc x)) (dec x)) 4))))
	(is (= 2 (cap ((fn ([x] (dec x) (inc x)) ([x y] (- x y) (+ x y))) 1))))
	(is (= 3 (cap ((fn ([x] (dec x) (inc x)) ([x y] (- x y) (+ x y))) 1 2))))
	(is (= 3 (cap ((fn [x] (dec x)) 4))))
	(cap ppp (defn- aaa [x] (println (inc x)) (dec x)))
	(is (= 3 (aaa 4)))
	(cap ppp (defn- aaa [x] (dec x)))
	(is (= 3 (aaa 4))))
      (testing "special-form"
	(is (number? (cap (System/currentTimeMillis)))))))