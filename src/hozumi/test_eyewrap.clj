
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
       {:a 1} `(m-identity {:a 1})
       nil `(m-identity nil)
       () `(m-identity ()))
  (testing "don't throw Exception"
    (is (not= nil (macroexpand-all '(condp = x :a 1 :b 2))))))

(deftest test-allocate-id
  (let [mem (atom (mem-init))]
    (is (= {:maxid 1, :result {:child {1 {:form '(+ 5 6)}}}, :parent-table {1 nil}}
	   (swap! mem allocate-id '(+ 5 6) nil)))
    (is (= {:maxid 2, :result {:child {1 {:child {2 {:form '+}}, :form '(+ 5 6)}}}, :parent-table {2 1, 1 nil}}
	   (swap! mem allocate-id '+ 1)))
    (is (= {:maxid 3, :result {:child {1 {:child {3 {:form 5},
						  2 {:form '+}},
					  :form '(+ 5 6)}}},
	    :parent-table {3 1, 2 1, 1 nil}}
	   (swap! mem allocate-id 5 1)))
    (is (= {:maxid 4, :result {:child {1 {:child {4 {:form 6},
						  3 {:form 5},
						  2 {:form '+}},
					  :form '(+ 5 6)}}},
	    :parent-table {4 1, 3 1, 2 1, 1 nil}}
	   (swap! mem allocate-id 6 1)))))

(deftest test-update-mem
  (let [mem (atom (mem-init))]
    (is (= {:maxid 1, :result {:child {1 {:form '(+ 5 6)}}}, :parent-table {1 nil}}
	   (swap! mem allocate-id '(+ 5 6) nil)))
    (is (= {:maxid 2, :result {:child {1 {:child {2 {:form '+}}, :form '(+ 5 6)}}}, :parent-table {2 1, 1 nil}}
	   (swap! mem allocate-id '+ 1)))
    (is (= {:maxid 2, :result {:child {1 {:child {2 {:id 2, :out +, :form '+}}, :form '(+ 5 6)}}}, :parent-table {2 1, 1 nil}}
	   (swap! mem update-mem + 2)))
    (is (= {:maxid 3, :result {:child {1 {:child {3 {:form 5}, 2 {:id 2, :out +, :form '+}}, :form '(+ 5 6)}}}, :parent-table {3 1, 2 1, 1 nil}}
	   (swap! mem allocate-id 5 1)))
    (is (= {:maxid 3, :result {:child {1 {:child {3 {:id 3, :out 5, :form 5}, 2 {:id 2, :out +, :form '+}}, :form '(+ 5 6)}}}, :parent-table {3 1, 2 1, 1 nil}}
	   (swap! mem update-mem 5 3)))
    (is (= {:maxid 4, :result {:child {1 {:child {4 {:form 6}, 3 {:id 3, :out 5, :form 5}, 2 {:id 2, :out +, :form '+}}, :form '(+ 5 6)}}}, :parent-table {4 1, 3 1, 2 1, 1 nil}}
	   (swap! mem allocate-id 6 1)))
    (is (= {:maxid 4, :result {:child {1 {:child {4 {:id 4, :out 6, :form 6}, 3 {:id 3, :out 5, :form 5}, 2 {:id 2, :out +, :form '+}}, :form '(+ 5 6)}}}, :parent-table {4 1, 3 1, 2 1, 1 nil}}
	   (swap! mem update-mem 6 4)))))

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
	(cap yyy (defn- touch [coll target-index]
		   (-> [(coll target-index)]
		       (into (subvec coll 0 target-index))
		       (into (subvec coll (inc target-index))))))
	(is (= [3 1 2 4 5] (touch [1 2 3 4 5] 2)))
	(is (= [:b :a :c] (touch [:a :b :c] 1)))
	(yyy)
	(yyy 3)
	(yyy 25 22 :v)
	(yyy :all :v)
	(yyy :v)
	(yyy :1 :v)
	(yyy :pp)
	(yyy :i)
	(yyy :c)
	(yyy :help))
      (testing "coll"
	(are [expected form] (= expected (cap form))
	     {} {}
	     {nil nil} {nil nil}
	     {:a 2} {:a (inc 1)}
	     [] []
	     [nil] [nil]
	     [:a] [:a]
	     [:a 2] [:a (inc 1)]))
      (testing "function in coll"
	(is (= 2 (cap ({:a (inc 1) :b (dec 1)} :a))))
	(is (= 2 (cap ([(inc 1) (inc 2) (dec 1) (dec 2)] 0))))
	(is (= [1] (cap [(inc 0)])))
	(is (= [:c :a 14 81] (touch [:a (+ 2 (* 3 4)):c (* 9 9)] 2)))
	(is (= [{:b 81} {:a 6} 12 :c] (touch [{:a (+ 1 (+ 2 3))} (* 3 4) {:b (* 9 9)} :c] 2))))
      (testing "infinity lazy seq"
	(is (= (seq [1 2]) (cap (take 2 (cycle [1 2 3]))))))
      (testing "multiple body fn"
	(is (= 3 (cap ((fn [x] (println (inc x)) (dec x)) 4))))
	(is (= 2 (cap ((fn ([x] (dec x) (inc x)) ([x y] (- x y) (+ x y))) 1))))
	(is (= 3 (cap ((fn ([x] (dec x) (inc x)) ([x y] (- x y) (+ x y))) 1 2))))
	(is (= 3 (cap ((fn [x] (dec x)) 4))))
	(cap yyy (defn- aaa [x] (println (inc x)) (dec x)))
	(is (= 3 (aaa 4)))
	(cap yyy (defn- aaa [x] (dec x)))
	(is (= 3 (aaa 4))))
      (testing "empty and element"
	(is (= () (cap ())))
	(is (= :a (cap :a)))
	(is (= nil (cap nil)))
	(is (= :a (cap (let [] :a))))
	(is (= nil (cap (let [] nil))))
	(is (= nil (cap (let [a nil] a))))
	(is (= () (cap (try ()))))
	(is (= :a (cap (try :a))))
	(is (= nil (cap (try nil)))))
      (testing "let"
	(is (= 14 (cap (let [a (+ 1 (* 2 3)) b (/ 6 3)] (* a b))))))
      (testing "special-form"
	(is (number? (cap (System/currentTimeMillis)))))))