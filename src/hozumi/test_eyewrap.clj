
(ns
  #^{:author "Takahiro Hozumi"
     :doc "Code observation tool."}
  hozumi.eyewrap.test-eyewrap
  (:use [hozumi.eyewrap])
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

(deftest test-update-mem
  (let [m (atom {:maxid 0, :result {}})]
    (are [expected mem form v childs] (= expected (update-mem mem form v childs))
	 {:maxid 1, :result {1 {:id 1, :form '(+ 1 2) :out 3 :childs 2}}} @m '(+ 1 2) (+ 1 2) 2
	 {:maxid 1, :result {1 {:id 1, :form '1 :out 1}}} @m '1 1 nil
	 {:maxid 1, :result {1 {:id 1, :form ':a :out :a}}} @m ':a :a nil)))
       