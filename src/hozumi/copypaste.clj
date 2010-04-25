
(defn touch [coll target-index]
  (-> [(coll target-index)]
      (into (subvec coll 0 target-index))
      (into (subvec coll (inc target-index)))))

(defn cond1 [x]
  (cond
    (= x :hey) "Hey!"
    (= x :bye) "Bye!"
    (= x :hi) "Hi!"))

(defn condp1 [x]
  (cond = x
	:hey "Hey!"
	:bye "Bye!"
	:hi "Hi!"))

(defn cond2 [x]
  (cond
    (list? x) "List!"
    (map? x) "Map!"
    (vector? x) "Vector!"))

(defn condp2 [x]
  (condp #(%1 %2) x
    list? "List!"
    map? "Map!"
    vector? "Vector!"))


(defn touch [coll target-index]
;         [0 1 2 3 4]  2
  (-> [(coll     target-index)]
; (-> [([0 1 2 3 4]       2)]
; (-> [2]
      (into (subvec coll 0 target-index))
;     (into ,,, (subve [0 1 2 3 4] 0 2)
;     (into ,,, [0 1])
;     [2 0 1]
      (into (subvec coll (inc target-index)))))
;     (into ,,, (subvec [0 1 2 3 4] (inc 2)))
;     (into ,,, (subvec [0 1 2 3 4] 3)
;     (into ,,, [3 4])
;     [2 0 1 3 4]



(use 'clojure.contrib.pprint)
;(pprint (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))
