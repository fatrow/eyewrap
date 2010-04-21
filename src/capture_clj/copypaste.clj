
(defn touch [coll target-index]
  (-> [(coll target-index)]
      (into (subvec coll 0 target-index))
      (into (subvec coll (inc target-index)))))


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
