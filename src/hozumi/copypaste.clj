
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

(use 'clojure.contrib.server-socket)
(import (java.io BufferedReader
                 InputStreamReader
                 PrintWriter
                 OutputStreamWriter))

(defn my-line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings.
  rdr must implement java.io.BufferedReader."
  [#^java.io.BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

(defn hoge [in out]
  (let [reader (BufferedReader. (InputStreamReader. in))
        writer (PrintWriter. (OutputStreamWriter. out))]
    (doall
     (for [line (my-line-seq reader)]
       (do
         (.println writer line)
         (.flush writer))))))

(def echo-server (ref nil))
(def port-no 3000)

(defn start-echo-server []
  (dosync (ref-set echo-server (create-server port-no hoge))))

(defn stop-echo-server []
  (close-server @echo-server)
  (dosync (ref-set echo-server nil)))


(defmacro aaa [x]
  (let [m (gensym "m")]
    `(let [~m (atom 0)]
       (fn [] ~(if x
                 `(swap! ~m inc)
		 `(swap! ~m dec))))))
(defmacro aaa [x]
  `(let [m# (atom 0)]
     (fn []
       ~(if x
	  `(reset! m# inc)
	  `(reset! m# dec)))))

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
