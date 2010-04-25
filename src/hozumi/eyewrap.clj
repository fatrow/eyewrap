(ns
  #^{:author "Takahiro Hozumi"
     :doc "Execute observetion tool."}
  hozumi.eyewrap
  (:use [clojure.contrib.pprint])
  (:use [clojure.contrib.seq-utils :only (flatten)]))

(defn elem? [x]
  (not (coll? x)))

(defn func? [x]
  (or (ifn? x) (fn? x)))

(defn macro? [sym]
  (let [ans (cond (symbol? sym) (:macro (meta (resolve sym)))
		  (var? sym) (:macro (meta sym)))]
    (if (= nil ans) false ans)))	

(defn conv-to [type coll]
  (cond (vector? type) (vec coll)
	(map? type) (apply merge {} coll)
	(set? type) (set coll)
	(list? type) (apply list coll)))

(defn macroexpand-all [form]
  (cond (elem? form) form
	(seq? form) (try (if (and (symbol? (first form))
				  (macro? (first form)))
			   (let [expanded (macroexpand form)]
			     (if (= expanded nil)
			       nil
			       (cons (first expanded)
				     (map macroexpand-all (rest expanded)))))
			   (cons (macroexpand-all (first form))
				 (map macroexpand-all (rest form))))
			 (catch java.lang.Exception e form))
	(coll? form) (conv-to form (map macroexpand-all form))))

(defn- update-mem
  ([mem form v childs]
     (let [newid (inc (:maxid mem))
	   ans1 (if childs
		  (assoc {} :childs childs)
		  {})
	   ans2 (assoc ans1
		  :out v,
		  :form form,
		  :id newid)]
       {:maxid newid :result (assoc (:result mem)
			       newid ans2)})))

(defmacro memo-calc
  ([mem form v]
     `(memo-calc ~mem ~form ~v nil))
  ([mem form v childs]
     `(let [catched-v# (try ~v (catch java.lang.Exception t# t#))
	    {id# :maxid} (swap! ~mem update-mem ~form catched-v# ~childs)]
	(get-in @~mem [:result id# :out]))))

(defn- id-reverse [{result :result :as mem}]
  (let [k (keys result)
	v (vals result)]
    (assoc mem :result (apply hash-map (interleave (reverse k) v)))))

(defn- mem-init []
  {:maxid 0, :result (sorted-map)})

(declare tail-cap sp-cap)

(defmacro maybe-f-cap [mem form]
  (cond (elem? form) `(memo-calc ~mem '~form ~form)
	(seq? form) (let [[head & tail] form]
		      (cond (special-symbol? head) `(sp-cap ~mem ~form)
			    (or (keyword? head)
				(func? (resolve head))
					;Symbol as a function
				(symbol? head)) `(memo-calc ~mem
							    '~form
							    (apply ~head
								   (tail-cap ~mem ~tail))
							    (count '~tail))
				:else :error))
					;(elem? head) `(apply ~head (tail-cap ~mem ~tail))))
	(coll? form) `(memo-calc ~mem '~form (conv-to ~form (tail-cap ~mem ~form))
				 ~(count form))));(if (map? form) (* 2(count form)) (count form))))

(defmacro tail-cap [mem form]
  (let [head (first form)
	tail (rest form)]
    (cond (empty? form) ()
	  (elem? head) `(cons (memo-calc ~mem '~head ~head)
			      (tail-cap ~mem ~tail))
	  (seq? head) `(cons (maybe-f-cap ~mem ~head)
			     (tail-cap ~mem ~tail))
	  (coll? head) `(cons (maybe-f-cap ~mem ~head)
			      (tail-cap ~mem ~tail)))))

(defmacro sp-cap [mem form]
  (let [[head & tail] form]
    (condp = head
      'let* (let [binds (second form)
		  body (drop 2 form)
		  newbinds (vec (interleave (take-nth 2 binds)
					    (map #(list 'maybe-f-cap mem %)
						 (take-nth 2 (rest binds)))))
		  code `(let* ~newbinds
			      ~@(map #(list 'maybe-f-cap mem %) body))]
	      `(memo-calc ~mem
			  '~form
			  ~code
			  ~(+ (-> (count binds) (/ ,,, 2)) (count body))))
      'def (let [[name [fs _ :as body]] tail]
	     (cond (special-symbol? fs) `(def ~name (sp-cap ~mem ~body))
		   :else `(def ~name (maybe-f-cap ~mem ~body))))
      'fn* `(memo-calc ~mem
		       '~form
		       (fn* ~@(map (fn [arg body] (into body (list arg)))
				   (map first tail)
				   (map (fn [s] (map #(list 'maybe-f-cap mem %) s))
					(map rest tail))))
		       ~(count tail))
      'if `(memo-calc ~mem
		      '~form
		      (if ~@(map #(list 'maybe-f-cap mem %) tail))
		      2)
      'do `(memo-calc ~mem
		      '~form
		      (do ~@(map #(list 'maybe-f-cap mem %) tail))
		      ~(count tail))
      'quote `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      '.     `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'new   `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'var   `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'loop* `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'recur `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'try   `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'throw `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'catch `(memo-calc ~mem
			 '~form
			 ~form
			 nil)
      'set!  `(memo-calc ~mem
			 '~form
			 ~form
			 nil))))

(defn- min-id [node]
  (if (:childs node)
    (apply min (:id node) (map min-id (:child-node node)))
    (:id node)))

(defn- makenode
  ([result id]
     (makenode result id 1))
  ([result id need-num]
     (if (zero? need-num) ()
	 (let [current (result id)
	       childs-num (:childs current)]
	   (cond
	     (nil? childs-num) (cons current
				     (makenode result (dec id) (dec need-num)))
	     :else (let [child-node (reverse (makenode result (dec id) childs-num))
			 minid (apply min (map min-id child-node))]
		     (cons (assoc current
			     :child-node child-node)
			   (makenode result
				     (dec minid)
				     (dec need-num)))))))))

(defn- print-node1 [{:keys [form out childs child-node]} level]
  (if (= form out)
    :const
    (do (println level ": + " form)
	(let [uptodate-form (atom form)]
	  (doseq [{cform :form, cout :out, :as child} child-node]
	    (if (not= :const (print-node1 child (inc level)))
	      (println level ": ->" (swap! uptodate-form
					   #(replace {cform cout} %))))))
	(println level ": =>" out))))

(defn- print-node [{:keys [maxid result]}]
  (print-node1 (first (makenode result maxid)) 0))

(defmacro cap [form]
  (let [expanded (macroexpand-all form)
	op (first expanded)
	name (second expanded)
	third (first (drop 2 expanded))]
    (let [mem (gensym "mem")]
      `(let [~mem (atom (mem-init))]
	 ~(if (and (= 'def op)
		   (= 'fn* (first third)))
	    (let [fnbodies (rest third)]
	      `(def ~name
		    (fn* ~@(map (fn [arg body] (into body (list arg)))
				(map first fnbodies)
				(map #(concat `((reset! ~mem (mem-init)))
					      %
					      `((print-node @~mem))
					      `((get-in @~mem [:result (:maxid @~mem) :out])))
				     (map (fn [s] (map #(list 'maybe-f-cap mem %) s))
					  (map rest fnbodies)))))))
	    `(do (maybe-f-cap ~mem ~(macroexpand-all form))
		 (print-node @~mem)
		 (get-in @~mem [:result (:maxid @~mem) :out])))))))

(fn* ([x] (inc x) (dec x))
     ([x y] (+ x y) (- x y)))

;(([x] (inc x) (dec x))
; ([x y] (+ x y) (- x y)))

;([1 2] (:a :b) (:c :d)) ([3 4] (:e :f) (:g :h))

;(([1 2] (macro1 (:a :b)) (macro1 (:c :d)))
; ([3 4] (macro1 (:e :f)) (macro1 (:g :h))))

