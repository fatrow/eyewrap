
(ns
  #^{:author "Takahiro Hozumi"
     :doc "Code observation tool."}
  hozumi.capture-clj
  (:use [clojure.contrib.pprint]))  

(defn elem? [x]
  (not (coll? x)))

(defn func? [x]
  (or (ifn? x) (fn? x)))

(defn macro? [name]
  (cond (symbol? name) (:macro (meta (resolve name)))
	(var? name) (:macro (meta name))))
	

(defn conv-to [type coll]
  (cond (vector? type) (vec coll)
	(map? type) (apply merge {} coll)
	(set? type) (set coll)
	(list? type) (apply list coll)))

(defn macroexpand-all [form]
  (cond (elem? form) form
	(seq? form) (if (and (symbol? (first form))
			     (macro? (first form)))
		      (let [expanded (macroexpand form)]
			(cons (first expanded)
			      (map macroexpand-all (rest expanded))))
		      (cons (macroexpand-all (first form))
			    (map macroexpand-all (rest form))))
	(coll? form) (conv-to form (map macroexpand-all form))))


(defn argtype [lis]
  (let [current-arg-num (count (rest lis))
	arg-lists (reduce (fn [m x] (assoc m (count x) x)) {}
			  (:arglists (meta (resolve (first lis)))))]
    (if-let [atype (arg-lists current-arg-num)]
      atype
      (arg-lists (apply max (keys arg-lists))))))

(declare caprest)
(defn cap [stack [head & tail :as form]]
  (println :s stack :f form :cap)
  (cond (elem? form) (conj stack form)
	(empty? form) (conj stack form)
	(list? form) (if (func? head)
		       (caprest (conj stack form) tail)
		       (caprest (conj stack form) form))
	:else (caprest (conj stack form) form)))

(defn caprest [stack [head & tail :as form]]
  (println :s stack :f form :caprest)
  (cond (elem? form) (conj stack form)
	(empty? form) (conj stack form)
	:else (do (println head)
		  (cond (list? head) (caprest (cap stack head) tail)
			(coll? head) (caprest (caprest stack head) tail)
			:else (caprest (conj stack head) tail)))))

(defmacro visu [form]
  (letfn [(make-table [m node]
		      (if (coll? node)
			(reduce (fn [ma lis]
				  (assoc ma lis (eval lis))))))]))
(defn capn [mem form]
  (cap mem form))

(defn update-mem [mem form v]
  (let [newid (inc (:maxid mem))]
    {:maxid newid :result (assoc (:result mem) newid {:form form :out v})}))

(defmacro memo-calc [mem form v]
  `(let [{id# :maxid} (swap! ~mem update-mem ~form ~v)]
     (get-in @~mem [:result id# :out])))

(defmacro capfront [form]
  `(let [mem# (atom {:maxid 0, :result {}})]
     (maybe-f-cap mem# ~(macroexpand-all form))
     mem#))

(declare tail-cap sp-cap)

(defmacro maybe-f-cap [mem form]
  (cond (elem? form) `(memo-calc ~mem ~form)
	(seq? form) (let [[head & tail] form]
		      (cond (func? (resolve head)) `(memo-calc ~mem '~form (apply ~head (tail-cap ~mem ~tail)))
			    (special-symbol? head) `(sp-cap ~mem ~form)
			    (elem? head) `(apply ~head (tail-cap ~mem ~tail))))
	(coll? form) `(memo-calc ~mem '~form (conv-to ~form (tail-cap ~mem ~form)))))

(defmacro tail-cap [mem [head & tail :as form]]
  (cond (empty? form) ()
	(elem? head) `(cons (memo-calc ~mem '~head ~head)
			    (tail-cap ~mem ~tail))
	(seq? head) `(cons (maybe-f-cap ~mem ~head)
			    (tail-cap ~mem ~tail))
	(coll? head) `(cons (maybe-f-cap ~mem ~head)
			    (tail-cap ~mem ~tail))))

(defmacro sp-cap [mem form]
  (let [[head & tail] form]
    (cond (= 'let* head) (let [binds (second form)
			       body (drop 2 form)]
			   (concat (list 'let*)
				   (list (vec (interleave (take-nth 2 binds)
							  (map #(list 'maybe-f-cap mem %)
							       (take-nth 2 (rest binds))))))
				   (map #(list 'maybe-f-cap mem %) body)))
	  (= 'def head) (let [[name [fs _ :as body]] tail]
			  (cond (special-symbol? fs) `(def ~name (sp-cap ~mem ~body))
				:else `(def ~name (maybe-f-cap ~mem ~body))))
	  (= 'fn* head) ;(let [binds fnbody] (first tail)
	  (fn* (interleave (map first tail) (map (map (rest %)tail))(maybe-f-cap ~mem ~fnbody)))
	  (seq? (second form)) (let [[binds fnbody] (first tail)]
						     `(fn* ~binds (maybe-f-cap ~mem ~fnbody)))
	  (= 'do head) `(do ~@(map #(list 'maybe-f-cap mem %) tail)))))

