(ns
  #^{:author "Takahiro Hozumi"
     :doc "code observetion tool."}
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
	(seq? form) (try (let [expanded (macroexpand form)]
			   (cond (elem? expanded)  expanded
				 (= 'quote (first expanded)) expanded
				 (seq? expanded)  (cons (first expanded)
							(map macroexpand-all (rest expanded)))
				 (coll? expanded) (macroexpand-all expanded)))
			 (catch java.lang.Exception e form))
	(coll? form) (conv-to form (map macroexpand-all form))))
    
(defn get-idpath1 [parent-table path]
  (loop [path path]
    (let [parent-id (parent-table (first path))]
      (if (nil? parent-id)
	(vec path)
	(recur (cons parent-id path))))))

(defn get-idpath [parent-table id]
  (get-idpath1 parent-table (if (nil? id) [] [id])))

(defn update-mem
  [{:keys [maxid result parent-table]} form v parent-id]
  (let [newid (inc maxid)
	access-vec (vec (interleave (repeat :child)
				    (conj (get-idpath parent-table parent-id)
					  newid)))
	ans (assoc (get-in result access-vec)
	      :out v,
	      :form form,
	      :id newid)]
    {:maxid newid,
     :result (assoc-in result access-vec ans),
     :parent-table (assoc parent-table newid parent-id)}))

(defn update-mem-existing-id
  [{:keys [maxid result parent-table] :as mem} form v id]
  (let [access-vec (vec (interleave (repeat :child)
				    (get-idpath parent-table id)))
	ans (assoc (get-in result access-vec)
	      :out v,
	      :form form,
	      :id id)]
    (assoc mem :result (assoc-in result access-vec ans))))

(defn allocate-id [{:keys [maxid result parent-table]} parent-id]
  (let [newid (inc maxid)
	access-vec (vec (interleave (repeat :child)
				    (conj (get-idpath parent-table parent-id)
					  newid)))]
    {:maxid newid,
     :result (assoc-in result access-vec {})
     :parent-table (assoc parent-table newid parent-id)}))

(defmacro memo-calc
  [mem form v parent-id-sym]
  `(let [catched-v# (try ~v (catch java.lang.Exception e# e#))
	 {id# :maxid, result# :result} (swap! ~mem update-mem ~form catched-v# ~parent-id-sym)
	 access-vec# (vec (conj (vec (interleave (repeat :child)
						 (get-idpath (:parent-table @~mem) id#)))
				:out))]
     (get-in result# access-vec#)))

(defmacro memo-calc-existing-id
  [mem form v id-sym]
  `(let [access-vec# (vec (cons :result
				(conj (vec (interleave (repeat :child)
						       (get-idpath (:parent-table @~mem) ~id-sym)))
				      :out)))
	 catched-v# (try ~v (catch java.lang.Exception e# e#))]
     (swap! ~mem update-mem-existing-id ~form catched-v# ~id-sym)
     (get-in @~mem access-vec#)))

(defn mem-init []
  {:maxid 0, :result (sorted-map), :parent-table {}})

(defmacro maybe-f-cap [mem form parent-id-sym]
  (if (elem? form)
    `(memo-calc ~mem '~form ~form ~parent-id-sym)
    (cond 
      (seq? form) (let [[head & tail] form]
		    (cond (special-symbol? head) `(sp-cap ~mem ~form ~parent-id-sym)
			  (or (keyword? head)
			      (func? (resolve head))
			      ;;Symbol as a function
			      (symbol? head)) (let [newid-sym (gensym "id")]
						`(let [~newid-sym (:maxid (swap! ~mem allocate-id
										 ~parent-id-sym))]
						   (memo-calc-existing-id
						    ~mem '~form
						    (apply ~head
							   (tail-cap ~mem ~tail ~newid-sym))
						    ~newid-sym)))
			      :else :error))
      ;;(elem? head) `(apply ~head (tail-cap ~mem ~tail))))
      (coll? form) (let [newid-sym (gensym "id")]
		     `(let [~newid-sym (:maxid (swap! ~mem allocate-id
						       ~parent-id-sym))]
			(memo-calc-existing-id
			 ~mem '~form
			 (conv-to ~form (tail-cap ~mem ~form ~newid-sym))
			 ~newid-sym))))))

(defmacro tail-cap [mem form parent-id-sym]
  (let [head (first form)
	tail (rest form)]
    (cond (empty? form) ()
	  (elem? head) `(cons (memo-calc ~mem '~head ~head ~parent-id-sym)
			      (tail-cap ~mem ~tail ~parent-id-sym))
	  (seq? head) `(cons (maybe-f-cap ~mem ~head ~parent-id-sym)
			     (tail-cap ~mem ~tail ~parent-id-sym))
	  (coll? head) `(cons (maybe-f-cap ~mem ~head ~parent-id-sym)
			      (tail-cap ~mem ~tail ~parent-id-sym)))))

(defmacro sp-cap [mem form parent-id-sym]
  (let [[head & tail] form]
    (let [newid-sym (gensym "id")]
      `(let [~newid-sym (:maxid (swap! ~mem allocate-id
				       ~parent-id-sym))]
	 ~(condp = head
	   'let* (let [binds (second form)
		       body (drop 2 form)
		       newbinds (vec (interleave (take-nth 2 binds)
						 (map #(list 'maybe-f-cap mem % newid-sym)
						      (take-nth 2 (rest binds)))))
		       code `(let* ~newbinds
				   ~@(map #(list 'maybe-f-cap mem % newid-sym) body))]
		   `(memo-calc-existing-id ~mem
					   '~form
					   ~code
					   ~newid-sym))
	   'def (let [[var-name [fs _ :as body]] tail]
		  `(memo-calc-existing-id
				~mem
				'~form
				(def ~var-name (maybe-f-cap ~mem ~body ~newid-sym))
				~newid-sym))
	   'fn* `(memo-calc-existing-id
		  ~mem
		  '~form
		  (fn* ~@(map (fn [arg body] (into body (list arg)))
			      (map first tail)
			      (map (fn [s] (map #(list 'maybe-f-cap mem % newid-sym) s))
				   (map rest tail))))
		  ~newid-sym)
	   'if `(memo-calc-existing-id
		 ~mem
		 '~form
		 (if ~@(map #(list 'maybe-f-cap mem % newid-sym) tail))
		 ~newid-sym)
	   'do `(memo-calc-existing-id
		 ~mem
		 '~form
		 (do ~@(map #(list 'maybe-f-cap mem % newid-sym) tail))
		 ~newid-sym)
	   'quote `(memo-calc ~mem
			      '~form
			      ~form
			      ~newid-sym)
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
	   'recur form
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
			      nil))))))

(def *max-print-size* 100)

(defn lazy-chked-v [out]
  (if (= clojure.lang.LazySeq (type out))
    (let [data (take (+ 1 *max-print-size*) out)]
      (if (= (+ 1 *max-print-size*) (count data))
	(concat data ['...LazySeq...])
	data))
    out))

(defn print-node1 [{:keys [id form out child]} level]
  (if (= form out)
    :const
    (do (println level ": + " form)
	(let [uptodate-form (atom form)
	      limited-size-v (lazy-chked-v out)]
	  (doseq [{cform :form, cout :out, :as achild} (reverse (vals child))]
	    (let [child-out (print-node1 achild (inc level))]
	      (if (not= :const child-out)
		(println level ": ->" (swap! uptodate-form
					     #(replace {cform child-out} %))))))
	  (println level ": =>" limited-size-v)
	  limited-size-v))))

(defn print-node [{:keys [result]} n]
  (print-node1 (nth (vals (:child result)) n) 0))

(defmacro cap
  ([form]
     `(let [mem# (atom (mem-init))]
	(maybe-f-cap mem# ~(macroexpand-all form) nil)
	(print-node @mem# 0)
	(get-in (:result @mem#)
		(vec (conj (vec (interleave (repeat :child)
					    (get-idpath (:parent-table @mem#) 1)))
			   :out)))))
  ([caller form]
     (let [expanded (macroexpand-all form)
	   op (first expanded)
	   name (second expanded)
	   third (first (drop 2 expanded))]
       (let [mem (gensym "mem")]	     
	 `(let [~mem (atom (mem-init))]
	    ~(if (and (= 'def op)
		      (= 'fn* (first third)))
	       (let [fnbodies (rest third)]
		 `(do (defn ~caller
			([] (print-node @~mem 0))
			([x#] (cond (number? x#) (print-node @~mem x#)
				    :else (condp = x#
					    :p (print-node @~mem)
					    :pp (pprint @~mem)
					    :c (do (reset! ~mem (mem-init))
						   @~mem)
					    :else (do (reset! ~mem (mem-init))
						      @~mem)))))
		      (def ~name
			   (fn* ~@(map (fn [arg body] (into body (list arg)))
				       (map first fnbodies)
				       (map (fn [s] (map #(list 'maybe-f-cap mem % nil) s))
					    (map rest fnbodies)))))))
	       `(cap ~form)))))))

(fn* ([x] (inc x) (dec x))
     ([x y] (+ x y) (- x y)))
