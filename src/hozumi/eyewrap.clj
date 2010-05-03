(ns
  #^{:author "Takahiro Hozumi"
     :doc "code observetion tool."}
  hozumi.eyewrap
  (:use [clojure.contrib.pprint]))


(defn elem? [x]
  (not (coll? x)))

(defn conv-to [type coll]
  (cond (vector? type) (vec coll)
	(map? type) (apply merge {} coll)
	(set? type) (set coll)
	(list? type) (apply list coll)))

(defn macroexpand-all [form]
  (cond (elem? form) form
	(seq? form) (let [expanded (macroexpand form)]
		      (cond (elem? expanded)  expanded
			    (and (coll? expanded) (empty? expanded)) expanded
			    (= 'quote (first expanded)) expanded
			    (seq? expanded)  (cons (macroexpand-all (first expanded))
						   (map macroexpand-all (rest expanded)))
			    (coll? expanded) (macroexpand-all expanded)))
	(coll? form) (conv-to form (map macroexpand-all form))))

(defn get-idpath [parent-table id]
  (letfn [(get-idpath1  [parent-table path]
			(loop [path path]
			  (let [parent-id (parent-table (first path))]
			    (if (nil? parent-id)
			      (vec path)
			      (recur (cons parent-id path))))))]
    (get-idpath1 parent-table (if (nil? id) [] [id]))))

(defn get-access-vec
  ([parent-table id]
     (vec (interleave (repeat :child)
		      (get-idpath parent-table id))))
  ([parent-table id newid]
     (vec (interleave (repeat :child)
		      (conj (get-idpath parent-table id)
			    newid)))))

(defn update-mem
  [{:keys [maxid result parent-table]} form v parent-id]
  (let [newid (inc maxid)
	access-vec (get-access-vec parent-table parent-id newid)
	ans (assoc (get-in result access-vec)
	      :out v,
	      :form form,
	      :id newid)]
    {:maxid newid,
     :result (assoc-in result access-vec ans),
     :parent-table (assoc parent-table newid parent-id)}))

(defn update-mem-existing-id
  [{:keys [maxid result parent-table] :as mem} form v id]
  (let [access-vec (get-access-vec parent-table id)
	ans (assoc (get-in result access-vec)
	      :out v,
	      :form form,
	      :id id)]
    (assoc mem :result (assoc-in result access-vec ans))))

(defn allocate-id [{:keys [maxid result parent-table]} parent-id]
  (let [newid (inc maxid)
	access-vec (get-access-vec parent-table parent-id newid)]
    {:maxid newid,
     :result (assoc-in result access-vec {})
     :parent-table (assoc parent-table newid parent-id)}))

(defmacro memo-calc
  [mem form v parent-id-sym]
  `(let [catched-v# (try ~v (catch java.lang.Exception e# e#))
	 {id# :maxid, result# :result} (swap! ~mem update-mem ~form catched-v# ~parent-id-sym)
	 access-vec# (conj (get-access-vec (:parent-table @~mem) id#) :out)]
     (get-in result# access-vec#)))

(defmacro memo-calc-existing-id
  [mem form v id-sym]
  `(let [access-vec# (-> [:result]
			 (into ,,, (get-access-vec (:parent-table @~mem) ~id-sym))
			 (into ,,, [:out]))
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
			  :else (let [newid-sym (gensym "id")]
				  `(let [~newid-sym (:maxid (swap! ~mem allocate-id ~parent-id-sym))]
				     (memo-calc-existing-id
				      ~mem '~form
				      (apply (maybe-f-cap ~mem ~head ~newid-sym)
					     (tail-cap ~mem ~tail ~newid-sym))
				      ~newid-sym)))))
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

(defn reconst-fn [mem body newid-sym]
  (let [uniformed-body (if (vector? (first body)) (list body) body)]
    `(fn* ~@(map (fn [arg body] (list arg body))
		 (map first uniformed-body)
		 (map #(if (= 1 (count %))
			 `(maybe-f-cap ~mem ~@% ~newid-sym)
			 `(maybe-f-cap ~mem (do ~@%) ~newid-sym))
		      (map rest uniformed-body))))))

(defmacro sp-cap [mem form parent-id-sym]
  (let [[head & tail] form]
    (let [newid-sym (gensym "id")]
      `(let [~newid-sym (:maxid (swap! ~mem allocate-id ~parent-id-sym))]
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
		   ~(reconst-fn mem tail parent-id-sym)
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
	    'quote `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    '.     `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'new   `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'var   `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'loop* `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'recur `form
	    'try   `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'throw `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'catch `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym)
	    'set!  `(memo-calc-existing-id ~mem
					   '~form
					   ~form
					   ~newid-sym))))))
  
(def *max-print-size* 100)

(defn lazy-chked-v [out]
  (if (= clojure.lang.LazySeq (type out))
    (let [data (take (+ 1 *max-print-size*) out)]
      (if (= (+ 1 *max-print-size*) (count data))
	(concat data ['...LazySeq...])
	data))
    out))

(defn my-print [level typ form style]
  (condp = style
    :pp (do (printf "%-2d:%2s %s" level typ (apply str (repeat level "  ")))
	 (let [str-writer (java.io.StringWriter.)]
	   (pprint form str-writer)
	   (println (.trim (.replaceAll (.toString str-writer)
					"\n"
					(apply str (concat '("\n") (repeat 6 " ")
							   (repeat level " "))))))))
    :1line (do (printf "%-2d:%2s %s" level typ (apply str (repeat level "  ")))
	       (println form))))

(defn print-node1 [{:keys [id form out child]} level style]
  (cond (= form out) :const
	(elem? form) out
	:else (do (my-print level "+" form style)
		  (let [uptodate-form (atom form)
			limited-size-v (lazy-chked-v out)
			[first-child :as my-childs] (reverse (vals child))
			target-childs (cond (and (seq? form)
						 (or (special-symbol? (first form))
						     (coll? (:out first-child)))) my-childs
						 (and (seq? form)
						      (elem? (:out first-child))) (rest my-childs)
						 (coll? form) my-childs)]
		    (doseq [{cform :form, cout :out, :as achild} target-childs]
		      (let [child-out (print-node1 achild (inc level) style)]
			(if (not= :const child-out)
			  (my-print level "->" (swap! uptodate-form #(replace {cform child-out} %)) style))))
		    (my-print level "=>" limited-size-v style)
		    limited-size-v))))

(defn print-node [{:keys [result]} n style]
  (print-node1 (nth (vals (:child result)) n) 0 style))

(defmacro cap
  ([form]
     `(let [mem# (atom (mem-init))]
	(maybe-f-cap mem# ~(macroexpand-all form) nil)
	(print-node @mem# 0 :1line)
	(get-in (:result @mem#)
		(conj (get-access-vec (:parent-table @mem#) 1)
		      :out))))
  ([caller form]
     (let [expanded (macroexpand-all form)
	   op (first expanded)
	   name (second expanded)
	   third (first (drop 2 expanded))]
       (let [mem (gensym "mem")]	     
	 `(let [~mem (atom (mem-init))]
	    ~(if (and (= 'def op)
		      (= 'fn* (first third)))
	       (do `(do (defn ~caller
			  ([] (print-node @~mem 0 :1line))
			  ([x#] (cond (number? x#) (print-node @~mem x# :1line)
				      :else (condp = x#
					      :pp (print-node @~mem 0 :pp)
					      :internal (pprint @~mem)
					      :c (do (reset! ~mem (mem-init))
						     @~mem)
					      (println ":pp - pprint trace log.
number - print old trace log.
:internal - print internal data.
:c - clear cache.")))))
			(def ~name
			     ~(let [fnbodies (rest third)]
				(reconst-fn mem fnbodies nil)))))
	       `(cap ~form)))))))

