(ns
  #^{:author "Takahiro Hozumi"
     :doc "code observetion tool."}
  hozumi.eyewrap
  (:use [clojure.contrib.pprint]
	[clojure.set :only [difference]]))


(defn elem? [x]
  (not (coll? x)))

(defn func? [x]
  (or (ifn? x) (fn? x)))

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
  [{:keys [maxid result parent-table] :as mem} v id]
  (let [access-vec (get-access-vec parent-table id)
	ans (assoc (get-in result access-vec)
	      :out v,
	      :id id)]
    (assoc mem
      :result (assoc-in result access-vec ans))))

(defn allocate-id [{:keys [maxid result parent-table]} form parent-id]
  (let [newid (inc maxid)
	access-vec (get-access-vec parent-table parent-id newid)]
    {:maxid newid,
     :result (assoc-in result access-vec {:form form})
     :parent-table (assoc parent-table newid parent-id)}))

(defmacro memo-calc
  [mem form v parent-id-sym]
  `(let [[catched-v# err-flag#] (try [~v false]
				 (catch java.lang.Exception e#
				   [e# true]))
	 {id# :maxid} (swap! ~mem allocate-id ~form ~parent-id-sym)]
     (swap! ~mem update-mem catched-v# id#)
     (if err-flag#
       (throw catched-v#)
       catched-v#)))

(defmacro memo-calc-existing-id
  [mem v id-sym]
  `(let [[catched-v# err-flag#] (try [~v false]
				     (catch java.lang.Exception e#
				       [e# true]))]
     (swap! ~mem update-mem catched-v# ~id-sym)
     (if err-flag#
       (throw catched-v#)
       catched-v#)))

(defn mem-init []
  {:maxid 0, :result (sorted-map), :parent-table {}})

(defmacro maybe-f-cap [mem form parent-id-sym]
  (cond
    (or (elem? form) (empty? form)) `(memo-calc ~mem '~form ~form ~parent-id-sym)
    (seq? form) (let [[head & tail] form]
		  (cond (special-symbol? head) `(sp-cap ~mem ~form ~parent-id-sym)
			:else (let [newid-sym (gensym "id")]
				`(let [~newid-sym (:maxid (swap! ~mem allocate-id '~form ~parent-id-sym))]
				   (memo-calc-existing-id
				    ~mem
				    (apply (maybe-f-cap ~mem ~head ~newid-sym)
					   (tail-cap ~mem ~tail ~newid-sym))
				    ~newid-sym)))))
    (coll? form) (let [newid-sym (gensym "id")]
		   `(let [~newid-sym (:maxid (swap! ~mem allocate-id '~form
						    ~parent-id-sym))]
		      (memo-calc-existing-id
		       ~mem
		       (conv-to ~form (tail-cap ~mem ~form ~newid-sym))
		       ~newid-sym)))))

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
      `(let [~newid-sym (:maxid (swap! ~mem allocate-id '~form ~parent-id-sym))]
	 ~(condp = head
	    'let* (let [binds (second form)
			body (drop 2 form)
			newbinds (vec (interleave (take-nth 2 binds)
						  (map #(list 'maybe-f-cap mem % newid-sym)
						       (take-nth 2 (rest binds)))))
			code `(let* ~newbinds
				    ~@(map #(list 'maybe-f-cap mem % newid-sym) body))]
		    `(memo-calc-existing-id ~mem
					    ~code
					    ~newid-sym))
	    'def (let [[var-name [fs _ :as body]] tail]
		   `(memo-calc-existing-id
		     ~mem
		     (def ~var-name (maybe-f-cap ~mem ~body ~newid-sym))
		     ~newid-sym))
	    'fn* `(memo-calc-existing-id
		   ~mem
		   ~(reconst-fn mem tail newid-sym)
		   ~newid-sym)
	    'if `(memo-calc-existing-id
		  ~mem
		  (if ~@(map #(list 'maybe-f-cap mem % newid-sym) tail))
		  ~newid-sym)
	    'do `(memo-calc-existing-id
		  ~mem
		  (do ~@(map #(list 'maybe-f-cap mem % newid-sym) tail))
		  ~newid-sym)
	    'quote `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    '.     `(memo-calc-existing-id ~mem
					   (. ~@(map #(if (and (seq? %) (func? (resolve (first %))))
							(list 'maybe-f-cap mem % newid-sym) %)
						     tail))
					   ~newid-sym)
	    'new   `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'var   `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'loop* `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'recur `form
	    'try   `(memo-calc-existing-id ~mem
					   (try ~@(map (fn [exp]
							 (cond (elem? exp) (list 'maybe-f-cap mem exp newid-sym)
							       :else (let [op (first exp)]
								       (if (or (= 'catch op) (= 'finally op))
									 exp
									 (list 'maybe-f-cap mem exp newid-sym)))))
						       tail))
					   ~newid-sym)
	    'catch `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'finally `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'throw `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym)
	    'set!  `(memo-calc-existing-id ~mem
					   ~form
					   ~newid-sym))))))

(defn replace1 [target replaced form]
  (if (fn? replaced)
    form
    (condp = (first form)
      'let* (let [[_ binds & body] form
		  binds-val (take-nth 2 (rest binds))
		  [binds-val-f binds-val-l] (split-with #(not= target %) binds-val)]
	      (if (empty? binds-val-l)
		`(let* ~binds ~@(replace1 target replaced body))
		`(let* ~(vec (interleave (take-nth 2 binds)
					 (concat binds-val-f (list replaced) (rest binds-val-l))))
		       ~@body)))
      (let [[former latter] (split-with #(not= target %) form)]
	(cond
	  (empty? latter) form
	  (vector? form) (assoc form (count former) replaced)
	  :else (concat former (list replaced) (rest latter)))))))
  
(def *max-print-size* 30)

(defn print-node [{:keys [result, parent-table]} node-id style]
  (letfn
      [(lazy-chked-v
	[out]
	(if (= clojure.lang.LazySeq (type out))
	  (let [data (take (+ 1 *max-print-size*) out)]
	    (if (= (+ 1 *max-print-size*) (count data))
	      (concat data ['...LazySeq...])
	      data))
	  out))
       (my-print
	[level typ form id]
	(let [idstring (if (get style :v false) (format "%3d:" id) "")]
	  ;(println style)
	  (cond
	    (:pp style) (do (printf "%-2d:%s%2s %s" level idstring typ (apply str (repeat level "  ")))
			    (let [str-writer (java.io.StringWriter.)]
			      (pprint form str-writer)
			      (println (.trim (.replaceAll (.toString str-writer)
							   "\n"
							   (apply str (concat '("\n")
									      (repeat (if (:v style) 10 6) " ")
									      (repeat level "  "))))))))
	    (:1line style) (do (printf "%-2d:%s%2s %s" level idstring typ (apply str (repeat level "  ")))
			       (println form)))))
       (print-node1
	[{:keys [id form out child]} level]
	(cond (= form out) ::const
	      (elem? form) out
	      :else (do (my-print level "+" form id)
			(let [uptodate-form (atom form)
			      limited-size-v (lazy-chked-v out)
			      my-childs (reverse (vals child))]
			  (doseq [{cform :form, cout :out, child-childs :child, :as achild} my-childs]
			    (let [child-out (print-node1 achild (inc level))]
			      (if (and (not= ::const child-out)
				       (not (fn? child-out)))
				(my-print level "->"
					  (swap! uptodate-form
						 #(replace1 cform child-out %))
					  id))))
			  (my-print level "=>" limited-size-v id)
			  limited-size-v))))]
    (let [node (get-in result (get-access-vec parent-table node-id))]
      (if (fn? (get node :out +))
	(let [child-nodes (-> node :child vals)
	      child-nodes (if (get style :all false)
			    child-nodes
			    (list (nth child-nodes (:nth style))))]
	  (doseq [child-node (reverse child-nodes)]
	    (if (<= 2 (count child-nodes))
	      (println (apply str (repeat 100 "-"))))
	    (print-node1 child-node 0)))
	(print-node1 node 0)))))

(defmacro cap
  ([form]
     `(let [mem# (atom (mem-init))]
	(try (maybe-f-cap mem# ~(macroexpand-all form) nil)
	     (print-node @mem# nil {:nth 0, :1line true})
	     (get-in (:result @mem#)
		     (conj (get-access-vec (:parent-table @mem#) 1)
			   :out))
	     (catch java.lang.Exception e#
	       (print-node @mem# nil {:nth 0, :1line true})
	       e#))))
  ([caller form]
     (let [mem (gensym "mem")]
       `(let [~mem (atom (mem-init))]
	  (defn ~caller
	    ([] (~caller nil))
	    ([& args#] (let [[node-ids# options#] (split-with (fn [arg#] (or (number? arg#) (nil? arg#))) args#)
			     node-ids# (if (empty? node-ids#) [nil] node-ids#)
			     history-key# (first (filter
						  (fn [op-key#]
						    (nil? (re-seq #"[^0-9]"
								  (apply str (rest (str op-key#))))))
						  options#))
			     history-str#  (first (filter (fn [op-str#] (nil? (re-seq #"[^0-9]" op-str#)))
							  (map (fn [op#] (apply str (rest (str op#)))) options#)))
			     history-num# (if history-str# (Integer. history-str#) 0)
			     option-map# (assoc (apply hash-map (interleave options# (repeat true)))
					   :nth history-num#)
			     option-map# (if (or (:pp option-map#)
						  (:1line option-map#))
					   option-map#
					   (assoc option-map# :1line true))
			     ope-set# (difference (set (keys option-map#))
						  #{:v :all :pp :1line :nth history-key#})]
			 (doseq [id# node-ids#]
			   (if (<= 2 (count node-ids#))
			     (println (apply str (repeat 100 "="))))
			   (print-node @~mem id# option-map#))
			 (doseq [opt# ope-set#]
			   (condp = opt#
			     :i (pprint @~mem)
			     :c    (do (reset! ~mem (mem-init))
				       @~mem)
			     (println "id (e.g. 2, 9, 12..) - print only specific expression which ids correspond to.
:v - print id.
:pp - pprint trace log.
:number (e.g. :1, :2, :3..)  - print old trace log.
:all - print all trace log.
:i - print internal data.
:c - clear cache."))))))
	  ~(let [expanded (macroexpand-all form)
		 fs (first expanded)
		 se (second expanded)
		 third (first (drop 2 expanded))]
	     (cond (and (= 'def fs)
			(= 'fn* (first third))) `(def ~se
						      ~(reconst-fn mem (rest third) nil))
			(= 'fn* fs) (reconst-fn mem (rest expanded) nil)
			:else `(maybe-f-cap ~mem ~expanded nil)))))))

