; required functions
(defn append [& args] (apply conj args))
(defn mat-mul [& args] (apply m/mmul args))
(defn mat-add [& args] (apply m/add args))
(defn mat-transpose [& args] (apply m/transpose args))
(defn mat-tanh [M] (m/emap tanh M))
(defn mat-relu [M] (m/emap (fn [x] (if (> x 0) x 0)) M))
(defn mat-repmat [M r c]
  (let [R (reduce (partial m/join-along 0) (repeat r M))]
    (reduce (partial m/join-along 1) (repeat c R))))
(defn sample [x] (sample* x))


;helper functions
(defn zip-sets [x y]
  (for [xi x yi y] [xi yi] ) )

(defn vec-remove
 "remove elem in coll"
 [coll pos]
 (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))
(defn contains-val?
 [coll val]
 (reduce #(if (= val %2) (reduced true) %1) false coll))

(defn contains-vector [vector-ish-thing]
   (not (every? #{0}
     (for [x  vector-ish-thing]
       	(if (vector? x) 1 0) ))))

(defn split-at' [idx v]
   [ [(subvec v 0 idx)] [(subvec v idx)] ] )

(defn get-lets-pairs [current-vector]
	(cond
   	(= 2 (count current-vector))
     		[current-vector]
     	(= 4 (count current-vector))
     		(split-at 2 current-vector)
     	(not (= 0 (mod (count current-vector) 2)))
     		(println "error in get-lets-pairs")
     	:else
     		(conj (get-lets-pairs (get (split-at 2 current-vector) 1)) (get (split-at 2 current-vector) 0))))

(defn get-random-variables [let-definition]
  (remove nil? (for [definition let-definition]
  	(cond
      (not (nil? (some #(= :sample %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :observe %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :normal %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :bernoulli %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :discrete %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :beta %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :uniform-continuous %) (flatten definition))))
      	(nth definition 3)
    :else
      nil ))))

(defn replace-val-in-nested-vector [nested-vector value replacement]
  (into [] (for [element nested-vector]
    (cond
      (= value element)
      	replacement

      (vector? element)
      	(replace-val-in-nested-vector element value replacement)

      :else
      	element ))))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn conj-if-not-present [v1 v2]
  (into [] (flatten (conj v1 (into [] (remove nil? (for [v v2] (if (nil? (some #(= v %) v1)) v nil)))))))
  )
