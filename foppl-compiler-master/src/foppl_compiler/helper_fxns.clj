(ns foppl-compiler.helper-fxns
  (:require [anglican.runtime :refer [sqrt sin cos log exp normal observe* sample* uniform-continuous dirichlet
                                      gamma discrete flip]]
            [anglican.core :refer :all]
            [foppl-compiler.parser :refer :all]
            [clojure.string :as str]))

;helper functions
(defn zip-sets [x y]
  (for [xi x yi y] [xi yi] ) )

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
