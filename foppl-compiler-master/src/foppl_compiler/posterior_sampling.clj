(ns foppl-compiler.posterior_sampling
  (:require [anglican.runtime :refer [log exp normal observe* sample* uniform-continuous]]
            [anglican.core :refer :all]
            [clojure.string :as str]))

(defn clojure-dependencies-suck [quoted-expression] ;fixes clojures bullshit
  (clojure.walk/postwalk-replace {'observe* 'anglican.runtime/observe*
                                  'sample* 'anglican.runtime/sample*
                                  'normal 'anglican.runtime/normal
                                  'sqrt 'anglican.runtime/sqrt
                                  'dirichlet 'anglican.runtime/dirichlet
                                  'gamma 'anglican.runtime/gamma
                                  'discrete 'anglican.runtime/discrete
                                  'dirac 'foppl-compiler.posterior_sampling/dirac
                                  'flip 'anglican.runtime/flip}
      quoted-expression))



(defn dirac [point-mass] (normal point-mass 0.0005))

; (def distribution-types ;also includes function to check support
;   {'discrete {'discrete (fn [integer states] (if (> integer states) )) 'flip (fn [integer states] (if (> integer states) )) }
;    'continuous {normal dirichlet gamma dirac}})
;
; (def continuous-distributions )
;
;
; (defn get-continuous-distributions [G]
;   (remove nil?
;     (for [link-funcitons (get G :Y)]
;       (if )
;       ))
;   )

(defn get-pdf-eval [quoted-sample value] ;returns log pdf eval
  (if (str/includes? (str quoted-sample) "observe")
      (eval (clojure-dependencies-suck quoted-sample))
      (eval (clojure-dependencies-suck (read-string
          (str (str/join "" (drop-last (str
            (clojure.walk/postwalk-replace {'sample* 'observe*} quoted-sample)))) " " value ")" ))))))

(defn sample-from-proposal-dist [G current-sample variable] ; returns a 1d sample from prior
      (eval (clojure-dependencies-suck
            (clojure.walk/postwalk-replace current-sample (get (get G :P) variable))))
      )

(defn proposal-pdf [G current-sample variable] ;returns log liklyhood
  (get-pdf-eval (clojure.walk/postwalk-replace current-sample (get (get G :P) variable)) (get current-sample variable)))

(defn get-unobserved-rv [G]
    (let [vertices (get G :V)]
      (filter #(re-find #"sample" (str %)) vertices)
    ))

(defn evaluate-joint [G sample variable] ; returns the log of the joint
  ; find all link functions that contain variables
  (let [relevent-link-fxns (into [] (remove nil?
                           (for [link-functions (get G :P)]
                              (if (str/includes? (str (val link-functions)) (str variable))
                              link-functions nil))))]
      (+ (reduce + (for [link-function relevent-link-fxns]
            (get-pdf-eval (clojure.walk/postwalk-replace sample (val link-function)) (get sample (key link-function)) ) ) )
         (get-pdf-eval (clojure.walk/postwalk-replace sample (get (get G :P) variable)) (get sample variable))) ))

(defn Accept [G variable current-sample new-sample current-joint-pdf new-joint-pdf] ;returns 1d rv
  (let [U (sample* (uniform-continuous 0 1)) ;working in log space so we change the algo a bit
        alpha (min 1 (exp (- (+ new-joint-pdf (proposal-pdf G current-sample variable) )
                             (+ current-joint-pdf (proposal-pdf G new-sample variable))))) ]
      (if (< U alpha)
        new-sample
        current-sample)))

(defn Gibbs-step [initial-state G]
  (let [sampled-variables (into [] (get-unobserved-rv G))] ;gets all unobserved vertices
    (loop [current-sample initial-state
           iter 0]
        (if (>= iter (count sampled-variables))
            current-sample
            (recur
                  (let [new-sample (assoc current-sample (nth sampled-variables iter)
                                         (if (seq? (sample-from-proposal-dist G current-sample (nth sampled-variables iter)))
                                         (into [] (sample-from-proposal-dist G current-sample (nth sampled-variables iter)))
                                                  (sample-from-proposal-dist G current-sample (nth sampled-variables iter)))  )]
                      (Accept G
                              (nth sampled-variables iter)
                              current-sample
                              new-sample
                              (evaluate-joint G current-sample (nth sampled-variables iter))
                              (evaluate-joint G new-sample (nth sampled-variables iter))) )
                  (inc iter))))))

(defn value-sets [& maps]
  (apply merge-with into (for [m maps, [k v] m] {k [v]})))

(defn Gibbs-sampler [initial-state G E S burn-in thinning printing]
    (into [] (loop [iter 0
                    vairable-samples  [ initial-state ] ]
        (if (= 0 (mod iter printing)) (println "samples generated: " iter " out of " S " with burn in" burn-in))
        (cond
          (>= iter S)
            vairable-samples
          (and (< iter S ) (> iter burn-in ))
            (if (= 0 (mod iter thinning))
              (recur (inc iter) (conj vairable-samples
                    (assoc (Gibbs-step (last vairable-samples) G)
                           'sampleprevoutput
                           (eval (clojure.walk/postwalk-replace (last vairable-samples) E))) ))
              (recur (inc iter) (conj (drop-last vairable-samples)
                    (assoc (Gibbs-step (last vairable-samples) G)
                           'sampleprevoutput
                           (eval (clojure.walk/postwalk-replace (last vairable-samples) E))))))
          (and (< iter S ) (<= iter burn-in ))
            (recur (inc iter) [(Gibbs-step (last vairable-samples) G)] )
                       ))))

(defn sum-over-rows [A]
    (let [columns (count (first A))
          rows (count A)]
          (for [column (take columns (range))]
              (reduce + (for [row (take rows (range))]
                (nth (nth A row) column))))))

(defn matrix-average [A, n]
  (let [sum-vector (sum-over-rows A)]
    (into [] (for [val sum-vector] (/ val n)))))

(defn tf-average [v]
  (let [tf (keys (frequencies v))
        bins (vals (frequencies v))]
      (if (= 1 (count (keys (frequencies v))))
        (if (= true (keys (frequencies v)))
        {true (nth bins 0) false 0}
        {true 0 false (nth bins 0)})
        {(nth tf 0) (/ (nth bins 0) (reduce + bins))
         (nth tf 1) (/ (nth bins 1) (reduce + bins))})))

; (defn lazy-Gibbs
; 	([state G E]
;     (lazy-seq (cons state (lazy-Gibbs
;       (assoc (Gibbs-step state G) 'sampleprevoutput
;       (eval (clojure-dependencies-suck (clojure.walk/postwalk-replace state E)))) G E)))))
;
; (defn lazy-mean [lazy-gibs-seq]
;  ; takes lazy sequence and sums it recursively
;  (loop [x lazy-gibs-seq
;         sum 0]
;        (if (empty? x)
;          sum
;         (recur (rest x) (my-add (my-add (first x)) sum)))))
; ;
; ; (defn get-averages [lazy-gibs-seq]
; ;     ()
; ;   )
; ; (defn add-samples [sample1 sample2]
; ;   (for [rv (keys sample1)])
; ;   )
;
; (defn my-add-helper [x y]
;  (cond
;    (and (vector? x) (vector? y))
;      (map + x y)
;    (and (bool? x) (bool? y))
;      {true (+ (if x 1 0) (if y 1 0)) false (+ (if x 0 1) (if y 0 1))}
;    (and (map? x) (bool? y))
;      {true (+ (get x true) (if y 1 0)) false (+ (get x false) (if y 0 1))}
;    (and (bool? x) (map? y))
;      {true (+ (get y true) (if x 1 0)) false (+ (get y false) (if x 0 1))}
;    (and (map? x) (map? y))
;      {true (+ (get y true) (get x true)) false (+ (get y false) (get x true))}
;    :else
;      (+ x y)
;    ))
;
; (defn my-average [x n]
;   (cond
;     (vector? x)
;       (for [x_ x] (/ x n))
;     (map? x)
;       {(nth (keys x) 0) (/ (nth (vals x) 0) n)
;        (nth (keys x) 1) (/ (nth (vals x) 1) n)}
;     :else
;       (/ x n)
;   ))
