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

; (defn get-continuous-distributions [G]
;   (remove nil?
;     (for [link-funcitons (get G :Y)]
;     ))
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

(defn Gibbs-sampler [initial-state G S burn-in]
    (into [] (loop [iter 0
                    samples [initial-state]]
        (if (= 0 (mod iter 1000)) (println "samples generated: " iter " out of " S))
        (cond
          (>= iter S)
            samples
          (and (< iter S ) (> iter burn-in ))
            (recur (inc iter) (conj samples (Gibbs-step (last samples) G) ))
          (and (< iter S ) (<= iter burn-in ))
            (recur (inc iter) [(Gibbs-step (last samples) G)] )
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
      {(nth tf 0) (/ (nth bins 0) (reduce + bins))
       (nth tf 1) (/ (nth bins 1) (reduce + bins))}
        ))


; (defn calculate-covariance-matrix [samples]
;   ; first make a hashmap for the columns of each variable
;   (zipmap (keys (first samples)) (take (count (keys (first samples))) (range)))
;   ;now we iterate through samples and build a matrix
;
;
;   )
