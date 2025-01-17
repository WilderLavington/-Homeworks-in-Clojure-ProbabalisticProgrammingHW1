(ns foppl-compiler.core
  "This namespace contains an implementation of a FOPPL compiler following chapter
  3.1 of 'An Introduction to Probabilistic Programming' by van de Meent et al."
  (:require [clojure.set :as set]
            ;[anglican.runtime :refer :all]
            [anglican.runtime :refer [sample* observe* normal log flip sqrt]]
            [clojure.string :as str]
            [foppl-compiler.desugar :refer [desugar]]
            [foppl-compiler.parser :refer :all]
            [foppl-compiler.posterior_sampling :refer :all]
            [foppl-compiler.hmc :refer :all]
            [foppl-compiler.auto-diff :refer :all]
            [foppl-compiler.auto-diff :refer :all]
            [foppl-compiler.interpretor :refer :all]
            [foppl-compiler.substitute :refer [substitute]]
            [foppl-compiler.partial-evaluation :refer [partial-evaluation]]
            [foppl-compiler.symbolic-simplify :refer [symbolic-simplify]]
            [foppl-compiler.primitives :refer :all]
            [foppl-compiler.analyze :refer [analyze empty-env empty-graph]]
            [foppl-compiler.free-vars :refer [free-vars]]))

(def ^:dynamic *primitive-procedures*
  "primitive procedures for Anglican semantics" ;; TODO check implications of this choice
  (let [;; higher-order procedures cannot be primitive
        exclude '#{loop
                   map reduce
                   filter keep keep-indexed remove
                   repeatedly
                   every? not-any? some
                   every-pred some-fn
                   comp juxt partial}
        ;; runtime namespaces
        runtime-namespaces '[clojure.core anglican.runtime foppl-compiler.primitives]]
    (set (keep (fn [[k v]]
                 (when (and (not (exclude k))
                            (fn? (var-get v)))
                   k))
               (mapcat ns-publics runtime-namespaces)))))


(defn graph->instructions [[rho G E]]
  (conj
   (vec
    (for [t (topo-sort G)]
      [t ((:P G) t)]))
   [:return E]))

(defn eval-instructions [instructions]
  (reduce (fn [acc [s v]]
            (let [scope (list 'let (vec (apply concat acc))
                              v)]
              (binding [*ns* (find-ns 'foppl-compiler.core)]
                (conj acc [s (eval scope)]))))
          []
          instructions))

(defn program->graph [p]
  (reduce (fn [[rho G E] exp]
            (analyze rho true exp))
          [empty-env empty-graph nil]
          p))

(defn count-vertices [G]
  (count (:V G)))

(defn count-edges [G]
  (count (apply concat (vals (:A G)))))

(defn sample-from-prior [G]
  (-> G
     graph->instructions
     eval-instructions))

(defn observes->samples [instructions]
  (reduce (fn [acc ix]
            (let [[sym v] ix]
              (if (re-find #"observe\d+" (name sym))
                (if (= (first v) 'if)
                  (let [[_ cond [_ dist _] _] v]
                    (binding [*ns* (find-ns 'foppl-compiler.core)]
                      (if (eval (list 'let (vec (apply concat acc))
                                      cond))
                        (conj acc [sym (list 'sample* dist)])
                        acc)))
                  (let [[_ dist _] v]
                    (conj acc [sym (list 'sample* dist)])))
                (conj acc ix))))
          []
          instructions))

(defn sample-from-joint [G]
  (-> G
     graph->instructions
     observes->samples
     eval-instructions))

(defn bind-free-variables [G])

(defn count-graph [code]
  (let [G
        (->> code
             (map partial-evaluation)
             (map symbolic-simplify)
             (map desugar)
             program->graph
             second
             )]
    [(count-vertices G) (count-edges G)]))

(defn get-graph [code]
  (->> code
             (map partial-evaluation)
             (map symbolic-simplify)
             (map desugar)
             program->graph

             ))

; (defn get-initial-value [G]
;    (let [ordered-sample-variables (filter #(re-find #"sample" (str %)) (topo-sort G))]
;         (loop [link-fxns (get G :P)
;               idx 0]
;           (if (>= idx (count ordered-sample-variables))
;               link-fxns
;               (recur (let [new-key {(nth ordered-sample-variables idx)
;                                     (if (seq? (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))
;                                               (into [] (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))
;                                               (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))}
;                            remaining-link-fxns (dissoc link-fxns (nth ordered-sample-variables idx))]
;                            (merge (clojure.walk/postwalk-replace new-key remaining-link-fxns) new-key))
;                     (inc idx))))))


;(load-file "/Users/wilder/Desktop/foppl-compiler-master/src/foppl_compiler/posterior_sampling.clj")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "==================================================================")
  (println "==========================HOMEWORK 5==============================")
  (println "==================================================================")
  (println "Homework 5: Problem 1 \n")
  (def hoppl-code '(
    (defn until-success [p n]
      (let [dist (flip p)]
           (if (sample dist)
           n
           (until-success p (+ n 1)))))
    (let [p 0.01]
     (until-success p 0))))

  (def average (liklyhood-weighting hoppl-code 10000))
  (println average)

  (println "==================================================================")
  (println "Homework 5: Problem 2 \n")
  (def hoppl-code '(
      (defn marsaglia-normal [mean var]
        (let [d (uniform-continuous -1.0 1.0)
              x (sample d)
              y (sample d)
              s (+ (* x x ) (* y y ))]
                (if (< s 1)
                  (+ mean (* (sqrt var)
                             (* x (sqrt (* -2 (/ ( log s) s))))))
                  (marsaglia-normal mean var))))
        (let [mu (marsaglia-normal 1 5)
              sigma (sqrt 2)
              lik (normal mu sigma)]
          (observe lik 8)
          (observe lik 9)
          mu) ))

  (def average (liklyhood-weighting hoppl-code 50000))
  (println average)

  (println "==================================================================")
  (println "Homework 5: Problem 3 \n")
  (def hoppl-code '(
     (let [observations [0.9 0.8 0.7 0.0 -0.025 -5.0 -2.0 -0.1 0.0 0.13 0.45 6 0.2 0.3 -1 -1]
      init-dist (discrete [1.0 1.0 1.0])
      trans-dists {0 (discrete [0.1 0.5 0.4])
                   1 (discrete [0.2 0.2 0.6])
                   2 (discrete [0.15 0.15 0.7])}
      obs-dists {0 (normal -1 1)
                 1 (normal 1 1)
                 2 (normal 0 1)}]
      (reduce
        (fn [states obs]
          (let [state (sample (get trans-dists
                                   (peek states)))]
            (observe (get obs-dists state) obs)
            (conj states state)))
        [(sample init-dist)]
        observations)) ))

  (def average (liklyhood-weighting hoppl-code 50000))
  (println average)

  )
