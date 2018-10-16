(ns foppl-compiler.core
  "This namespace contains an implementation of a FOPPL compiler following chapter
  3.1 of 'An Introduction to Probabilistic Programming' by van de Meent et al."
  (:require [clojure.set :as set]
            [anglican.runtime :refer :all]
            [anglican.runtime :refer [sample* observe* normal]]
            [clojure.string :as str]
            [foppl-compiler.desugar :refer [desugar]]
            [foppl-compiler.parser :refer :all]
            [foppl-compiler.posterior_sampling :refer :all]
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



(defn invert-graph [G]
  (reduce (fn [acc m] (merge-with set/union acc m))
          {}
          (for [[p children] G
                c children]
            {c #{p}})))


(defn topo-sort [{:keys [V A P]}]
  (let [terminals
        (loop [terminals []
               A A
               V V]
          (let [ts (filter (comp empty? (invert-graph A)) V)
                V (set/difference V (set ts))]
            (if (empty? V)
              (into terminals ts)
              (recur (into terminals ts)
                     (select-keys A V)
                     V))))]
    terminals))


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

(defn get-initial-value [G]
   (let [ordered-sample-variables (filter #(re-find #"sample" (str %)) (topo-sort G))]
        (loop [link-fxns (get G :P)
              idx 0]
          (if (>= idx (count ordered-sample-variables))
              link-fxns
              (recur (let [new-key {(nth ordered-sample-variables idx)
                                    (if (seq? (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))
                                              (into [] (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))
                                              (eval (clojure-dependencies-suck (get link-fxns (nth ordered-sample-variables idx)))))}
                           remaining-link-fxns (dissoc link-fxns (nth ordered-sample-variables idx))]
                           (merge (clojure.walk/postwalk-replace new-key remaining-link-fxns) new-key))
                    (inc idx))))))


(load-file "/Users/wilder/Desktop/foppl-compiler-master/src/foppl_compiler/posterior_sampling.clj")
(load-file "/Users/wilder/Desktop/foppl-compiler-master/src/foppl_compiler/parser.clj")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "==================================================================")
  (println "==========================HOMEWORK 3==============================")
  (println "==================================================================")
  (println "Homework 3: Problem 1 \n")
  (def foppl-code '( (let [mu (sample (normal 1 (sqrt 5)))
                           sigma (sqrt 2)
                           lik (normal mu sigma)]
                       (observe lik 8)
                       (observe lik 9)
                       mu) ))
  (def program (get-graph foppl-code))
  (def G (second program))
  (def initial-state (get-initial-value G))
  (time (def samples (Gibbs-sampler initial-state G 20000 5000)))
  (println initial-state)
  (println
    (for [random-variables (filter #(re-find #"sample" (str %)) (keys (first samples)))]
      {random-variables
        {:sample-mean
           (/ (reduce + (for [realizations samples]
          (get realizations random-variables))) (count samples)) }}))



  (println "==================================================================")
  (println "Homework 3: Problem 2 \n")
  (def foppl-code '( (defn observe-data [_ data slope bias]
                      (let [xn (first data)
                            yn (second data)
                            zn (+ (* slope xn) bias)]
                        (observe (normal zn 1.0) yn)
                        (rest (rest data))))

                    (let [slope (sample (normal 0.0 10.0))
                          bias  (sample (normal 0.0 10.0))
                          data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                                       4.0 7.7 5.0 10.2 6.0 12.9)]
                      (loop 6 data observe-data slope bias)
                      (vector slope bias)) ))
  (def program (get-graph foppl-code))
  (def G (second program))
  (def initial-state (get-initial-value G))
  (time (def samples (Gibbs-sampler initial-state G 10000 5000)))
  (println initial-state)
  (println
    (for [random-variables (filter #(re-find #"sample" (str %)) (keys (first samples)))]
      {random-variables
        {:sample-mean
           (/ (reduce + (for [realizations samples]
          (get realizations random-variables))) (count samples)) }}))

  (println "==================================================================")
  (println "Homework 3: Problem 3 \n")
  (def foppl-code '( (let [data [1.1 2.1 2.0 1.9 0.0 -0.1 -0.05]
                          likes (foreach 3 []
                                         (let [mu (sample (normal 0.0 10.0))
                                               sigma (sample (gamma 1.0 1.0))]
                                           (normal mu sigma)))
                          pi (sample (dirichlet [1.0 1.0 1.0]))
                          z-prior (discrete pi)
                          z (foreach 7 [y data]
                              (let [z (sample z-prior)]
                                (observe (get likes z) y)))]
                      (= (first z) (second z))) ))
  (def program (get-graph foppl-code))
  (def G (second program))
  (def initial-state (get-initial-value G))
  (time (def samples (Gibbs-sampler initial-state G 10000 5000)))
  (println initial-state)
  (println
    (for [random-variables (filter #(re-find #"sample" (str %)) (keys (first samples)))]
      (if (vector? (get (first samples) random-variables) )
      {random-variables
        {:sample-mean
           (matrix-average (into [] (for [realizations samples]
             (get realizations random-variables))) (count samples)) }}
      {random-variables
        {:sample-mean
           (/ (reduce + (for [realizations samples]
             (get realizations random-variables))) (count samples)) }})))

  (println "==================================================================")
  (println "Homework 3: Problem 4 \n")
  (def foppl-code '( (let [sprinkler true
                          wet-grass true
                          is-cloudy (sample (flip 0.5))

                          is-raining (if (= is-cloudy true )
                                        (sample (flip 0.8))
                                        (sample (flip 0.2)))
                          sprinkler-dist (if (= is-cloudy true)
                                            (flip 0.1)
                                            (flip 0.5))
                          wet-grass-dist (if (and (= sprinkler true)
                                                  (= is-raining true))
                                            (flip 0.99)
                                            (if (and (= sprinkler false)
                                                     (= is-raining false))
                                              (flip 0.0)
                                              (if (or (= sprinkler true)
                                                      (= is-raining true))
                                                (flip 0.9)
                                                1)))]
                      (observe sprinkler-dist sprinkler)
                      (observe wet-grass-dist wet-grass)
                      is-raining) ))
  (def program (get-graph foppl-code))
  (def G (second program))
  (def initial-state (get-initial-value G))
  (time (def samples (Gibbs-sampler initial-state G 10000 5000)))
  (println initial-state)
  (println
    (for [random-variables (filter #(re-find #"sample" (str %)) (keys (first samples)))]
      (if (boolean? (get (first samples) random-variables) )
      {random-variables
        {:sample-mean
           (tf-average (into [] (for [realizations samples]
             (get realizations random-variables)))) }}
      {random-variables
        {:sample-mean
           (/ (reduce + (for [realizations samples]
             (get realizations random-variables))) (count samples)) }})))

  (println "==================================================================")
  (println "Homework 3: Problem 5 \n")
  (def foppl-code '( (let [x (sample (normal 0 10))
                            y (sample (normal 0 10))]
                        (observe (dirac (+ x y)) 7)
                        [x y]) ))

  (def program (get-graph foppl-code))
  (def G (second program))
  (def initial-state (get-initial-value G))
  (time (def samples (Gibbs-sampler initial-state G 10000 5000)))
  (println initial-state)
  (println (for [random-variables (filter #(re-find #"sample" (str %)) (keys (first samples)))]
    {random-variables
      {:sample-mean
         (/ (reduce + (for [realizations samples]
           (get realizations random-variables))) (count samples))
       :sample-variance
       (/ (reduce +
             (for [x samples]
               (* (- (get x random-variables)
                   (/ (reduce +
                     (for [realizations samples]
                         (get realizations random-variables)))
                   (count samples)))
                 (- (get x random-variables)
                   (/ (reduce +
                     (for [realizations samples]
                         (get realizations random-variables)))
                     (count samples))))))
       (- (count samples) 1))
          }}))
  )
