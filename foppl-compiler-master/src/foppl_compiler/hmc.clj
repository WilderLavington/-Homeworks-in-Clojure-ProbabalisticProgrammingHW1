(ns foppl-compiler.hmc
  (:require [anglican.runtime :refer [sqrt sin cos log exp normal observe* sample* uniform-continuous dirichlet
                                      gamma discrete flip]]
            [anglican.core :refer :all]
            [foppl-compiler.parser :refer :all]
            [foppl-compiler.posterior_sampling :refer :all]
            [foppl-compiler.auto-diff :refer :all]
            [clojure.string :as str]))

(defn my-eval [general-ast eval eval-context]
    ; recursively set up our function evaluator tree
    (cond
      (vector? general-ast)
        (cond

          ; handles values that were in brackets (dirichlet and discrete)
          (= (first general-ast) :definition)
            (into [] (for [x (rest general-ast)] (my-eval x eval eval-context)))

          ; sample / observe cases
          (= (second general-ast) "observe*") ;returns quoted if we dont want to eval
            (if eval
                (observe* (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
                (my-eval (nth general-ast 2) eval (my-eval (nth general-ast 3) eval eval-context)))

          (= (second general-ast) "sample*")
            (sample* (my-eval (nth general-ast 2) eval eval-context))

          ; distribution cases -> only need pdf conversion for normal
          (= (second general-ast) "normal")
              (if eval
                  (normal (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
                  (seq ['normpdf eval-context (my-eval (nth general-ast 2) eval eval-context)
                                              (my-eval (nth general-ast 3) eval eval-context)]))

          (= (second general-ast) "dirichlet")
              (dirichlet (my-eval (nth general-ast 2) eval eval-context))
          (= (second general-ast) "gamma")
              (gamma (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
          (= (second general-ast) "discrete")
              (discrete (my-eval (nth general-ast 2) eval eval-context))
          (= (second general-ast) "dirac")
              (normal (my-eval (nth general-ast 2) eval eval-context) 0.000000005)
          (= (second general-ast) "flip")
              (flip (my-eval (nth general-ast 2) eval eval-context))

          (= (second general-ast) "if")
              ; if wont return any intermediary info, we will just eval + collapse
              (if (my-eval (nth general-ast 2) eval eval-context)
                  (my-eval (nth general-ast 3) eval eval-context)
                  (my-eval (nth general-ast 4) eval eval-context))

          ; just in case we run into an if statement
          (= (second general-ast) ">")
            (> (my-eval (nth general-ast 2) eval eval-context)
               (my-eval (nth general-ast 3) eval eval-context))
          (= (second general-ast) "<")
            (< (my-eval (nth general-ast 2) eval eval-context)
               (my-eval (nth general-ast 3) eval eval-context))
          (= (second general-ast) ">=")
            (>= (my-eval (nth general-ast 2) eval eval-context)
                (my-eval (nth general-ast 3) eval eval-context))
          (= (second general-ast) "<=")
            (<= (my-eval (nth general-ast 2) eval eval-context)
                (my-eval (nth general-ast 3) eval eval-context))

          ; now for the functions
          (= (second general-ast) "normpdf")
            (apply (fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                                                            (* 2 (* sigma sigma))))
                                                 (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma)))))))
               [(my-eval (nth general-ast 2) eval eval-context)
                (my-eval (nth general-ast 3) eval eval-context)
                (my-eval (nth general-ast 4) eval eval-context)])


          (= (second general-ast) "exp")
            (exp (my-eval (nth general-ast 2) eval eval-context))

          (= (second general-ast) "sqrt")
            (sqrt (my-eval (nth general-ast 2) eval eval-context))

          (= (second general-ast) "sin")
            (sin (my-eval (nth general-ast 2) eval eval-context))

          (= (second general-ast) "cos")
            (cos (my-eval (nth general-ast 2) eval eval-context))

          (= (second general-ast) "log")
            (log (my-eval (nth general-ast 2) eval eval-context))

          ; and the primitives
          (= (second general-ast) "+")
            (+ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))

          (= (second general-ast) "-")
            (- (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))

          (= (second general-ast) "*")
            (* (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))

         (= (second general-ast) "/")
            (/ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))

          :else
            (println "inner my-eval error" general-ast)
          )

      ; and the base cases
      (or (integer? general-ast) (float? general-ast))
        general-ast
      (string? general-ast)
        (read-string general-ast)
      (symbol? general-ast)
        general-ast

      :else
        (println "outer my-eval error" general-ast)
      )
    )

(defn function-casing-helper [quoted-expression variable sample-map]
  (clojure.walk/postwalk-replace (dissoc sample-map variable) (my-eval (create-ast quoted-expression) false nil))
  )

(defn get-pdf [quoted-sample value sample-map] ;returns log pdf eval with variables replaced
  (if (str/includes? (str quoted-sample) "observe")
      (clojure.walk/postwalk-replace sample-map quoted-sample)
      (clojure.walk/postwalk-replace sample-map (read-string (str (str/join "" (drop-last (str
            (clojure.walk/postwalk-replace {'sample* 'observe*} quoted-sample)))) " " value ")" )))))

(defn scale-map [scaling my-map]
(into {} (for [pair my-map] [(first pair) (* scaling (second pair))]))
  )

(defn get-pdf-gradient-mapping [G variable]
  ; get observe statements
  1
  ;

  ;
  )

(defn addition-casing [vector-of-functions]
  (if (= (count vector-of-functions) 1)
      (first vector-of-functions)
      (seq ['+ (first vector-of-functions) (addition-casing (rest vector-of-functions))]))
  )

(defn get-joint-pdf-gradient [G variable sample]
  (let [relevent-link-fxns (into [] (remove nil? (for [link-functions (get G :P)]
                              (if (str/includes? (str (val link-functions)) (str variable)) link-functions nil))))]
        ;apply auto-diff to get partial wrt our variable
        (get (autodiff
        ; convert quoted string to function representation
        (seq ['fn [variable]
        ; add all of the conditioned pdfs togather
        (addition-casing (conj
        ; markov blanket
        (into [] (for [link-function relevent-link-fxns]
            (function-casing-helper (get-pdf (second link-function) (get sample (first link-function)) (dissoc sample variable)) variable sample)))
        ; prior
        (function-casing-helper (get-pdf (get (get G :P) variable) (get sample variable) sample) variable sample)))])
        ; this is where the gradient is evaluated
        [(get sample variable)]) :gradient)))

(defn leapfrop-integration [G initial-sample momentum-sample integration-time step-size]
    (let [rhalf ; apply arithmatic to add in momentum
                (apply merge-with + [momentum-sample  (scale-map -1 (scale-map step-size (scale-map 0.5
                ; hash-map with partial derivatives
                (into {} (for [variable (get-unobserved-rv G)]
                              [(read-string (first (keys (get-joint-pdf-gradient G variable initial-sample))))
                               (first (vals (get-joint-pdf-gradient G variable initial-sample)))])))))])]
    (loop [sample_t initial-sample
          momentum_t rhalf
          t 0]
          (if (> t integration-time)
              [sample_t momentum_t]
              (recur
                (apply merge-with + [(into {} (for [variable (get-unobserved-rv G)] [variable (get sample_t variable)]))
                              (scale-map step-size momentum_t)])
                ; apply arithmatic to add in momentum
                (apply merge-with + [momentum_t  (scale-map -1 (scale-map step-size (scale-map 1.0
                ; hash-map with partial derivatives
                (into {} (for [variable (get-unobserved-rv G)]
                              [(read-string (first (keys (get-joint-pdf-gradient G variable sample_t))))
                               (first (vals (get-joint-pdf-gradient G variable sample_t)))])))))])
                (inc t))))))

(defn draw-momentum-vals [M, variables]
  (into {} (for [index (into [] (take (count variables) (range)))] [(nth variables index) (sample* (normal 0 (nth M index))) ])))

(defn hamiltonian [G current-sample momentum-sample mass]
    (apply merge-with + [(into {} (for [variable (keys current-sample)] [variable (- 0 (evaluate-joint G current-sample variable)) ]))
                        (apply merge-with * [(apply merge-with / [momentum-sample mass]) momentum-sample])]))

(defn HMC-step [G initial-sample iterations integration-time step-size variable-mass]
    (let [momentum-sample (draw-momentum-vals variable-mass (get-unobserved-rv G))]
      (let [next-sample-W-momentum (leapfrop-integration G initial-sample momentum-sample integration-time step-size)]
        (if (< (sample* (uniform-continuous 0 1))
               (exp (- (hamiltonian G initial-sample momentum-sample
                          (into {} (for [key (keys momentum-sample)] [key (first variable-mass)])))
                       (hamiltonian G (first next-sample-W-momentum ) (second next-sample-W-momentum)
                          (into {} (for [key (keys momentum-sample)] [key (first variable-mass)]))))))
                       (first next-sample-W-momentum)
                        initial-sample)
          (first next-sample-W-momentum )
          initial-sample)))

(defn HMC-generator
  ([G initial-sample iterations integration-time step-size variable-momentums E]
    (lazy-seq (cons initial-sample (HMC-generator
      (assoc (HMC-step G initial-sample iterations integration-time step-size variable-momentums) 'sampleprevoutput
      (eval (clojure-dependencies-suck (clojure.walk/postwalk-replace initial-sample E)))) G E)))))
