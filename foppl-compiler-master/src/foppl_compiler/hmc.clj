(ns foppl-compiler.hmc
  (:require [anglican.runtime :refer [sqrt sin cos log exp normal observe* sample* uniform-continuous dirichlet
                                      gamma discrete flip]]
            [anglican.core :refer :all]
            [clojure.set :as set]
            [foppl-compiler.parser :refer :all]
            ;[foppl-compiler.core :refer [get-initial-value]]
            [foppl-compiler.posterior_sampling :refer :all]
            [foppl-compiler.auto-diff :refer :all]
            [clojure.string :as str]))

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
                (if (nil? eval-context)
                (observe* (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
                (my-eval (nth general-ast 3) eval eval-context))
            (my-eval (nth general-ast 2) eval (my-eval (nth general-ast 3) eval eval-context)))

          (= (second general-ast) "sample*")
            (if eval
              (if (nil? eval-context)
              (sample* (my-eval (nth general-ast 2) eval nil))
              (observe* (my-eval (nth general-ast 2) eval nil) eval-context))
              (if (nil? eval-context)
              (println "why r u here")
              (my-eval (nth general-ast 2) eval eval-context)))

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
              (if eval
                  (normal (my-eval (nth general-ast 2) eval eval-context) 1)
                  (seq ['normpdf eval-context (my-eval (nth general-ast 2) eval eval-context)
                                              1]))
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
                                                 (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma)))))))
               [(my-eval (nth general-ast 2) eval eval-context)
                (my-eval (nth general-ast 3) eval eval-context)
                (my-eval (nth general-ast 4) eval eval-context)])

          (= (second general-ast) "vector")
            (vector (for [x (rest (rest general-ast))] (my-eval (nth general-ast x) eval eval-context)))

          (= (second general-ast) "exp")
            (try (exp (my-eval (nth general-ast 2) eval eval-context))
            (catch Exception e (seq ['exp (my-eval (nth general-ast 2) eval eval-context)])))

          (= (second general-ast) "sqrt")
            (try (sqrt (my-eval (nth general-ast 2) eval eval-context))
            (catch Exception e (seq ['sqrt (my-eval (nth general-ast 2) eval eval-context)])))

          (= (second general-ast) "sin")
            (try (sin (my-eval (nth general-ast 2) eval eval-context))
            (catch Exception e (seq ['sin (my-eval (nth general-ast 2) eval eval-context)])))

          (= (second general-ast) "cos")
            (try (cos (my-eval (nth general-ast 2) eval eval-context))
            (catch Exception e (seq ['cos (my-eval (nth general-ast 2) eval eval-context)])))

          (= (second general-ast) "log")
            (try (log (my-eval (nth general-ast 2) eval eval-context))
            (catch Exception e (seq ['log (my-eval (nth general-ast 2) eval eval-context)])))

          ; and the primitives
          (= (second general-ast) "+")
            (try (+ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
            (catch Exception e (seq ['+ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context)])))

          (= (second general-ast) "-")
            (try (- (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
            (catch Exception e (seq ['- (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context)])))

          (= (second general-ast) "*")
            (try (* (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
            (catch Exception e (seq ['* (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context)])))

         (= (second general-ast) "/")
           (try (+ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context))
           (catch Exception e (seq ['+ (my-eval (nth general-ast 2) eval eval-context) (my-eval (nth general-ast 3) eval eval-context)])))

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
        (println "outer my-eval error" general-ast)))

(defn get-pdf-eval-fast [quoted-link-function sample-value]
    (my-eval (create-ast quoted-link-function) true sample-value))

(defn scale-map [scaling my-map]
  (into {} (for [pair my-map] [(first pair) (* scaling (second pair))])))

(defn addition-casing [vector-of-functions]
  (if (= (count vector-of-functions) 1)
      (first vector-of-functions)
      (seq ['+ (first vector-of-functions) (addition-casing (rest vector-of-functions))])))

(defn convert-keys [my-map]
  (into {} (for [map-key (keys my-map)] [(if (string? map-key) (read-string map-key) map-key) (get my-map map-key)])))

(defn get-joint-pdf-gradient-full [G sample]
  (let [relevent-link-fxns (get G :P)
        variables (into [] (get-unobserved-rv G))]
        (scale-map -1 ; might need to switch this back
        (apply merge-with + (into [] (for [link-function relevent-link-fxns]
        (get (autodiff-fast
        ; convert quoted string to function representation
        (seq ['fn variables
        ; add all of the conditioned pdfs togather
        (my-eval (create-ast (second link-function)) false (get sample (first link-function)))])
        ; this is where the gradient is evaluated
        (into [] (for [variable variables] (get sample variable)))) :gradient)))))))

(defn leapfrog-integration [G initial-sample momentum-sample integration-time step-size]
    ; R_1/2 <- R_0 - 1/2 epsilon (grad U(chi_0))
    (let [rhalf_0 (apply merge-with +
                  [ momentum-sample
                   (convert-keys (scale-map -1 (scale-map step-size (scale-map 0.5 (get-joint-pdf-gradient-full G initial-sample)))))])
          sample_0 initial-sample]
    ; inner for loop
    (let [sample-r
      (loop [sample_t sample_0
             momentum_t rhalf_0
             t 0]
          (if (> t (- integration-time 1))
              [sample_t momentum_t]
              (recur
                ; chi_t <- chi_t-1 + epsilon R_t-1/2
                (apply merge-with +
                  [(into {} (for [variable (get-unobserved-rv G)] [variable (get sample_t variable)]))
                   (convert-keys (scale-map step-size momentum_t))])
                ; R_t+1/2 <- R_t-1/2 - epsilon (grad U(chi_t-1))
                (apply merge-with - [momentum_t (scale-map step-size
                              (convert-keys (get-joint-pdf-gradient-full G sample_t)))])
                (inc t)
                )))]
        ; chi_T <- chi_T-1 + epsilon R_T-1/2
        [(apply merge-with + [(first sample-r) (scale-map step-size (second sample-r))])
        ; R_T <- R_0 - 0.5 epsilon (grad U(chi_T-1))
         (apply merge-with - [ momentum-sample
                              (convert-keys (scale-map step-size (scale-map 0.5 (get-joint-pdf-gradient-full G (first sample-r)))))])])))

(defn draw-momentum-vals-simple [m, variables]
  (into {} (for [index (into [] (take (count variables) (range)))] [(nth variables index) (sample* (normal 0 m)) ])))

(defn evaluate-joint-full [G sample] ; returns the log of the joint
    (let [link-functions (get G :P)
          observed-variables (get-observed-rv G)
          unobserved-variables (get-unobserved-rv G)]
          (+
          (reduce + (for [prior-link-function (select-keys link-functions unobserved-variables)]
                (get-pdf-eval-fast (clojure.walk/postwalk-replace sample (val prior-link-function)) (get sample (key prior-link-function)))))
          (reduce + (for [liklyhood-link-function (select-keys link-functions observed-variables)]
                (get-pdf-eval-fast (clojure.walk/postwalk-replace sample (val liklyhood-link-function)) nil))))))


(defn hamiltonian [G current-sample momentum-sample mass]
    ; U(s) <- -log(joint)
    ; K(s) <- 0.5p^2/m
    ; H(s) <- U(s) + K(s)
    (* 1 (- (* (/ 0.5 mass) (reduce + (vals (apply merge-with * [momentum-sample momentum-sample])))) (evaluate-joint-full G current-sample))))

(defn HMC-step [G initial-sample integration-time step-size variable-mass]
    (let [momentum-sample (draw-momentum-vals-simple variable-mass (get-unobserved-rv G))]
      (let [next-sample-W-momentum (leapfrog-integration G initial-sample momentum-sample integration-time step-size)]
        (if (< (sample* (uniform-continuous 0 1))
               (exp (- (hamiltonian G initial-sample momentum-sample variable-mass)
                       (hamiltonian G (first next-sample-W-momentum) (second next-sample-W-momentum) variable-mass))))
                       (if (or (= (* -1 (log 0)) (evaluate-joint-full G (first next-sample-W-momentum)))
                               (= (* 1 (log 0)) (evaluate-joint-full G (first next-sample-W-momentum)))
                               (not (nil? (some #(or (= (* -1 (log 0)) %) (= (* 1 (log 0)) %))
                               (vals (get-joint-pdf-gradient-full G (first next-sample-W-momentum))))))
                               )
                            (get-initial-value G) ; we are just going to get start with a new value
                            (first next-sample-W-momentum))
                        initial-sample))))

(defn HMC-generator
  ([G initial-sample iterations integration-time step-size variable-mass E]
    (if (= 0 (mod iterations 500)) (println "samples generated: " iterations ", time: " (.toString (java.util.Date.))
    ", current sample: \n" initial-sample
    ))
    (lazy-seq (cons (get initial-sample 'sampleprevoutput) (HMC-generator G
      (apply dissoc
      (assoc
        (HMC-step G initial-sample integration-time step-size variable-mass)
        'sampleprevoutput (clojure.walk/postwalk-replace initial-sample E))
        (get-observed-rv G))
      (inc iterations) integration-time step-size variable-mass E)))))
