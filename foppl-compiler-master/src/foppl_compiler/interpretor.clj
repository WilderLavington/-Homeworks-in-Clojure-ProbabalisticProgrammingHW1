(ns foppl-compiler.interpretor
  (:require [anglican.runtime :refer [sqrt sin cos log exp observe* sample*
            beta uniform-continuous dirichlet normal gamma discrete flip]]
            [anglican.core :refer :all]
            [clojure.set :as set]
            [foppl-compiler.ast-creation :refer :all]
            [clojure.string :as str]))

(defn supported-expression? [e]
  (some #(= e %) ['= 'vector 'first 'second 'drop 'rest 'conj 'cons 'append
                  'append 'get 'reduce  'peek 'exp 'sqrt 'log '+ '- '* '/
                  '< '> '<= '>=]))

(defn interpretor-eval [e sigma el rho]
    ; (println e)
    ; (println sigma)
    (cond
      ; constant
      (number? e)
        {'evaluation e 'sigma sigma 'el el 'rho rho}

      ; variable - dont evaluate if in user-def
      (symbol? e)
        (if (contains? el e)
          {'evaluation (get el e) 'sigma sigma 'el el 'rho rho}
          {'evaluation (get rho e) 'sigma sigma 'el el 'rho rho})

      ; random variables
      (= (first e) :normal)
        {'evaluation (apply normal [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                       (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
         'sigma sigma 'el el 'rho rho}

      ; random variables
      (= (first e) :beta)
        {'evaluation (apply beta [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                     (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
        'sigma sigma 'el el 'rho rho}

      ; random variables
      (= (first e) :uniform-continuous)
        {'evaluation (apply uniform-continuous [(get (interpretor-eval (nth (second e) 1) sigma
            el rho) 'evaluation) (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
        'sigma sigma 'el el 'rho rho}

      ; random variables
      (= (first e) :discrete)
        {'evaluation (apply discrete [(get (interpretor-eval (nth (second e) 1) sigma
        el rho) 'evaluation)])
        'sigma sigma 'el el 'rho rho}

      ; random variables
      (= (first e) :flip)
        {'evaluation (apply flip [(get (interpretor-eval (nth (second e) 1) sigma
                        el rho) 'evaluation)])
         'sigma sigma 'el el 'rho rho}

      ; definition of user function
      (= (first e) :user-function)
        ; store but dont evaluate until we have values
        {'evaluation (nth e 2) 'sigma sigma 'el  el
         'rho (assoc rho (nth e 2)
                         {:variables (nth e 4)
                          :function  (nth e 6)})}

      ; definition of anonymous function
      (= (first e) :anonymous-function)
          ; store but dont evaluate until we have values
          {'evaluation (nth e 2) 'sigma sigma 'el  el
           'rho (assoc rho (nth e 2)
                           {:variables (nth e 4)
                            :function  (nth e 6)})}

      ; sample
      (= (first e) :sample)
        {'evaluation (sample* (get (interpretor-eval (nth e 1) sigma el rho) 'evaluation))
           'sigma sigma
           'el el
           'rho rho}

      ; observe
      (= (first e) :observe)
        (let [d-sigma (interpretor-eval (nth e 2) sigma el rho)
              c-sigma (interpretor-eval (nth e 4) sigma el rho)]
          {'evaluation (get c-sigma 'evaluation)
           'sigma (+ (get d-sigma 'sigma)
                  (+ (get c-sigma 'sigma)
                  (observe* (get d-sigma 'evaluation)
                            (get c-sigma 'evaluation))))
           'el el
           'rho rho})

      ; if statement
      (= (first e) :if)
        (let [e1_prime (interpretor-eval (nth e 2) sigma el rho)]
          (if (get e1_prime 'evaluation)
            (recur (nth e 4) (get e1_prime 'sigma) el rho)
            (recur (nth e 6) (get e1_prime 'sigma) el rho)))

      ; let statement
      (= (first e) :let)
        (let [v  (nth (second (second e)) 3)
              e1 (interpretor-eval (nth (second (second e)) 5) sigma el rho)
              statement (second (nth e 2))]
              ;(println "\n" e1)
            (recur statement (get e1 'sigma) (assoc el v (get e1 'evaluation)) rho))

      ; general expression
      (= (first e) :apply)
        (let [e0 (second e)
              ci (into [] (for [ei (first (into [] (drop 3 e)))] (interpretor-eval ei sigma el rho)))]
            (let [ci_prime (into [] (for [evalc ci] (get evalc 'evaluation)))
                  el_updated (apply merge (into [] (for [evalc ci] (get evalc 'el))))
                  rho_updated (apply merge (into [] (for [evalc ci] (get evalc 'rho))))]
                (cond
                   (contains? rho_updated (first ci_prime))
                      (apply (resolve (use '[anglican.runtime :refer [sqrt sin cos log exp observe* sample*
                                 beta uniform-continuous dirichlet normal gamma discrete flip ]]) e0)
                         (fn [& args] (interpretor-eval [:user-apply (str (first ci_prime))
                                      :args [(get (first args) 'evaluation) (first (into [] (rest args)))]]
                                      sigma el_updated rho_updated))
                         [{'evaluation (second ci_prime)
                           'sigma sigma 'el el 'rho rho}
                          (first (into [] (rest (drop 1 ci_prime))))])

                    ; otherwise
                    :else
                      (cond
                        (supported-expression? e0)
                          {'evaluation (apply (resolve (use '[anglican.runtime :refer [sqrt sin cos log exp observe* sample*
                                    beta uniform-continuous dirichlet normal gamma discrete flip ]]) e0) ci_prime)
                          'sigma sigma 'el el 'rho rho}
                        :else
                          (println "error in general expression rule: " e sigma el rho)))))

      ; user defined expression
      (= (first e) :user-apply)
        ; get function name and arguments
        (let [e0 (read-string (second e))
              ci (first (into [] (for [ei (drop 3 e)] (get (interpretor-eval ei sigma el rho) 'evaluation))))]
            (cond
              (contains? rho e0)
                ; add variables + values to map then evaluate user function
                (let [function  (get (get rho e0) :function)
                      variables (get (get rho e0) :variables)]
                  (let [temporary-variable-map (merge el (zipmap variables ci))]
                      (recur function sigma temporary-variable-map rho)))
              :else
                (println "error in user expression rule: " e sigma el rho)))

      ; dictionary and key value pairs
      (= (first e) :dictionary)
        {'evaluation (into {} (for [key-value (into [] (rest e))] [(get key-value 1) (get (interpretor-eval (get key-value 3) sigma el rho) 'evaluation)]))
         'sigma sigma 'el el 'rho rho}

      ; program statements
      (= (first e) :program-statement)
        (recur (nth e 1) sigma el rho)

      ; did we hit one of those pesky generic statements
      (= (first e) :statement)
        (recur (first (rest e)) sigma el rho)

      ; just a vector
      :else
        {'evaluation (into [] (for [v e] (get (interpretor-eval v sigma el rho) 'evaluation)))
         'sigma sigma 'el el 'rho rho}))

(defn outer-interpretor-handler [e sigma el rho]
   (cond
     ; are we in the outer program call?
     (= (first e) :program)
       ; update the set of programs being used at each step
       (let [programs (first (drop 1 e))]
         ;(println "outer program loop")
         (loop [program_i 0
                e_i e
                sigma_i sigma
                el_i el
                rho_i rho]
                ;(println "program being digested " (nth programs program_i))
             (cond
               (>= program_i (count programs))
                 {'evaluation e_i 'sigma sigma_i 'el el_i 'rho rho_i}
               (< program_i (count (second e)))
                 (let [program-evaluation (interpretor-eval (nth programs program_i) sigma_i el_i rho_i)]
                   ;(println "")
                   ;(println "This is a program eval - " program-evaluation)
                   (recur
                     (inc program_i)
                     (get program-evaluation 'evaluation)
                     (get program-evaluation 'sigma)
                     (get program-evaluation 'el)
                     (get program-evaluation 'rho)))
               :else
                 (println "you have reached an error, sorry my bruv. "
                 'evaluation e_i 'sigma sigma_i 'el el_i 'rho rho_i))))

     :else
       (println "you have a nasty little error on your hands... ")
     ))

(defn running-avg [average new-sample]
   (let [sample (get new-sample 'evaluation)
         log-liklihood (get new-sample 'sigma)]
      (if (vector? average)
         (into [] (for [idx (take (count average) (range))]
           (+ (get average idx) (* (get sample idx) (exp log-liklihood)))))
   (+ average (* sample (exp log-liklihood))))))

(defn avg [sample_]
  (let [sample (get sample_ 'evaluation)
        log-liklihood (get sample_ 'sigma)]
    (if (vector? (get sample_ 'evaluation))
        (into [] (for [idx (take (count sample) (range))]
            (* (get sample idx) (exp log-liklihood))))
        (* sample (exp log-liklihood)))))

(defn normalize-avg [avg weights]
 (if (vector? avg)
   (into [] (for [idx (take (count avg) (range))]
     (/ (get avg idx) weights)))
   (/ avg weights)))

(defn liklyhood-weighting [foppl-code L]
  (let [ast (program-wrapper foppl-code)]
    (let [initial-sim (select-keys (outer-interpretor-handler ast 0 {} {}) ['evaluation 'sigma])]
      (loop [l 0
            average (avg initial-sim)
            weights (exp (get initial-sim 'sigma))]
        (let [sim (select-keys (outer-interpretor-handler ast 0 {} {}) ['evaluation 'sigma])]
            (if (= 0 (mod l 1000)) (println (float (* (/ l L) 100)) " percent complete, " "time: " (.toString (java.util.Date.))))
            (if (>= l L)
              (normalize-avg average weights)
              (recur
              (inc l)
              (running-avg average sim)
              (+ weights (exp (get sim 'sigma)))
               ))))
            )))
