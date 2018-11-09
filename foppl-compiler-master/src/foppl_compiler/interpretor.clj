(ns foppl-compiler.interpretor
  (:require [anglican.runtime :refer [sqrt sin cos log exp observe* sample*
            beta uniform-continuous dirichlet normal gamma discrete flip]]
            [anglican.core :refer :all]
            [clojure.set :as set]
            [foppl-compiler.ast-creation :refer :all]
            [clojure.string :as str]))

(defn supported-expression? [e]
  (some #(= e %) ['= 'vector 'first 'second 'drop 'rest 'conj 'cons 'append
                  'append 'get 'reduce  'peek 'exp 'sqrt 'log '+ '- '* '/]))

(defn interpretor-eval [e sigma el rho]
  (cond
    (vector? e)
      (cond
        ; did we hit one of those pesky generic statements
        (= (first e) :statement)
          (interpretor-eval (first (rest e)) sigma el rho)
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

        ;if we are in an individual program statement run it!
        (= (first e) :program-statement)
          (interpretor-eval (nth e 1) sigma el rho)

        ; sample
        (= (first e) :sample)
          (let [d-sigma (interpretor-eval (nth e 1) sigma el rho)
                the-sample (sample* (get d-sigma 'evaluation))]
            {'evaluation the-sample
             'sigma (+ sigma (get d-sigma 'sigma))
             'el el
             'rho rho})

        ; observe
        (= (first e) :observe)
          (let [d-sigma (interpretor-eval (nth e 2) sigma el rho)
                c-sigma (interpretor-eval (nth e 4) sigma el rho)]
            {'evaluation (get c-sigma 'evaluation)
             'sigma (+ (get c-sigma 'sigma) (observe* (get d-sigma 'evaluation) (get c-sigma 'evaluation)))
             'el el
             'rho rho})

        ; if statement
        (= (first e) :if)
          (let [e1_prime (interpretor-eval (nth e 2) sigma el rho)]
            (if (get e1_prime 'evaluation)
              (interpretor-eval (nth e 4) sigma el rho)
              (interpretor-eval (nth e 6) sigma el rho)))

        ; let statement
        (= (first e) :let)
          (let [v  (nth (second (second e)) 3)
                e1 (interpretor-eval (nth (second (second e)) 5) sigma el rho)
                statement (second (nth e 2))]
              (interpretor-eval statement sigma (assoc el v (get e1 'evaluation)) rho))

        ; general expression
        (= (first e) :apply)
          (let [e0 (second e)
                ci (into [] (for [ei (first (into [] (drop 3 e)))] (get (interpretor-eval ei sigma el rho) 'evaluation)))]
              (cond
                (supported-expression? e0)
                  {'evaluation (apply (resolve e0) ci)
                  'sigma sigma 'el el 'rho rho}
                :else
                  (println "error in general expression rule: " e sigma el)))

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
                        (interpretor-eval function sigma temporary-variable-map rho)))
                :else
                  (println "error in user expression rule: " e sigma el)))

        ; random variables
        (= (first e) :normal)
          {'evaluation (apply normal [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                         (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
           'sigma sigma 'el el 'rho rho}
        (= (first e) :beta)
          {'evaluation (apply beta [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                       (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
          'sigma sigma 'el el 'rho rho}
        (= (first e) :uniform-continuous)
          {'evaluation (apply uniform-continuous [(get (interpretor-eval (nth (second e) 1) sigma
              el rho) 'evaluation) (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
          'sigma sigma 'el el 'rho rho}
        (= (first e) :discrete)
          {'evaluation (apply discrete [(get (interpretor-eval (nth (second e) 1) sigma
          el rho) 'evaluation)])
          'sigma sigma 'el el 'rho rho}
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
        ; just a vector
        :else
        {'evaluation (into [] (for [v e] (get (interpretor-eval v sigma el rho) 'evaluation)))
         'sigma sigma 'el el 'rho rho})

      ; constant
      (number? e)
        {'evaluation e 'sigma sigma 'el el 'rho rho}

      ; variable - dont evaluate if in user-def
      (symbol? e)
        {'evaluation (get el e) 'sigma sigma 'el el 'rho rho}

      ; error
      :else
        (println "error in interpretor-eval - " "e: " e ", sigma: " sigma ", el: " el ", rho: " rho)))


(defn liklyhood-weighting [foppl-code L]
  ;(println "AST: here it is - \n" (program-wrapper foppl-code))
  (println "Samples: here they are - \n")
  (let [ast (program-wrapper foppl-code)]
    ; apply eval
    (loop [l 0
           samples []]
        (if (>= l L)
          [samples]
          (recur (inc l)
                 (conj samples (interpretor-eval ast 0 {} {})))))))
