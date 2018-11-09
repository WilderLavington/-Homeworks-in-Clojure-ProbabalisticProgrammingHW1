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
        ; are we in the outer program call?
        (= (first e) :program)
          ; update the set of programs being used at each step
          (let [programs (drop 1 e)]
            (loop [program_i 0
                   e_i e
                   sigma_i sigma
                   el_i el
                   rho_i rho]
                (cond
                  (>= program_i (count programs))
                    {'evaluation e_i 'sigma sigma_i 'el el_i 'rho rho_i}
                  (< program_i (count (second e)))
                    (let [program-evaluation (interpretor-eval (nth programs program_i) sigma_i el_i rho_i)]
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
          (let [d-sigma (interpretor-eval (nth e 1) sigma el rho)]
            {'evaluation (sample* (get d-sigma 'evaluation))
             'sigma (get d-sigma 'sigma)
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
          (let [e1_prime (interpretor-eval (nth e 1) sigma el rho)]
            (if (get e1_prime 'evaluation)
              (interpretor-eval (nth e 2) sigma el rho)
              (interpretor-eval (nth e 3) sigma el rho)))

        ; let statement
        (= (first e) :let)
          (let [v (interpretor-eval (nth e 1) sigma el rho)
                e1 (interpretor-eval (nth e 2) sigma el rho)]
              (interpretor-eval (nth e 3) sigma
                (assoc el (get v 'evaluation) (get e1 'evaluation))))

        ; general expression
        (= (first e) :apply)
          (let [e0 (second e)
                ci (for [ei (drop 2 e)] (get (interpretor-eval ei sigma el rho) 'evaluation))]
              (cond
                (supported-expression? (get e0 'evaluation))
                  (apply (get e0 'evaluation) ci)
                :else
                  (println "error in general expression rule: " e sigma el)))

        ; user defined expression
        (= (first e) :user-apply)
          ; get function name and arguments
          (let [e0 (second e)
                ci (for [ei (drop 2 e)] (get (interpretor-eval ei sigma el rho) 'evaluation))]
              (cond
                (contains? rho (get e0 'evaluation))
                  ; add variables + values to map then evaluate user function
                  (let [function  (get (get rho (get e0 'evaluation)) :function)
                        variables (get (get rho (get e0 'evaluation)) :variables)]
                    (let [temporary-variable-map (merge el (zipmap variables ci))]
                        (interpretor-eval sigma temporary-variable-map rho)))
                :else
                  (println "error in user expression rule: " e sigma el)))

        ; random variables
        (= (first e) :normal)
          (apply normal [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                         (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
        (= (first e) :beta)
          (apply beta [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                       (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
        (= (first e) :uniform-continuous)
          (apply uniform-continuous [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)
                                     (get (interpretor-eval (nth (second e) 3) sigma el rho) 'evaluation)])
        (= (first e) :discrete)
          (apply discrete [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)])
        (= (first e) :flip)
          (apply flip [(get (interpretor-eval (nth (second e) 1) sigma el rho) 'evaluation)])

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
          (into [] (for [v e] (get (interpretor-eval v sigma el rho) 'evaluation))))

      ; constant
      (number? e)
        {'evaluation e 'sigma sigma 'el el 'rho rho}

      ; variable - dont evaluate if in user-def
      (symbol? e)
        {'evaluation (get el e) 'sigma sigma 'el el 'rho rho}

      ; error
      :else
        (println "error in interpretor-eval" e sigma el rho)))


(defn liklyhood-weighting [foppl-code L]
  (let [ast (program-wrapper foppl-code)]
    ; apply eval
    (loop [l 0
           samples []]
        (if (>= l L)
          [samples]
          (recur (inc l)
                 (conj samples (interpretor-eval ast 0 {} {})))))))
