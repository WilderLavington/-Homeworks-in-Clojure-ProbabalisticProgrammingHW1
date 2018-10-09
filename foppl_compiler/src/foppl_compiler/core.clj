(ns foppl_compiler.core
  (:require  ;[gorilla-plot.core :as plot]
               [clojure.repl :as repl]
               [clojure.string :as str]
               [instaparse.core :as insta]
               [clojure.core.matrix :as m]
               [anglican.runtime :refer [tanh observe* sample* normal exp cos sin log pow]]))

(load-file "/Users/wilder/Desktop/foppl_compiler/src/foppl_compiler/parser.clj")
(load-file "/Users/wilder/Desktop/foppl_compiler/src/foppl_compiler/helper-fxns.clj")
(load-file "/Users/wilder/Desktop/foppl_compiler/src/foppl_compiler/ast-creation.clj")
(load-file "/Users/wilder/Desktop/foppl_compiler/src/foppl_compiler/compiler.clj")
(load-file "/Users/wilder/Desktop/foppl_compiler/src/foppl_compiler/auto-diff.clj")


; required functions
(defn append [& args] (apply conj args))
(defn mat-mul [& args] (apply m/mmul args))
(defn mat-add [& args] (apply m/add args))
(defn mat-transpose [& args] (apply m/transpose args))
(defn mat-tanh [M] (m/emap tanh M))
(defn mat-relu [M] (m/emap (fn [x] (if (> x 0) x 0)) M))
(defn mat-repmat [M r c]
 (let [R (reduce (partial m/join-along 0) (repeat r M))]
   (reduce (partial m/join-along 1) (repeat c R))))

; need to fix sample andobserve
(defn sample [x] (sample* x))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println "==================================================================")
  (println "==========================HOMEWORK 1==============================")
  (println "==================================================================")
  (println "Homework 1: Problem 1 \n")
  (def foppl-code '( (let [mu (sample (normal 1 (sqrt 5)))
                           sigma (sqrt 2)
                           lik (normal mu sigma)]
                       (observe lik 8)
                       (observe lik 9)
                       mu)
                        ))
  (def program (program-wrapper foppl-code))
  ;(println program)
  (def compiled-program (foppl-compiler foppl-code))
  (println compiled-program)
  (println "==================================================================")
  (println "Homework 1: Problem 2 \n")
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
                       (loop 5 data observe-data slope bias)
                       (vector slope bias)) ))
  (def program (program-wrapper foppl-code))
  ; (println program)
  (def compiled-program (foppl-compiler foppl-code))
  (println compiled-program)

  (println "==================================================================")
  (println "Homework 1: Problem 3 \n")
  (def foppl-code '( (defn hmm-step [t states data trans-dists likes]
                          (let [z (sample (get trans-dists
                                               (last states)))]
                            (observe (get likes z)
                                     (get data t))
                            (append states z)))
                      (let [data [0.9 0.8 0.7 0.0 -0.025 -5.0 -2.0 -0.1
                                0.0 0.13 0.45 6 0.2 0.3 -1 -1]
                          trans-dists [(discrete [0.10 0.50 0.40])
                                       (discrete [0.20 0.20 0.60])
                                       (discrete [0.15 0.15 0.70])]
                          likes [(normal -1.0 1.0)
                                 (normal 1.0 1.0)
                                 (normal 0.0 1.0)]
                          states [(sample (discrete [0.33 0.33 0.34]))]]
                      (loop 16 states hmm-step data trans-dists likes)) ))
  (def program (program-wrapper foppl-code))
  (println program)
  (def compiled-program (foppl-compiler foppl-code))
  (println compiled-program)

  (println "==================================================================")
  (println "Homework 1: Problem 4 \n")
  (def foppl-code '( (let [weight-prior (normal 0 1)
                            W_0 (foreach 10 []
                                  (foreach 1 [] (sample weight-prior)))
                            W_1 (foreach 10 []
                                  (foreach 10 [] (sample weight-prior)))
                            W_2 (foreach 1 []
                                  (foreach 10 [] (sample weight-prior)))
                            b_0 (foreach 10 []
                                  (foreach 1 [] (sample weight-prior)))
                            b_1 (foreach 10 []
                                  (foreach 1 [] (sample weight-prior)))
                            b_2 (foreach 1 []
                                  (foreach 1 [] (sample weight-prior)))
                            x   (mat-transpose [[1] [2] [3] [4] [5]])
                            y   [[1] [4] [9] [16] [25]]
                            h_0 (mat-tanh (mat-add (mat-mul W_0 x)
                                                   (mat-repmat b_0 1 5)))
                            h_1 (mat-tanh (mat-add (mat-mul W_1 h_0)
                                                   (mat-repmat b_1 1 5)))
                            mu  (mat-transpose
                                  (mat-tanh (mat-add (mat-mul W_2 h_1)
                                                     (mat-repmat b_2 1 5))))]
                          (foreach 5 [y_r y
                                      mu_r mu]
                             (foreach 1 [y_rc y_r
                                         mu_rc mu_r]
                                (observe (normal mu_rc 1) y_rc)))
                          [W_0 b_0 W_1 b_1]) ) )

  (def program (program-wrapper foppl-code))
  (println program)

  (println "==================================================================")
  (println "==========================HOMEWORK 2==============================")
  (println "==================================================================")
  (println "Homework 2: example: \n")
  (def foppl-code '(fn [a b c] (/ (+ a (* 7 b)) (sin c))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [1.0 2.0 3.0]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [a b c] (/ (+ a (* 7 b)) (sin c)))
                              [1.0 2.0 3.0] 0.0000001) )
  (println "==================================================================")



  (println "Homework 2: Problem 1: \n")
  (def foppl-code '(fn [x] (exp (sin x))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [1.0]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x] (exp (sin x)))
                              [1.0] 0.0000001) )
  (println "==================================================================")
  (println "Homework 2: Problem 2: \n")
  (def foppl-code '(fn [x y] (+ (* x x) (sin x))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [1.0 10.0]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x y] (+ (* x x) (sin x)))
                              [1.0 10.0] 0.0000001) )
  (println "==================================================================")
  (println "Homework 2: Problem 3: \n")
  (def foppl-code '(fn [x] (if (> x 5) (* x x) (+ x 18))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [3.0]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x] (if (> x 5) (* x x) (+ x 18)))
                              [3.0] 0.0000001) )
  (println "==================================================================")
  (println "Homework 2: Problem 4: \n")
  (def foppl-code '(fn [x] (log x)) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [10.0]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x] (log x))
                              [10.0] 0.0000001) )
  (println "==================================================================")
  (println "Homework 2: Problem 5: \n")
  (def foppl-code '(fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                                                  (* 2 (* sigma sigma))))
                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [0.5 0.0 1.2]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                                                  (* 2 (* sigma sigma))))
                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma)))))))
                              [0.5 0.0 1.2] 0.00001) )
  (println "==================================================================")
  (println "Homework 2: Problem 6: \n")
  (def foppl-code '(fn [x mu sigma] (normpdf x mu sigma)) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [1.0 0.1 2.1]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x mu sigma] (exp (+ (- 0 (/ (* (- x mu) (- x mu))
                                                  (* 2 (* sigma sigma))))
                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))))
                              [1.0 0.1 2.1] 0.00001) )
  (println "==================================================================")
  (println "Homework 2: Problem 7: \n")
  (def foppl-code '(fn [x1 x2 x3] (+ (+ (normpdf x1 2 5)
                        (if (> x2 7)
                          (normpdf x2 0 1)
                          (normpdf x2 10 1)))
                    (normpdf x3 -4 10))) )
  (println "foppl code: " foppl-code)
  (println (autodiff foppl-code [1.4 0.5 10.1]))
  (println "numerical check that we are good: ")
  (println (numerical-approx (fn [x1 x2 x3] (+ (+ (normpdf x1 2 5)
                                (if (> x2 7)
                                  (normpdf x2 0 1)
                                  (normpdf x2 10 1)))
                            (normpdf x3 -4 10)))
                              [1.4 0.5 10.1] 0.0000001) )
  (println "==================================================================")
  )
