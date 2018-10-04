(ns foppl_compiler.core
  (:require  ;[gorilla-plot.core :as plot]
               [clojure.repl :as repl]
               [clojure.string :as str]
               [instaparse.core :as insta]
               [clojure.core.matrix :as m]
               [anglican.runtime :refer [tanh observe* sample* normal exp cos sin log]]))


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


  (println "Homework 1: Problem 1")
  (def foppl-code '( (let [mu (sample (normal 1 (sqrt 5)))
                           sigma (sqrt 2)
                           lik (normal mu sigma)]
                       (observe lik 8)
                       (observe lik 9)
                       mu) ))
  (foppl-compiler foppl-code)


  (println "==================================================================")
  (println "==================================================================")
  (println "==================================================================")
  (println "Homework 2: Problem 1: \n")
  (def foppl-code '( (fn [x] (exp (sin x))) ))
  (println "foppl code: " foppl-code)
  (def general-ast (create-ast (function-parse (str foppl-code ))))
  (def expression (get-expression general-ast))
  (def forward-graph (function-logic-builder (get expression :forward-graph) 1))
  (println "")
  (println "forward graph: ")
  (println (print-graph-forward-evals forward-graph))
  (println "\n")
  (println "reverse mode auto-diff: x = 1")
  (println (reverse-mode-auto-diff forward-graph, (fn [x] 1), 1, 0))
  (println "==================================================================")
  (println "Homework 2: Problem 2: \n")
  (def foppl-code '( (fn [x y] (+ (* x x) (sin x))) ))
  (println "foppl code: " foppl-code)
  ; (def general-ast (create-ast (function-parse (str foppl-code ))))
  ; (def expression (get-expression general-ast))
  ; (def forward-graph (function-logic-builder (get expression :forward-graph) 1))
  ; (println "")
  ; (println "forward graph: ")
  ; (println (print-graph-forward-evals forward-graph))
  ; (println "\n")
  ; (println "reverse mode auto-diff: x = 1")
  ; (println (reverse-mode-auto-diff forward-graph, (fn [x y] 1 0), 1, 0))
  ; (println (reverse-mode-auto-diff forward-graph, (fn [x y] 0 1), 1, 0))
  ;(println (reverse-mode-auto-diff forward-graph, (fn [x] 1), 1, 0))
  (println "==================================================================")
  (println "Homework 2: Problem 3: \n")
  (def foppl-code '( (fn [x] (if (> x 5) (* x x) (+ x 18))) ))
  (println "==================================================================")
  (println "Homework 2: Problem 4: \n")
  (def foppl-code '( (fn [x] (log x)) ))
  (println "==================================================================")
  (println "Homework 2: Problem 5: \n")
  (def foppl-code '( (fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                                                  (* 2 (* sigma sigma))))
                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))) ))
  (println "==================================================================")
  (println "Homework 2: Problem 6: \n")
  (def foppl-code '( (fn [x mu sigma] (normpdf x mu sigma)) ))
  (println "==================================================================")
  (println "Homework 2: Problem 7: \n")
  (def foppl-code '( (fn [x mu sigma] (normpdf x mu sigma)) ))
  (println "==================================================================")
  )
