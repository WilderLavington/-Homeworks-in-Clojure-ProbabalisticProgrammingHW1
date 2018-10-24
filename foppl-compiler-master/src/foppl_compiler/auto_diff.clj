(ns foppl-compiler.auto-diff
  (:require [anglican.runtime :refer [pow sin cos log exp normal observe* sample* uniform-continuous]]
            [anglican.core :refer :all]
            [foppl-compiler.parser :refer :all]
            [clojure.string :as str]))

(defn NaN?
  "Test if this number is nan"
  [x]
  ; Nan is the only value for which equality is false
  (Double/isNaN x))

(defn normpdf [x mu sigma] (observe* (normal mu sigma) x))

; need to fix what is being passed where, the function is evaluating through
(defn function-logic-builder [general-ast, variables, argument]
    ; recursively set up our function evaluator tree
    (cond
      (vector? general-ast)
        (cond

          (= (second general-ast) "if")
              ; if wont return any intermediary info, we will just eval + collapse
              (if (function-logic-builder (nth general-ast 2) variables argument)
                  ; then generate the graph that is created
                  (function-logic-builder (nth general-ast 3) variables argument)
                  (function-logic-builder (nth general-ast 4) variables argument))

          ; just in case we run into an if statement
          (= (second general-ast) ">")
            (> (get (function-logic-builder (nth general-ast 2) variables argument) :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables argument) :forward-pass-eval))
          (= (second general-ast) "<")
            (< (get (function-logic-builder (nth general-ast 2) variables argument) :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables argument) :forward-pass-eval))
          (= (second general-ast) ">=")
            (>= (get (function-logic-builder (nth general-ast 2) variables argument) :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables argument) :forward-pass-eval))
          (= (second general-ast) "<=")
            (<= (get (function-logic-builder (nth general-ast 2) variables argument) :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables argument) :forward-pass-eval))

          ; now for the functions
          (= (second general-ast) "normpdf")
            {:node (symbol (str 'normalpdf (gensym)))
             :arg argument
             :fxn (fn [x mu sigma] (normpdf x mu sigma))
             :deriv
               {"x" (fn [x mu sigma]
                 (if (NaN?
                   (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                      (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma))))))))
                                                      (/ (* (- mu x) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                        (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma sigma))))))
                   0
                   (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma))))))))
                                                       (/ (* (- mu x) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                        (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma sigma)))))))
                "mu" (fn [x mu sigma]
                  (if (NaN?
                   (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592 (* sigma sigma))))))))
                   (- 0 (/ (* (- mu x) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                         (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma sigma)))))))
                   0
                   (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                          (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma))))))))
                   (- 0 (/ (* (- mu x) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                            (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma sigma)))))) ))
                "sigma" (fn [x mu sigma]
                  (if (NaN?
                  (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma))))))))
                  (/ (* (- (* (- x mu) (- x mu) ) (* sigma sigma)) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                         (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma (* sigma sigma)))))))
                  0
                  (* (exp (* -1 (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma))))
                                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589794 (* sigma sigma))))))))
                  (/ (* (- (* (- x mu) (- x mu) ) (* sigma sigma)) (exp (/ (- 0 (* (- x mu) (- x mu))) (* 2 (* sigma sigma))) ))
                         (* (pow (* 2 3.141592653589794) 0.5) (* sigma (* sigma (* sigma sigma)))))) ))}
             :forward-pass-eval
             (try (apply (fn [x mu sigma] (normpdf x mu sigma))
                [(get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                 (get (function-logic-builder (nth general-ast 3) variables "mu") :forward-pass-eval)
                 (get (function-logic-builder (nth general-ast 4) variables "sigma") :forward-pass-eval)])
             (catch Exception e (seq ['normpdf
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables "mu") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 4) variables "sigma") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")
                                  "mu" (function-logic-builder (nth general-ast 3) variables "mu")
                                  "sigma" (function-logic-builder (nth general-ast 4) variables "sigma")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "exp")
            {:node (symbol (str 'exp (gensym)))
             :arg argument
             :fxn (fn [x] (exp x))
             :deriv {"x" (fn [x] (exp x))}
             :forward-pass-eval
             (try (exp
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval))
             (catch Exception e (seq ['exp
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "sin")
            {:node (symbol (str 'sin (gensym)))
             :arg argument
             :fxn (fn [x] (sin x))
             :deriv {"x" (fn [x] (cos x))}
             :forward-pass-eval
             (try (sin
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval))
             (catch Exception e (seq ['sin
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "cos")
            {:node (symbol (str 'cos (gensym)))
             :arg argument
             :fxn (fn [x] (cos x))
             :deriv {"x" (fn [x] (- (sin x)))}
             :forward-pass-eval
             (try (cos
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval))
             (catch Exception e (seq ['cos
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "log")
            {:node (symbol (str 'log (gensym)))
             :arg argument
             :fxn (fn [x] (log x))
             :deriv {"x" (fn [x] (/ 1 x))}
             :forward-pass-eval
             (try (log
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval))
             (catch Exception e (seq ['log
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")}
             :reverse-auto-diff-eval nil }

          ; and the primitives
          (= (second general-ast) "+")
            {:node (symbol (str '+ (gensym)))
             :arg argument
             :fxn (fn [x y] (+ x y))
             :deriv { "x" (fn [x y] 1)
                      "y" (fn [x y] 1)}
             :forward-pass-eval
             (try (+
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval))
             (catch Exception e (seq ['+
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")
                                  "y" (function-logic-builder (nth general-ast 3) variables "y")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "-")
            {:node (symbol (str '- (gensym)))
             :arg argument
             :fxn (fn [x y] (- x y))
             :deriv { "x" (fn [x y] 1)
                      "y" (fn [x y] -1)}
             :forward-pass-eval
             (try (-
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval))
             (catch Exception e (seq ['-
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")
                                  "y" (function-logic-builder (nth general-ast 3) variables "y")}
             :reverse-auto-diff-eval nil }

          (= (second general-ast) "*")
            {:node (symbol (str '* (gensym)))
             :arg argument
             :fxn (fn [x y] (* x y))
             :deriv {"x" (fn [x y] y)
                     "y" (fn [x y] x)}
             :forward-pass-eval
             (try (*
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval))
             (catch Exception e (seq ['*
                (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
                (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval)])))
             :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")
                                  "y" (function-logic-builder (nth general-ast 3) variables "y")}
             :reverse-auto-diff-eval nil }

         (= (second general-ast) "/")
           {:node (symbol (str '* (gensym)))
            :arg argument
            :fxn (fn [x y] (/ x y))
            :deriv {"x" (fn [x y] (/ 1 y))
                    "y" (fn [x y] (* x (/ -1 (* y y))))}
            :forward-pass-eval
            (try (/
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval))
            (catch Exception e (seq ['/
               (get (function-logic-builder (nth general-ast 2) variables "x") :forward-pass-eval)
               (get (function-logic-builder (nth general-ast 3) variables "y") :forward-pass-eval)])))
            :forward-pass-graph {"x" (function-logic-builder (nth general-ast 2) variables "x")
                                 "y" (function-logic-builder (nth general-ast 3) variables "y")}
            :reverse-auto-diff-eval nil }

          :else
            (println "inner function-logic-builder error" second general-ast)
          )

      ; and the base cases
      (or (integer? general-ast) (float? general-ast))
        {:node (gensym)
         :arg argument
         :fxn (fn [] general-ast)
         :deriv (fn [] 0)
         :forward-pass-eval general-ast
         :forward-pass-graph general-ast
         :reverse-auto-diff-eval 0}

      (string? general-ast)
        ; check if its a stored function or a string val
        (if (get variables general-ast)
        ; if its a variable
          {:node general-ast
           :arg argument
           :fxn (fn [] (get variables general-ast))
           :deriv (fn [] 1)
           :forward-pass-eval (get variables general-ast)
           :forward-pass-graph (get variables general-ast)
           :reverse-auto-diff-eval 0}
        ; if its not.
           {:node general-ast
            :arg argument
            :fxn (fn [] (read-string general-ast))
            :deriv (fn [] 0)
            :forward-pass-eval (read-string general-ast)
            :forward-pass-graph (read-string general-ast)
            :reverse-auto-diff-eval 0}
          )

      :else
        (println "outer function-logic-builder error" general-ast)
      )
    )

(defn print-graph [g]
  (println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ")
  (println ":node " (get g :node))
  (println ":argument " (get g :arg))
  (println ":fxn " (get g :fxn))
  (println ":deriv " (get g :deriv))
  (println ":forward-pass-eval " (get g :forward-pass-eval))
  (println ":reverse-auto-diff-eval " (get g :reverse-auto-diff-eval))
  )

(defn print-graph-forward-evals [g]
  ; check if there are lower levels to display
  (if (map? g)
    ; print current graph
    (print-graph g)
  )
  ; are those lower levels variables or another graph
  (if (map? g)
    (if (map? (get g :forward-pass-graph))
      (doseq [sub-graphs (into [] (vals (get g :forward-pass-graph)))]
        (print-graph-forward-evals sub-graphs)
      )))
    )

(defn n-args [f]
    (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn function-map [variable-map function]
  (cond
    (= (n-args function) 1)
      (apply function [(get variable-map "x")])
    (= (n-args function) 2)
      (apply function [(get variable-map "x")
                       (get variable-map "y")])
    (= (n-args function) 3)
      (apply function [(get variable-map "x")
                       (get variable-map "mu")
                       (get variable-map "sigma")])
    :else
      (println "not ready for this function just yet"))
  )

(defn generate-variable-map [g]
  (into [] (merge (let [argument-variable (into [] (keys (get g :forward-pass-graph)))
               sub-graphs (into [] (vals (get g :forward-pass-graph)))]
             (for [index (into [] (take (count argument-variable) (range)))]
                { (get (nth sub-graphs index) :arg)
                  (get (nth sub-graphs index) :forward-pass-eval) }
             )))))

(defn reverse-mode-auto-diff [g, last-partial-eval, level] ; takes in an eval graph made above.
      ; check that we are still in a map
      (if (map? g)
        ; check if we have hit bottom
        (if (map? (get g :forward-pass-graph))
            ; if we have not, we are going to update the reverse mode pass in our current g
            {:node (get g :node)
             :arg (get g :arg)
             :fxn (get g :fxn)
             :deriv (get g :deriv)
             :forward-pass-eval (get g :forward-pass-eval)
             :forward-pass-graph
            ;(try
                  (let [argument-variable (into [] (keys (get g :forward-pass-graph)))
                        sub-graphs (into [] (vals (get g :forward-pass-graph)))]

                      (into {} (merge
                        (for [index (into [] (take (count argument-variable) (range)))]
                           {(nth argument-variable index)
                           (reverse-mode-auto-diff
                                     (nth sub-graphs index)
                                     (* (function-map (apply merge (generate-variable-map g)) (get (get g :deriv)
                                     (nth argument-variable index)))
                                        last-partial-eval)
                                     (inc level))}

                         ))))
            ;(catch Exception e (println "\n error!!! \n" level last-partial-eval "\n error!!!\n")))
            :reverse-auto-diff-eval
                last-partial-eval
            }

            ; if we are at a leaf then we need only one update
            (if (or (integer? (read-string (get g :node))) (float? (read-string (get g :node))))
                  {:node (get g :node)
                   :fxn (get g :fxn)
                   :deriv (get g :deriv)
                   :forward-pass-eval (get g :forward-pass-eval)
                   :forward-pass-graph (get g :forward-pass-eval)
                   :reverse-auto-diff-eval
                      0
                   }
                  {:node (get g :node)
                   :fxn (get g :fxn)
                   :deriv (get g :deriv)
                   :forward-pass-eval (get g :forward-pass-eval)
                   :forward-pass-graph (get g :forward-pass-eval)
                   :reverse-auto-diff-eval
                      last-partial-eval
                   })
             )
          (println "error" g last-partial-eval)
          )

    )

(defn dissoc-by [f m] (->> m (filter #(f (first %))) (into {})))

(defn calculate-gradient-helper [reverse-mode-graph gradient]
     (flatten [(if (map? reverse-mode-graph)
       ; check if we have hit bottom
       (if (map? (get reverse-mode-graph :forward-pass-graph))
          (into [] (for [sub-graphs (into [] (vals (get reverse-mode-graph :forward-pass-graph)))]
               (calculate-gradient-helper sub-graphs gradient)))
          (if (contains? gradient (get reverse-mode-graph :node))
              (assoc gradient (get reverse-mode-graph :node) (+ (get gradient (get reverse-mode-graph :node)) (get reverse-mode-graph :reverse-auto-diff-eval)))
              (assoc gradient (get reverse-mode-graph :node) (get reverse-mode-graph :reverse-auto-diff-eval)) ))
       (println "error g is not a map"))])
    )

(defn calculate-gradient [reverse-mode-graph gradient]
    (let [total-grad (into [] (calculate-gradient-helper reverse-mode-graph gradient)) ]
      (dissoc-by #(not (or (integer? (read-string %)) (float? (read-string %)))) (apply merge-with + total-grad))
    )
  )

(defn numerical-approx [function values h]
  (for [dim (into [] (take (count values) (range)))]
    (* (/ 1 h) (* 0.5 (- (apply function (assoc values dim (+ (get values dim) h)))
       (apply function (assoc values dim (- (get values dim) h))))) ) )
  )

(defn create-formatted-args [variables values]
  (into {}
    (for [idx (into [] (take (count values) (range)))]
      {(nth variables idx) (nth values idx)})))

(defn correct-grad [variables gradient-calc]
  (into {} (for [var variables]
    (if (contains? gradient-calc var)
      {var (get gradient-calc var)}
      {var 0.0} ) ) ) )

(defn autodiff [f-expression arguments]
    ; takes in quoted expression f (i.e. '(fn ...) ) and its desired arguments then returns the
    ; function evaluation as well as partials at that point with respect to
    ; each of its variables (i.e. the gradient), additionally I added a numerical
    ; check onto the return so we now we are right.
    (let [general-ast (create-ast f-expression)]
    (let [formatted-args (create-formatted-args (into [] (rest (nth general-ast 2))) arguments)]
    (let [graph (reverse-mode-auto-diff (function-logic-builder (into [] (nth general-ast 3)) formatted-args "root") 1 0)]
        {:graphical-representation graph
         :gradient (correct-grad (into [] (rest (nth general-ast 2))) (calculate-gradient graph {}) )
         :eval (get graph :forward-pass-eval)}
      ))))
(defn autodiff-fast [f-expression arguments]
    ;(println arguments)
    ; takes in quoted expression f (i.e. '(fn ...) ) and its desired arguments then returns the
    ; function evaluation as well as partials at that point with respect to
    ; each of its variables (i.e. the gradient), additionally I added a numerical
    ; check onto the return so we now we are right.
    (let [general-ast (create-ast f-expression)]
    (let [formatted-args (create-formatted-args (into [] (rest (nth general-ast 2))) arguments)]
    (let [graph (reverse-mode-auto-diff (function-logic-builder (into [] (nth general-ast 3)) formatted-args "root") 1 0)]
        {;:graphical-representation graph
         :gradient (correct-grad (into [] (rest (nth general-ast 2))) (calculate-gradient graph {}) )
         ;:eval (get graph :forward-pass-eval)
         }
      ))))
