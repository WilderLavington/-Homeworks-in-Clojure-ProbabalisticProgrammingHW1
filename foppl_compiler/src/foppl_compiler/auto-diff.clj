
; helper functions

(def partial-fns
    (reduce
      merge
      (list
        {'* [(fn [a b] b) (fn [a b] a)]}
        ; f(a,b) = a * b <-> (* a b)
        ; df/da = b
        ; df/db = a

        {'- [(fn [a b] 1) (fn [a b] -1)]}
        ; f(a,b) = a - b <-> (- a b)
        ; df/da = 1
        ; df/db = -1

        {'+ [(fn [a b] 1) (fn [a b] 1)]}
        ; f(a,b) = a + b <-> (+ a b)
        ; df/da = 1
        ; df/db = 1

        {'/ [(fn [a b] (/ 1 b)) (fn [a b] (* a (/ -1 (* b b))))]}
        ; f(a,b) = a / b <-> (/ a b)
        ; df/da = 1
        ; df/db = -1/b^2

        {'exp [(fn [a] (exp a))]}
        ; f(a) = (exp a)
        ; df/da = (exp a)

        {'relu [(fn [a] (if (> a 0) 1 0))]}
        ; f(a) = (relu a)
        ; df/da = 1 if a > 0, 0 otherwise

        {'log [(fn [a] (/ 1 a))]}

        {'normpdf [(fn [y m s] (observe* (normal m s) y))]}

        {'sin [(fn [a] (cos a))]})))

(defn normpdf [y m s]
  (observe* (normal m s) y))

(defn addd [exprl i d]
  (if (= i 0)
    (reduce conj [`(~'+ ~d ~(first exprl))] (subvec exprl 1))
    (reduce conj (subvec exprl 0 i)
            (reduce conj [`(~'+ ~d ~(get exprl i))] (subvec exprl (+ i 1))))))

(defn finite-difference-expr [expr args i d]
  `(~'/ (~'- (~expr ~@(addd args i d)) (~expr ~@args)) ~d))

(defn finite-difference-grad [expr]
  (let [[op args body] expr
        d (gensym)
        fdes (map #(finite-difference-expr expr args % d) (range (count args)))
        argsyms (map (fn [x] `(~'quote ~x)) args)]
    `(~'fn [~@args]
       (~'let [~d 0.001]
         ~(zipmap argsyms fdes)))))



; need to fix what is being passed where, the function is evaluating through
(defn function-logic-builder [general-ast, variables]
    ; recursively set up our function evaluator tree
    (cond
      (vector? general-ast)
        (cond
          (= (second general-ast) "exp")
            {:node (symbol (str 'exp (gensym)))
             :fxn (fn [x] (exp x))
             :deriv (fn [x] (exp x))
             :forward-pass-eval
             (try (exp
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval))
             (catch Exception e (seq ['exp
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "sin")
            {:node (symbol (str 'sin (gensym)))
             :fxn (fn [x] (sin x))
             :deriv (fn [x] (cos x))
             :forward-pass-eval
             (try (sin
               (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval))
             (catch Exception e (seq ['sin
               (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "cos")
            {:node (symbol (str 'cos (gensym)))
             :fxn (fn [x] (cos x))
             :deriv (fn [x] (- (sin x)))
             :forward-pass-eval
             (try (cos
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval))
             (catch Exception e (seq ['cos
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "log")
            {:node (symbol (str 'log (gensym)))
             :fxn (fn [x] (log x))
             :deriv (fn [x] (/ 1 x))
             :forward-pass-eval
             (try (log
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval))
             (catch Exception e (seq ['log
                (get (function-logic-builder (nth general-ast 2) variables) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "+")
            {:node (symbol (str '+ (gensym)))
             :fxn (fn [x y] (+ x y))
             :grad (fn [x y] [1 1])
             :forward-pass-eval
             (try (+
               (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
               (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval))
             (catch Exception e (seq ['+
               (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
               (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)
                                  :arg2 (function-logic-builder (nth general-ast 3) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "-")
            {:node (symbol (str '- (gensym)))
             :fxn (fn [x y] (- x y))
             :grad (fn [x y] [1 -1])
             :forward-pass-eval
             (try (-
                (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
                (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval))
             (catch Exception e (seq ['-
                (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
                (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)
                                  :arg2 (function-logic-builder (nth general-ast 3) variables)}
             :reverse-auto-diff-eval [] }

          (= (second general-ast) "*")
            {:node (symbol (str '* (gensym)))
             :fxn (fn [x y] (* x y))
             :deriv (fn [x y] [y x])
             :forward-pass-eval
             (try (*
                (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
                (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval))
             (catch Exception e (seq ['*
                (get (first (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)
                (get (second (function-logic-builder (nth general-ast 2) variables)) :forward-pass-eval)])))
             :forward-pass-graph {:arg1 (function-logic-builder (nth general-ast 2) variables)
                                  :arg2 (function-logic-builder (nth general-ast 3) variables)}
             :reverse-auto-diff-eval [] }

          :else
            (println "inner function-logic-builder error" second general-ast)
          )

      (or (integer? general-ast) (float? general-ast))
        {:node (gensym)
         :fxn (fn [] general-ast)
         :deriv (fn [] 0)
         :forward-pass-eval general-ast
         :forward-pass-graph general-ast
         :reverse-auto-diff-eval nil}

      (symbol? general-ast)
        {:node variables
         :fxn (fn [] variables)
         :deriv (fn [] 0)
         :forward-pass-eval variables
         :forward-pass-graph variables
         :reverse-auto-diff-eval nil}

      (string? general-ast)
        {:node variables
         :fxn (fn [] variables)
         :deriv (fn [] 0)
         :forward-pass-eval variables
         :forward-pass-graph variables
         :reverse-auto-diff-eval nil}

      :else
        (println "outer function-logic-builder error" general-ast)
      )
    )

(defn get-expression [general-ast]
    {:input-variables (into [] (rest (nth (nth general-ast 1) 2)))
     :output-variables []
     :forward-graph (into [] (nth (nth general-ast 1) 3))
     :backward-graph [] }
  )
(defn print-graph [g]
  (println ":node " (get g :node))
  (println ":fxn " (get g :fxn))
  (println ":deriv " (get g :deriv))
  (println ":forward-pass-eval " (get g :forward-pass-eval))
  (println ":reverse-auto-diff-eval " (get g :reverse-auto-diff-eval))
  ;(println ":forward-pass-graph " (get g :forward-pass-graph ))
  ;(println ":reverse-pass " (get g :reverse-pass))
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
      (for [sub-graphs (into [] (vals (get g :forward-pass-graph)))]
        (print-graph-forward-evals sub-graphs)
      )))
    )

(defn reverse-mode-auto-diff [g, last-partial, last-partial-eval, level] ; takes in an eval graph made above.
    ; check that we havent hit bottom and update
    (if (map? g)
      (if (map? (get g :forward-pass-graph))
          (first (for [sub-graphs (into [] (vals (get g :forward-pass-graph)))]
          (reverse-mode-auto-diff
                  sub-graphs
                  (get g :deriv)
                  (* (last-partial (get g :forward-pass-eval))  last-partial-eval)
                  (inc level))))
          (* (last-partial (get g :forward-pass-graph))  last-partial-eval)
                  )
        last-partial-eval
        )
      )
