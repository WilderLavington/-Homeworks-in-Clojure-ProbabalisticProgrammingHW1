(ns foppl-compiler.ast-creation
  (:require [anglican.runtime :refer [sqrt sin cos log exp normal observe* sample* uniform-continuous dirichlet
                                      gamma discrete flip]]
            [anglican.core :refer :all]
            [clojure.set :as set]
            [foppl-compiler.helper-fxns :refer :all]
            [foppl-compiler.parser :refer :all]
            [clojure.string :as str]))

(declare definition-logic-builder)

; let desugaring
(defn get-dependent-variables [let-definitions]
  (for [variable  let-definitions]
       ; returns vector of vector with variable name and the names of the things its dependent on
  	   [(symbol (nth variable 3)) (into [] (remove nil?
                           (for [variable-name (for [v let-definitions] (nth v 3))]
                             (some #{(symbol variable-name)} (into [] (flatten (nth variable 5))))
                             )))]))

(defn get-independent-variables [variables-list]
  (do (def independent-variable-names
      (remove nil? (for [variables variables-list]
        (if (empty? (nth variables 1))
          (nth variables 0)
           nil ))))
      (def new-variables-list
       (for [v variables-list]
         	[(first v) (remove (set independent-variable-names) (second v)) ])))
  [(into [] new-variables-list) (into [] independent-variable-names)])

(defn order-variable-def-dependencies [let-definitions]
  (loop [un-ordered-variables (get-dependent-variables let-definitions)
         ordered-variables []]
    (if (>= (count ordered-variables) (count un-ordered-variables))
      ordered-variables
      (recur (first (get-independent-variables un-ordered-variables))
             (into [] (flatten (conj-if-not-present ordered-variables (second (get-independent-variables un-ordered-variables))  )))))))

(defn re-order-let-dependencies [let-variables]
    (sort-by #(.indexOf (order-variable-def-dependencies let-variables) (symbol (nth % 3)) ) let-variables))

(defn get-let-variables [let-statement]
  (nth (nth (nth let-statement 0) 1) 1))

(defn get-let-statements [let-statement]
   (nth (nth (nth let-statement 0) 2) 1))

(defn replace-with-ordered [let-statement ordered-variables]
  [:let [:let-variables ordered-variables]
        [:let-return (get-let-statements let-statement)]])

(defn de-sugar-let-statement-helper [let-statement]
  (loop [iter-variables (count (get-let-variables let-statement))
         iter-statements (- (count (get-let-statements let-statement)) 1)
         de-sugared (last (get-let-statements let-statement))]
    (cond
      (and (= iter-variables 0) (= iter-statements 0))
     	de-sugared
      (and (= iter-statements 0) (not (= iter-variables 0)))
      	(recur (dec iter-variables) iter-statements
               [:let [:let-variables (nth (get-let-variables let-statement) (- iter-variables 1))]
               		   [:let-return de-sugared] ])
      :else
      	(recur iter-variables (dec iter-statements)
              [:let  [:let-variables [:variable_id (gensym) :variable_name (gensym) :value (nth (get-let-statements let-statement) (- iter-statements 1))]]
               		   [:let-return de-sugared] ]))))

(defn de-sugar-let-statement [let-statement]
  (de-sugar-let-statement-helper
    [(replace-with-ordered let-statement (re-order-let-dependencies (get-let-variables let-statement)))]))

; loop desugaring
(defn desugar-loop-statment [c e f f_args]
 (let [v (into [] (for [v_i (into [] (take (+ c 1) (range))) ] (gensym)))
       a_args (into [] (for [e_i f_args] [:variable_id (gensym) :variable_name (gensym) :value e_i] ))]
    (loop [v_i (nth v c)
                                  iter c
                                  nested-let-statment (nth v c)]
                             (if (= iter 0)
                               nested-let-statment
                             (recur (nth v (- iter 1))
                                    (dec iter)
                                    [:let [:let-variables [:variable_id (gensym)  :variable_name v_i
                                                           :value [:user-apply f
                                                                   :args (into [] (concat [[:apply 'get :arg1 (nth v iter) :arg2 iter]] f_args))]] ]
                                          [:let-return [:statement nested-let-statment]]] )))
                                          ))

; foreach desugaring
(defn desugar-foreach-statement [c sugared-let-defs expression]
  ; first lets set up the un-sugared statements
  [:apply 'vector :arg
    (first (into [] (for [counter (into [] (take c (range)))]
      ; we can just desugar the let statements on the spot
      ;(de-sugar-let-statement
        [:let [:let-variables
            (if (empty? (into []
                (let [current-vector-val (into [] (take-nth 2 (rest sugared-let-defs)))
                      variables (into [] (take-nth 2 sugared-let-defs))]
                    (concat (into []
                        (for [v (into [] (take (count variables) (range)))]
                          [:variable_id (gensym) :variable_name (nth variables v) :value [:apply 'get :arg1 (nth current-vector-val v)
                                          :arg2 counter]]))))))
               [:variable_id (gensym)  :variable_name (gensym) :value nil]
               (into [] (let [current-vector-val (take-nth 2 (rest sugared-let-defs))
                       variables (take-nth 2 sugared-let-defs)]
                       (concat (into []
                           (for [v (into [] (take (count variables) (range)))]
                             [:variable_id (gensym) :variable_name v :value [:apply 'get :arg1 (get current-vector-val v)
                                             :arg2 counter]]))))))

                                          ]
             [:let-return expression]];)
             )))])

; builds digestable - desugared - logic to feed to compile rules
(defn definition-logic-builder [current-vector]
  (cond
    (= nil current-vector)
    	[]
    (= :dictionary (get current-vector 0))
      (into []
        (loop [variable-map (rest current-vector)
               mappings [:dictionary]]
        (if (empty? variable-map)
          mappings
          (recur
            (drop 3 variable-map)
            (conj mappings [:name (definition-logic-builder (first variable-map))
                            :value (definition-logic-builder (second variable-map))])))))
   	(= :expression (get current-vector 0))
    	(cond
  			; catch to ensure we store multiple function statements
           (every? #(vector? %) (drop 1 current-vector))
          		(for [x (drop 1 current-vector)]
                  (definition-logic-builder x)
                  )

        		; the let
    			 (= "let" (get current-vector 1))
              (de-sugar-let-statement
                [[:let ;defines variable values
                  	  [:let-variables
                       (into [] (for [x (get-lets-pairs (into [] (rest (first (drop 2 current-vector)))))]
                  			[:variable_id (gensym)
                   			 :variable_name (symbol (first x))
                   			 :value  (definition-logic-builder (last x))]
                  				))]
                      [:let-return
                       	; if there are multiple statements, and a return value
                       	(into [] (for [statement (drop 3 current-vector) ]
                          [:statement (definition-logic-builder statement)])) ]]])

          	; if statement
           (= "if" (get current-vector 1))
            [:if
             :boolean (definition-logic-builder (get current-vector 2))
             :then (definition-logic-builder (get current-vector 3))
             :else (definition-logic-builder (get current-vector 4))]
            ; equality defs
           (= "=" (get current-vector 1))
          		[:apply '= :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "<=" (get current-vector 1))
          		[:apply '<= :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= ">=" (get current-vector 1))
          		[:apply '>= :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "<" (get current-vector 1))
             [:apply '< :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= ">" (get current-vector 1))
             [:apply '> :args (definition-logic-builder (into [] (drop 2 current-vector)))]

           ; basic functions
           (= "vector" (get current-vector 1))
          		[:apply 'vector :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "first" (get current-vector 1))
          		[:apply 'first :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "second" (get current-vector 1))
          		[:apply 'second :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "drop" (get current-vector 1))
          		[:apply 'drop :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "rest" (get current-vector 1))
  				    [:apply 'rest :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "conj" (get current-vector 1))
          		[:apply 'conj :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "cons" (get current-vector 1))
              [:apply 'cons :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "append" (get current-vector 1))
              [:apply 'append :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "get" (get current-vector 1))
              [:apply 'get :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "reduce" (get current-vector 1))
              [:apply 'reduce :args (definition-logic-builder (into [] (drop 2 current-vector)))]
           (= "peek" (get current-vector 1))
              [:apply 'peek :args (definition-logic-builder (into [] (drop 2 current-vector)))]

           ; user defined functions
           (= "defn" (get current-vector 1))
          		[ :user-function
                :function-name (definition-logic-builder (get current-vector 2))
                ;defines variable values
                :function-variables
                   	(into [] (for [x (drop 1 (nth current-vector 3))]
              			  (symbol x)))
                :function-return
                   	; iterate through all statements and desugar
                    (first (into [] (for [statement [(nth current-vector 4)]]
                      (definition-logic-builder statement)
                      )))]

           (= "fn" (get current-vector 1))
              [ :anonymous-function
                :function-name (gensym)
                ;defines variable values
                :function-variables (into [] (for [x (drop 1 (nth current-vector 2))]
                     (symbol x)))
                :function-return
                     ; iterate through all statements and desugar
                     (first (into [] (for [statement [(nth current-vector 3)]]
                       (definition-logic-builder statement)
                       )))]

          	; random variable type functions
            (= "sample" (get current-vector 1))
          		  [:sample (definition-logic-builder (get current-vector 2))]
            (= "observe" (get current-vector 1))
    				    [:observe
                 :random-variable (definition-logic-builder (get current-vector 2))
                 :values-observed (definition-logic-builder (get current-vector 3))]

           	; random variable type functions
           	(= "normal" (get current-vector 1))
    				    [:normal
                   [:mu (definition-logic-builder (get current-vector 2))
                    :sigma (definition-logic-builder (get current-vector 3))]]
           	(= "beta" (get current-vector 1))
    				    [:beta
                   [:alpha (definition-logic-builder (get current-vector 2))
                    :beta (definition-logic-builder (get current-vector 3))]]
            (= "uniform-continuous" (get current-vector 1))
    				    [:uniform-continuous
                   [:a (definition-logic-builder (get current-vector 2))
                    :b (definition-logic-builder (get current-vector 3))]]
           	(= "bernoulli" (get current-vector 1))
    				    [:bernoulli
                   [:p (definition-logic-builder (get current-vector 2))]]
  		      (= "discrete" (get current-vector 1))
                [:discrete
          		     [:p (definition-logic-builder (get current-vector 2))]]
            (= "flip" (get current-vector 1))
                [:flip
         		       [:p (definition-logic-builder (get current-vector 2))]]

            ; primitive operations
          	(= "+" (get current-vector 1))
          		[:apply '+ :args (definition-logic-builder (into [] (drop 2 current-vector)))]
          	(= "-" (get current-vector 1))
  				    [:apply '- :args (definition-logic-builder (into [] (drop 2 current-vector)))]
          	(= "*" (get current-vector 1))
  				    [:apply '* :args (definition-logic-builder (into [] (drop 2 current-vector)))]
          	(= "/" (get current-vector 1))
  				    [:apply '/ :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "exp" (get current-vector 1))
          		[:apply 'exp :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "log" (get current-vector 1))
            	[:apply 'log :args (definition-logic-builder (into [] (drop 2 current-vector)))]
          	(= "sqrt" (get current-vector 1))
          		[:apply 'sqrt :args (definition-logic-builder (into [] (drop 2 current-vector)))]

            ; matrix operations
            (= "mat-transpose" (get current-vector 1))
              [:apply 'mat-transpose :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "mat-tanh" (get current-vector 1))
              [:apply 'mat-tanh :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "mat-add" (get current-vector 1))
              [:apply 'mat-add :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "mat-mul" (get current-vector 1))
              [:apply 'mat-mul :args (definition-logic-builder (into [] (drop 2 current-vector)))]
            (= "mat-repmat" (get current-vector 1))
              [:apply 'mat-repmat :args (definition-logic-builder (into [] (drop 2 current-vector)))]

           ; otherwise assume we inside a user defined function
           :else
             [:user-apply (nth current-vector 1)
              :args (definition-logic-builder (into [] (drop 2 current-vector)))])

    (= :definition (get current-vector 0))
    	(cond
          	(= (count current-vector) 1)
          		[]
    		    (and (string? (get current-vector 1)) )
          		(definition-logic-builder (into [] (drop 1 current-vector)))
          	(or (float? (get current-vector 1)) (integer? (get current-vector 1)))
    				  [(drop 1 current-vector)]
            (vector? (get current-vector 1))
              (into [] (for [x (drop 1 current-vector)] (definition-logic-builder x)))
            :else
                (println "error in definition" current-vector))

    :else
    	(cond
        (vector? current-vector)
          (into [] (for [x current-vector] (definition-logic-builder x) ))
        (keyword? current-vector)
          (println "error keyword" current-vector)
        (symbol? current-vector)
          (println "error symbol" current-vector)
        :else
          (read-string current-vector))))


; now we want a program-wrapper
(defn program-wrapper [foppl-code]
    (let [ast (create-ast foppl-code)]
        [:program
        (into [] (for [statement (drop 1 ast)]
          [:program-statement (definition-logic-builder statement)]
        ))]))
