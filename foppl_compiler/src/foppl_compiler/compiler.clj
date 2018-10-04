
(declare translate definition-logic-builder desugar-loop-statment)

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

; orginize and clean input
(def function-parser
  (insta/parser
    "expression = '(' [' '| definition  | expression | #'[a-zA-Z]+' | #'[0-9]+' | '+' | '-' | '*' | '/' | #'_'+ | '.' | '=' ]* ')'
     definition = '[' (' '| definition  | expression | #'[a-zA-Z]+' | #'[0-9]+' | '+' | '-' | '*' | '/' | #'_'+ | '.' | '=' )* ']'"))


;helper functions
(defn zip-sets [x y]
  (for [xi x yi y] [xi yi] ) )
(defn vec-remove
 "remove elem in coll"
 [coll pos]
 (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn combine-dash-notation [v]
 (if (some #(= "-" %) v)
   (cond
     (integer? (read-string (subs (nth v (+ (.indexOf v "-") 1)) 0 1)  ))
       (combine-dash-notation (vec-remove (assoc v (.indexOf v "-") (str (nth v (.indexOf v "-")) (nth v (+ 1 (.indexOf v "-"))))) (+ (.indexOf v "-") 1)))
     (and (= " " (nth v (+ (.indexOf v "-") 1))) (nth v (- (.indexOf v "-") 1)))
     	v
     :else
   	(combine-dash-notation (vec-remove (vec-remove (assoc v (.indexOf v "-") (str (nth v (- (.indexOf v "-") 1)) (nth v (.indexOf v "-")) (nth v (+ 1 (.indexOf v "-"))))) (+ (.indexOf v "-") 1)) (- (.indexOf v "-") 1)))
   )
   v
   )
 )
; need to fix notation for when _ exists as empty argument
(defn combine-underscore-notation [v]
  (if (some #(= "_" %) v)
 	(combine-underscore-notation (vec-remove (vec-remove (assoc v (.indexOf v "_") (str (nth v (- (.indexOf v "_") 1)) (nth v (.indexOf v "_")) (nth v (+ 1 (.indexOf v "_"))))) (+ (.indexOf v "_") 1)) (- (.indexOf v "_") 1)))
    v
    )
   )
(defn fix-decimal-notation [v]
 (if (some #(= "." %) v)
 	(fix-decimal-notation (vec-remove (vec-remove (assoc v (.indexOf v ".") (str (nth v (- (.indexOf v ".") 1)) (nth v (.indexOf v ".")) (nth v (+ 1 (.indexOf v "."))))) (+ (.indexOf v ".") 1)) (- (.indexOf v ".") 1)))
   v)
   )

(defn function-parse [function-string]
 (into [] (function-parser function-string))
 )
(defn contains-val?
 [coll val]
 (reduce #(if (= val %2) (reduced true) %1) false coll))

(defn clean-expression [expression-vector]
 (combine-dash-notation (fix-decimal-notation (combine-underscore-notation (into [] (filter #(not (or (= " " %) (= "(" %) (= ")" %) (= "\n" %)   ))
 		expression-vector) ))))
 )
(defn clean-definition [definition-vector]
 (combine-dash-notation (fix-decimal-notation (combine-underscore-notation (into [] (filter #(not (or (= " " %) (= "[" %) (= "]" %) (= "\n" %)   ))
 		definition-vector) ))))
 )

(defn contains-vector [vector-ish-thing]
   (not (every? #{0}
     (for [x  vector-ish-thing]
       	(if (vector? x) 1 0) )
   ))
 )
(defn split-at' [idx v]
   [ [(subvec v 0 idx)] [(subvec v idx)] ] )

(defn get-lets-pairs [current-vector]
	(cond
   	(= 2 (count current-vector))
     		[current-vector]
     	(= 4 (count current-vector))
     		(split-at 2 current-vector)
     	(not (= 0 (mod (count current-vector) 2)))
     		(println "error in get-lets-pairs")
     	:else
     		(conj (get-lets-pairs (get (split-at 2 current-vector) 1)) (get (split-at 2 current-vector) 0))
     )
  )

(defn create-ast [parsed-function]
  (into [] (cond
    (= :expression (get parsed-function 0))
      (for [x (clean-expression parsed-function)]
        (if (vector? x)
          (create-ast x)
          x
          )
      )
    (= :definition (get parsed-function 0))
      (for [x  (clean-definition parsed-function)]
        (if (vector? x)
          (create-ast x)
          x
          )
      )
    :else
    	nil
    ))
  )

; non-random expressions within defitions
(defn get-random-variables [let-definition]
  (remove nil? (for [definition let-definition]
  	(cond
      (not (nil? (some #(= :sample %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :observe %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :normal %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :bernoulli %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :discrete %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :beta %) (flatten definition))))
      	(nth definition 3)
      (not (nil? (some #(= :uniform-continuous %) (flatten definition))))
      	(nth definition 3)
    :else
      nil
      )
    ))
  )

(defn replace-val-in-nested-vector [nested-vector value replacement]
  (into [] (for [element nested-vector]
    (cond
      (= value element)
      	replacement

      (vector? element)
      	(replace-val-in-nested-vector element value replacement)

      :else
      	element
      ))
    )
  )

(defn definition-logic-builder [current-vector]

(cond
  (= nil current-vector)
  	[]
 	(= :expression (get current-vector 0))
  	(cond
			; catch to ensure we store multiple function statements
        	(every? #(vector? %) (drop 1 current-vector))
        		(for [x (drop 1 current-vector)]
                (definition-logic-builder x)
                )

      		; the let
  			(= "let" (get current-vector 1))
              [:let ;defines variable values
                	  [:let-variables
                     (into [] (for [x (get-lets-pairs (into [] (rest (first (drop 2 current-vector)))))]
                			[:variable_id (gensym)
                 			 :variable_name (symbol (first x))
                 			 :value  (definition-logic-builder (last x))]
                				))]
                    [:let-return
                     	; if there are multiple statements, and a return value
                     	(into [] (for [statement (drop 3 current-vector) ]
                        [:statement (definition-logic-builder statement)]))
                    	]

                ]

        	; more basic functions
        	(= "if" (get current-vector 1))
        		[:apply 'if
               :predicate (definition-logic-builder (get current-vector 2))
               :then (definition-logic-builder (get current-vector 3))
               :else (definition-logic-builder (get current-vector 4))]
        	(= "=" (get current-vector 1))
        		[:apply '= :arg1 (definition-logic-builder (get current-vector 2))
               		   :arg2 (definition-logic-builder (get current-vector 3))]
        	(= "vector" (get current-vector 1))
        		(into []  (definition-logic-builder (into [] (rest (rest current-vector)))))
            (= "first" (get current-vector 1))
        		[:apply 'first :arg (definition-logic-builder (get current-vector 2))]
        	(= "second" (get current-vector 1))
        		[:apply 'second :arg (definition-logic-builder (get current-vector 2))]
        	(= "drop" (get current-vector 1))
        		[:apply 'drop :arg (definition-logic-builder (get current-vector 2))]
        	(= "rest" (get current-vector 1))
				[:apply 'rest :arg (definition-logic-builder (get current-vector 2))]
        	(= "loop" (get current-vector 1))
       			(desugar-loop-statment (definition-logic-builder (get current-vector 2))
                                       (definition-logic-builder (get current-vector 3))
                                       (definition-logic-builder (get current-vector 4))
                                       (into [] (for [x  (drop 5 current-vector)] (definition-logic-builder x))) )


        	(= "sqrt" (get current-vector 1))
        		[:apply 'sqrt :arg (definition-logic-builder (get current-vector 2))]
          	(= "conj" (get current-vector 1))
        		(into [] (remove nil? (conj [(definition-logic-builder (get current-vector 2))] (definition-logic-builder (get current-vector 3)) )))

      		(= "append" (get current-vector 1))
        		(println "error - need to impliment" (get current-vector 1))
        	(= "foreach" (get current-vector 1))
        		(println "error - need to impliment" (get current-vector 1))

        	; user defined functions
        	(= "defn" (get current-vector 1))
        		[:function-name (definition-logic-builder (get current-vector 2))
                  ;defines variable values
                 :function-variables
                     	(into [] (for [x (definition-logic-builder (first (drop 3 current-vector)))]
                			[:variable_id (gensym)
                 			 :variable_name (symbol x)]
                				) )
                 :function-return
                     	; if there are multiple statements, and a return value
                     	(into [] (for [statement (drop 4 current-vector) ]
                        (definition-logic-builder statement)))
                    	]


        	; random variable type functions
          	(= "sample" (get current-vector 1))
        		[:sampled (definition-logic-builder (get current-vector 2))]
          	(= "observe" (get current-vector 1))
        		; there seems to be some sort of issue here - but its from evaluation of first and rest on "data"
  				[:observe :random-variable (definition-logic-builder (get current-vector 2)) :values-observed (definition-logic-builder (get current-vector 3))]

         	; random variable type functions
         	(= "normal" (get current-vector 1))
  				[(keyword "normal")
                 [:mu (definition-logic-builder (get current-vector 2))
                  :sigma (definition-logic-builder (get current-vector 3))]]
         	(= "beta" (get current-vector 1))
  				[(keyword "beta")
                 [:alpha (definition-logic-builder (get current-vector 2))
                  :beta (definition-logic-builder (get current-vector 3))]]
         	(= "bernoulli" (get current-vector 1))
  				[(keyword "bernoulli")
                 [:p (definition-logic-builder (get current-vector 2))]]
		    (= "discrete" (get current-vector 1))
        		[:p (definition-logic-builder (get current-vector 2))]

          ; primitive operations
        	(= "+" (get current-vector 1))
        		[:apply '+ :argument1 (definition-logic-builder (get current-vector 2)) :argument2 (definition-logic-builder (get current-vector 3))]
        	(= "-" (get current-vector 1))
				[:apply '- :argument1 (definition-logic-builder (get current-vector 2)) :argument2 (definition-logic-builder (get current-vector 3))]
        	(= "*" (get current-vector 1))
				[:apply '* :argument1 (definition-logic-builder (get current-vector 2)) :argument2 (definition-logic-builder (get current-vector 3))]
        	(= "/" (get current-vector 1))
				[:apply '/ :argument1 (definition-logic-builder (get current-vector 2)) :argument2 (definition-logic-builder (get current-vector 3))]
            (= "exp" (get current-vector 1))
        		[:apply 'exp :argument (get current-vector 2)]
        	(= "sqrt" (get current-vector 1))
        		[:apply 'sqrt :argument (get current-vector 2)]

        	; if its just a number
         	(or (float? (get current-vector 1)) (integer? (get current-vector 1)))
  				(get current-vector 1)

         ; otherwise assume we inside a user defined function
         :else
              [:user-apply (symbol (get current-vector 1))
               :args (into [] (for [var (into [] (drop 2 current-vector))] (read-string var)))
               ])

  (= :definition (get current-vector 0))
  	(cond
        	(= (count current-vector) 1)
        		nil
  		    (and (string? (get current-vector 1)) )
        		(definition-logic-builder (into [] (drop 1 current-vector)))
        	(or (float? (get current-vector 1)) (integer? (get current-vector 1)))
  				(drop 1 current-vector)

          :else
              (println "jeez idk definition" current-vector)
        )

  :else
  	(if (vector? current-vector)
        (for [x current-vector] (definition-logic-builder x) )
        (read-string current-vector)
        )
  )
)

; reorder-dependencies in let definition so we dont have conflicts and can define as we go.
(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))
(defn get-dependent-variables [let-definitions]
  (for [variable  let-definitions]
       ; returns vector of vector with variable name and the names of the things its dependent on
  	   [(symbol (nth variable 3)) (into [] (remove nil?
                           (for [variable-name (for [v let-definitions] (nth v 3))]
                             (some #{(symbol variable-name)} (into [] (flatten (nth variable 5))))
                             )))]
    )
  )
(defn get-independent-variables [variables-list]
  (do
    (def independent-variable-names
      (remove nil? (for [variables variables-list]
        (if (empty? (nth variables 1))
          (nth variables 0)
           nil
        ))))
     (def new-variables-list
       (for [v variables-list]
         	[(first v) (remove (set independent-variable-names) (second v)) ]
         )
       )
 	)
  [(into [] new-variables-list) (into [] independent-variable-names)]
  )
(defn conj-if-not-present [v1 v2]
  (into [] (flatten (conj v1 (into [] (remove nil? (for [v v2] (if (nil? (some #(= v %) v1)) v nil)))))))
  )
(defn order-variable-def-dependencies [let-definitions]
  (loop [un-ordered-variables (get-dependent-variables let-definitions)
         ordered-variables []]
    (if (>= (count ordered-variables) (count un-ordered-variables))
      ordered-variables
      (recur (first (get-independent-variables un-ordered-variables))
             (into [] (flatten (conj-if-not-present ordered-variables (second (get-independent-variables un-ordered-variables))  ))))))
    )
(defn re-order-let-dependencies [let-variables]
    (sort-by #(.indexOf (order-variable-def-dependencies let-variables) (symbol (nth % 3)) ) let-variables)
    )
(defn get-let-variables [let-statement]
  (nth (nth (nth let-statement 0) 1) 1)
  )
(defn get-let-statements [let-statement]
   (nth (nth (nth let-statement 0) 2) 1)
  )
(defn replace-with-ordered [let-statement ordered-variables]
  [:let
	[:let-variables ordered-variables]
    [:let-return (get-let-statements let-statement)]]
  )
(defn desugar-loop-statment [c e f f_args]
 (let [v (into [] (for [v_i (into [] (take c (range))) ] (gensym)))
       a_args (into [] (for [e_i f_args] [:variable_id (gensym) :variable_name (gensym) :value e_i] ))]
     [:let [:let-variables a_args]
       	   [:let-return  (loop [v_i (nth v (- c 1))
                                  iter (- c 1)
                                  nested-let-statment (nth v (- c 1))]
                             (if (= iter 0)
                               nested-let-statment
                             (recur (nth v (- iter 1))
                                    (dec iter)
                                    [:let [:let-variables [[:variable_id (gensym)  :variable_name v_i
                                                            :value [:apply-in f :at-index iter :inside-of (nth v iter) :with-args f_args] ]]]
                                           [:let-return [:statement nested-let-statment]]]

                                      ) ) )]
       ]))
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
              [:let  [:let-variables [:variable_id (gensym) :variable_name '_ :value (nth (get-let-statements let-statement) (- iter-statements 1))]]
               		 [:let-return de-sugared] ])
      )
    )
  )
(defn de-sugar-let-statement [let-statement]
  (de-sugar-let-statement-helper [(replace-with-ordered let-statement (re-order-let-dependencies (get-let-variables let-statement)))])
  )
(defn generate-de-sugared-ast [foppl-code]
  (de-sugar-let-statement (vec (definition-logic-builder (create-ast (function-parse (str foppl-code ))))))
  )

; find all vector expressions and apply them to their arguments

; desugar loop statement

(defn get-let-variable-name [de-sugered-let-statement]
      (nth (nth (nth de-sugered-let-statement 1) 1) 3))
(defn get-let-variable-value [de-sugered-let-statement]
      (nth (nth (nth de-sugered-let-statement 1) 1) 5))
(defn get-let-statement [de-sugered-let-statement]
       (nth (nth de-sugered-let-statement 2) 1))
(defn get-if-predicate [if-statement]
  (nth if-statement 3)
  )
(defn get-if-then [if-statement]
  (nth if-statement 5)
  )
(defn get-if-else [if-statement]
  (nth if-statement 7)
  )

(defn merge-graphs-apply [g1 g2 primitive]
    ; same as merge-graphs except we apply to outputs
    ; takes a union of vertices, edges, partial functions, observations
    ; then reduces partial functons so we only have link functions between rv
    	{:G { :V (flatten (remove nil? (set [(get (get g1 :G) :V) (get (get g2 :G) :V)])))
              :A (vec (remove empty? (concat (vector (get (get g1 :G) :A)) (vec (get (get g2 :G) :A)))))
              :P (remove empty? (flatten [(get (get g1 :G) :P) (get (get g2 :G) :P)]))
              :Y (flatten (remove nil? (set [(get (get g1 :G) :Y) (get (get g2 :G) :Y)])))}
    	 :E
       	(try ( (resolve primitive) (get g1 :E) (get g2 :E) )
          (catch Exception e  (seq [primitive (get g1 :E) (get g2 :E)]) )  )
       }
    )
(defn resolve-primitive [g primitive]
    ; same as merge-graphs except we apply to outputs
    ; takes a union of vertices, edges, partial functions, observations
    ; then reduces partial functons so we only have link functions between rv
    	{:G (get g :G)
    	 :E (try ( (resolve primitive) (get g :E) )
          (catch Exception e  (seq [primitive (get g :E)]) )  )
       }
    )

(defn merge-graphs [g1 g2]
    ; takes a union of vertices, edges, partial functions, observations
    ; then reduces partial functons so we only have link functions between rv
    	{:G {  :V (flatten (remove nil? (set [(get-in g1 [:G :V]) (get-in g2 [:G :V])])))
               :A (vec (remove empty? (concat (vector (get (get g1 :G) :A)) (vec (get (get g2 :G) :A)))))
               :P (remove empty? (flatten [(get (get g1 :G) :P) (get (get g2 :G) :P)]))
               :Y (flatten (remove nil? (set [(get (get g1 :G) :Y) (get (get g2 :G) :Y)])))}
    	 :E  (get g2 :E)} )

(defn update-expression-if [new-g g1 g2 g3]
    (assoc new-g :E (if (get g1 :E) (get g2 :E) (get g3 :E)))
    )
(defn create-normal-rv [normal-statement]
    	(try
       (normal (nth normal-statement 1) (nth normal-statement 3))
       (catch Exception e (symbol [:normal :mu (nth normal-statement 1) :sigma (nth normal-statement 3)] )  ))
    )
(defn get-pdf [distribution]
    (cond
     		(str/includes? (type distribution) "normal")
            [:name  (type distribution)
             :mean  (get distribution :mean)
             :sd	(get distribution :sd) ]
      	:else
      		(println distribution "error get-pdfs")
      )

    )
(defn score-function [expression, v, phi, rho]
    (cond
      ; score((c E1 . . .En), v) = (pc v E1 . . .En)
      (str/includes? (type expression) "distribution")
      	(cond
            (str/includes? (type expression) "normal")
            	[v (seq ['normal (get (translate (get expression :mean) phi rho) :E) (get (translate  (get expression :sd) phi rho) :E)])]
            :else
            	(println " no known dist - in score")
            )
      ; score((if E1 E2 E3), v) = (if E1 F2 F3)  [:apply if :predicate y :then z :else 1]
      (vector? expression)
        (cond
  		 (= (nth expression 1) 'if)
              (if   (get (translate (nth expression 3) phi rho) :E)
                    (score-function (get (translate (nth expression 5) phi rho) :E) v phi rho)
                    (score-function (get (translate (nth expression 7) phi rho) :E) v phi rho))
  		:else
          (println "error in vector in score")
          )
       ; score(E, v) = false otherwise
      :else
      	expression
      )
    )
(defn expression-a-dist? [e]
    (if (str/includes? (type e) "distribution")
      e
      nil
      )
    )
(defn decompose-expression [e]
    (cond
      (str/starts-with? (str e ) "(normal")
      	(remove nil?
              (for  [x (drop 1 (str/split (str/replace (str/replace (str e ) #"\(" "") #"\)" "") #" "))]
                	(if (symbol? (read-string x)) (read-string x) nil )
                  ))
  	(str/starts-with? (str e) "(if")
      	(println "havent dealt with this yet.")
  	:else
      	(println "not a known expression, check spacing" e)
      )
    )



(defn get-free-variables [E phi rho]
    (try
       (cond
        (vector? E)
            (remove nil? (flatten (for [x E] (get-free-variables x phi rho))))
        :else
            (cond
                (= anglican.runtime.normal-distribution (type E))
                  (if (empty? (into [] (remove nil? [(get-free-variables (get (translate (get E :mean) phi rho) :E) phi rho)
                                (get-free-variables (get (translate (get E :sd) phi rho) :E) phi rho)])))
                    nil
                    (into [] (remove nil? [(get-free-variables (get (translate (get E :mean) phi rho) :E) phi rho)
                                (get-free-variables (get (translate (get E :sd) phi rho) :E) phi rho)])))

                (or (float? E) (integer? E) )
                   	nil

               	(= (type E) clojure.lang.PersistentVector$ChunkedSeq)
              		(first (into [] (remove nil?  (for [x (into [] E)]
                      	(if (and (not (= 'normal x)) (symbol? x) )
                          x
                          nil
                          )
                      ))))

                (symbol? E)
                    E
                :else
                    nil
            )
        )
      (catch Exception e (println "get-free-variables [E phi rho]" (translate (get E :mean)) phi rho)))
    )

(defn get-user-fxn-variables [user-fxn-syntax]
  (for [variable (nth (second user-fxn-syntax) 3)]
    (nth variable 3)
    )
  )
(defn get-user-fxn-statements [user-fxn-syntax]
  (for [variable (nth (second user-fxn-syntax) 5)]
    variable
    )
  )

(defn convert-user-fn [user-fxn-syntax new-args]
  (for [statement (get-user-fxn-statements user-fxn-syntax)]
    (loop [ iter 0
            variables (get-user-fxn-variables user-fxn-syntax)
           	updated-statement statement]
      (if (= iter (count variables))
        updated-statement
        (recur (inc iter)
               variables
               (replace-val-in-nested-vector updated-statement (nth variables iter) (nth new-args iter))
          ))))
  )

(defn desuger-statements [statements] ;[[statement: statement1] [statement: statement2] [statement: statement3]]
    (into [] (for [statement statements]
      (if (= :let (first (second statements)))
          (de-sugar-let-statement [(second statements)])
          statement
      )
    )))


(defn foppl-compiler [foppl-code]

    ; sugared code
    (def suger-code (vec (definition-logic-builder (create-ast (function-parse (str foppl-code ))))))

    ; get user defined functions
   	(def rho (into {} (remove nil? (for [program suger-code]
      	(cond  (= (first program) :function-name) ; user defined function
              		{(second program) program} (= (first program) :let) nil
               		:else (println " error in outer compiler function"))))))

    ; get all other statements
  	(def general-statements
      (remove nil? (for [program suger-code] (cond  (= (first program) :function-name)
              nil (= (first program) :let) program))))

    ; iterate through each of our user functions and desugar
    (def de-sugared-statements
      (into []
        (for [statements general-statements]
          (if (= (first statements) :let)
            (de-sugar-let-statement [statements])
             statements ))))

    ; iterate through each of the statements and desugar
    (def de-sugared-rho
      (into []
        (for [user-funcs (into [] (keys rho))]
          ; only need to desugar the expression it represents
          (if (vector? (nth (get rho user-funcs) 5))
            (if (vector? (first (nth (get rho user-funcs) 5)))
              (if (= :let (first (first (nth (get rho user-funcs) 5))))
                [(nth (get rho user-funcs) 0) (nth (get rho user-funcs) 1)
                 (nth (get rho user-funcs) 2) (nth (get rho user-funcs) 3)
                 (nth (get rho user-funcs) 4)
             	 (de-sugar-let-statement (nth (get rho user-funcs) 5))]
                (get rho user-funcs))
              (get rho user-funcs))
            (get rho user-funcs)))))
  	(println "==================================================================")
   	(println "here is the graph: \n \n" (translate (first de-sugared-statements) true de-sugared-rho))
  	(println "")
 	  (println "here are the number of arcs: " (count (get-in (translate (first de-sugared-statements) true de-sugared-rho) [:G :A])) )
   	(println "")
  	(println "here are the number of vertices: " (count (get-in (translate (first de-sugared-statements) true de-sugared-rho) [:G :V])) )
    (println "==================================================================")
  )


(defn translate [branch phi rho]
  	(cond

      ; variable-rule
      (symbol? branch)
          {:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

      ; constant
      (or (= true branch) (= false branch) (integer? branch) (float? branch)) ;is this a constant?
         {:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

      ; is it a partially evaluateable fxn
      (= (type branch) clojure.lang.PersistentVector$ChunkedSeq)
      	{:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

    ; expression
    (vector? branch)
        (cond
            ; let statements
        		(= (first branch) :let)
                  		;(try
                  (let [translated-e1 (translate (get-let-variable-value branch) phi rho)]
                        (merge-graphs translated-e1
                        (translate (replace-val-in-nested-vector (get-let-statement branch) ; now we replace v with e1 everywhere in e2
                                                            (get-let-variable-name branch)
                                                            (get translated-e1 :E) ) phi rho) ))
            ; if statements
            (= (second branch) 'if)
          		(update-expression-if
                  (merge-graphs
                    (merge-graphs
                      ; expression for e1
                      (translate (get-if-predicate branch) phi rho)
                      ; expression for e2
                      (translate (get-if-then branch)) phi rho)
                    ; expression for e3
                    (translate (get-if-else branch)) phi rho)
                  (translate (get-if-predicate branch) phi rho)
                  (translate (get-if-then branch) phi rho)
                  (translate (get-if-else branch)) phi rho)


          ; sample statements
            (= (first branch) :sampled)
          		(try

                  (let [v (gensym)]
                    {:G  {:V (into [] [(get-in (translate (second branch) phi rho) [:G :V]) v ]) ; V U v
                          :A (if (empty? (get-in (translate (second branch) phi rho) [:G :V]))
                               (zip-sets (get-free-variables (get (translate (second branch) phi rho) :E) phi rho) [v])
                          	   (apply concat (zip-sets (get-free-variables (get (translate (second branch) phi rho) :E) phi rho) [v])
                                   		     (zip-sets (get-in (translate (second branch) phi rho) [:G :V]) [v])))
                          :P {:name v :F (score-function (get (translate (second branch) phi rho) :E) v phi rho)}
                          :Y (get (get (translate (second branch) phi rho) :G) :Y)}
                     :E v})

                  (catch Exception e (println "error sampled rule" (println branch) ))
                  )


          ; observe statement
            (= (first branch) :observe)
                  (let [v (gensym)
                        G1E1 (translate (nth branch 2) phi rho)
                        G2E2 (translate (nth branch 4) phi rho)
                        MGE (merge-graphs G1E1 G2E2)]

                    (try  ; {:name G__67184, :F 1}

                      {:G {:V (remove nil? [(get-in MGE [:G :V]) v])

                           :A (concat (get-in MGE [:G :A]) [(get-free-variables (score-function (get G1E1 :E) v phi rho) phi rho) v])

                           :P {:name v :F (if phi (score-function (get G1E1 :E) v phi rho)
                                            1 ) }
                           :Y {:name v :O (get G2E2 :E) } }
                       :E (get G2E2 :E) }

                      (catch Exception e (println "error observe rule" branch)))

                  )

          ; primitive operations this includes distributions
            (= (first branch) :normal)
        		(merge-graphs-apply
                  (translate (nth (second branch) 1) phi rho)
                  (translate (nth (second branch) 3) phi rho)
                  'normal)

          ; handles the looping statements - had to make another convert-user-fn
          	(= (first branch) :apply-in)
          		(println "we finally made it here!!!")

            (and (= (first branch) :apply) (not (= (second branch) :observe)))
          		(try
                    (cond
                      	(= (count branch) 4)
                            (resolve-primitive
                              (translate (nth branch 3) phi rho)
                              (nth branch 1))
                        :else
                            (merge-graphs-apply
                              (translate (nth branch 3) phi rho)
                              (translate (nth branch 5) phi rho)
                              (second branch))
                    )
          		(catch Exception e (println "primitive rules -apply " (count branch)))
                  )

          ; general statement wrapper - i.e. did we hit a statement - also handles user defined vals
          	(= (first branch) :statement)
          		(if (vector? (second branch))
                  (if (= (first(second branch)) :user-apply)
                    (try
                      (translate (first (into [] (convert-user-fn (second branch) (nth (second  branch) 3)))) phi rho)
                      (catch Exception e (println branch)))

                    (translate (second branch) phi rho)
                    )
                  (translate (second branch) phi rho)
                  )


            (vector? (first branch))
              	(translate (first branch) phi rho)
          	(vector? branch)
          		 {:G {:V  [] :A  [] :P  [] :Y  [] } :E (into [] (for [x branch] (get (translate x phi rho) :E)))}
        		:else
                  		(println (type branch) "error in translation of vectors cond")
          )
      :else
        (println branch "error in outer translation cond")
  	)

  )
