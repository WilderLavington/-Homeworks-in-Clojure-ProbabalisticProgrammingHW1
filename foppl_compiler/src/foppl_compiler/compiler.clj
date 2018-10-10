
(declare translate)
(def empty-graph  )
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
    (if (not (= 'get primitive))
    	{:G { :V (flatten (remove nil? (set [(get (get g1 :G) :V) (get (get g2 :G) :V)])))
              :A (vec (remove empty? (concat (vector (get (get g1 :G) :A)) (vec (get (get g2 :G) :A)))))
              :P (remove empty? (flatten [(get (get g1 :G) :P) (get (get g2 :G) :P)]))
              :Y (flatten (remove nil? (set [(get (get g1 :G) :Y) (get (get g2 :G) :Y)])))}
    	 :E
       	(try ( (resolve primitive) (get g1 :E) (get g2 :E) )
          (catch Exception e  (seq [primitive (get g1 :E) (get g2 :E)]) )  )
       }
       {:G { :V (flatten (remove nil? (set [(get (get g1 :G) :V) (get (get g2 :G) :V)])))
               :A (vec (remove empty? (concat (vector (get (get g1 :G) :A)) (vec (get (get g2 :G) :A)))))
               :P (remove empty? (flatten [(get (get g1 :G) :P) (get (get g2 :G) :P)]))
               :Y (flatten (remove nil? (set [(get (get g1 :G) :Y) (get (get g2 :G) :Y)])))}
        :E (seq [primitive (get g1 :E) (get g2 :E)])
        }
       )
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
    	{:G {:V (flatten (remove nil? (set [(get-in g1 [:G :V]) (get-in g2 [:G :V])])))
               :A (vec (remove empty? (concat (vector (get (get g1 :G) :A)) (vec (get (get g2 :G) :A)))))
               :P (remove empty? (flatten [(get (get g1 :G) :P) (get (get g2 :G) :P)]))
               :Y (flatten (remove nil? (set [(get (get g1 :G) :Y) (get (get g2 :G) :Y)])))}
    	 :E  (get g2 :E)} )
(defn merge-graph-vector [graphs]
  (loop [iter 0
         graph  {:G {:V nil  :A nil :P nil :Y nil} :E nil}]
  (if (= iter (count graphs))
    graph
    (recur (inc iter) (merge-graphs graph (get graphs iter))))))

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

(defn convert-user-fn [user-fxn-statement user-fxn-variables new-args]
  (if (vector? user-fxn-statement)
      (for [statement user-fxn-statement]
          (if (vector? statement)
              (loop [iter 0
                     variables user-fxn-variables
                     updated-statement statement]
                (if (= iter (count variables))
                  updated-statement
                  (recur (inc iter)
                         variables
                         (replace-val-in-nested-vector updated-statement (nth variables iter) (nth new-args iter)) )))
               statement
                    ))
      user-fxn-statement))

(defn foppl-compiler [foppl-code]

    ; generate ast
    (def programs (program-wrapper foppl-code))

    ; get all user defined functions into hashmap and set statements
    (def user-defs
      (into {} (for [udef (remove nil? (into [] (for [program-statement (second programs)]
              (if (= :function-name (first (second program-statement)))
              (second program-statement) nil ))))]
              {(symbol (nth udef 1)) {:args (nth udef 3) :eval (nth udef 5)}})))

    ; get all general statements
    (def general-statements
      (remove nil? (into [] (for [program-statement (second programs)]
          (if (= :function-name (first (second program-statement)))
              nil
              (second program-statement) )))))

    ; now start translation
    (for [statement general-statements]
        (translate statement true user-defs)
      )
  )

(defn translate [branch phi rho]
  	(cond

      ; variable-rule
      (symbol? branch)
          {:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

      ; constant
      (or (= true branch) (= false branch) (integer? branch) (float? branch)) ;is this a constant?
         {:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

      ; is it a partially evaluated fxn
      (= (type branch) clojure.lang.PersistentVector$ChunkedSeq)
      	{:G {:V  [] :A  [] :P  [] :Y  [] } :E branch}

    ; expression
    (vector? branch)
        (cond
            ; let statements
        		(= (first branch) :let)
                  (let [translated-e1 (translate (get-let-variable-value branch) phi rho)]
                        (merge-graphs translated-e1
                        (translate (replace-val-in-nested-vector (get-let-statement branch) ; now we replace v with e1 everywhere in e2
                                                            (get-let-variable-name branch)
                                                            (get translated-e1 :E) ) phi rho) ))
            ; if statements
            (= (second branch) 'if)
                (let [translation1 (translate (get-if-predicate branch) phi rho)
                      translation2 (translate (get-if-then branch) phi rho)
                      translation3 (translate (get-if-else branch) phi rho)]
                		  (update-expression-if
                        (merge-graphs
                          (merge-graphs translation1 translation2) translation3)
                          translation1 translation2 translation3) )

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
            (= (first branch) :discrete)
            		(merge-graphs-apply
                      (translate (nth (second branch) 1) phi rho)
                      (translate (nth (second branch) 3) phi rho)
                      'discrete)

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

          ; general statement wrapper - i.e. did we hit a function - also handles user defined vals
          	(= (first branch) :statement)
            		(translate (second branch) phi rho)

            (= (first branch) :user-apply)
                (let [GE_i (into [] (for [argument (nth branch 3)] (translate argument phi rho)))
                      function-args (into [] (get-in rho [(second branch) :args]))
                      function-statement (into [] (get-in rho [(second branch) :eval]))]
                  (merge-graph-vector
                    (into [] (concat GE_i
                    [(translate (into [] (convert-user-fn
                      function-statement
                      (take-last (count (into [] (for [G GE_i] (get G :E)))) function-args)
                      (into [] (for [G GE_i] (get G :E))) )) phi rho)]))))

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
