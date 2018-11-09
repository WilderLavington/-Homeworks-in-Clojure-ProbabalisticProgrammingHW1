(ns foppl-compiler.parser
  (:require [anglican.runtime :refer :all]
            [clojure.string :as str]
            [instaparse.core :as insta]))

; dedicated parser function

; takes in fopple code and then parses it "correctly" into a format
; that can be fed into my logic builder function

(def crude-parser
  (insta/parser
    "expression = '(' [ '' | ' '| definition | expression | dictionary | #'[a-zA-Z]+' | #'[0-9]+' | '>' | '<' | '=>' | '<=' | '+'
    | '-' | '*' | '/' | #'_'+ | '.' | ',' | '=' ]* ')'
     definition = '[' [ '' | ' '| definition | expression | dictionary | #'[a-zA-Z]+' | #'[0-9]+' | '>' | '<' | '=>' | '<=' | '+'
    | '-' | '*' | '/' | #'_'+ | '.' | ',' | '=' ]* ']'
     dictionary = '{' [ '' | ' '| definition | expression | dictionary | #'[a-zA-Z]+' | #'[0-9]+' | '>' | '<' | '=>' | '<=' | '+'
    | '-' | '*' | '/' | #'_'+ | '.' | ',' | '=' ]* '}'"))

(defn crude-parse [function-string]
    (into [] (crude-parser function-string)))

(defn vec-remove [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn forward-combine [v, position]
   (vec-remove
          (assoc v position (str (nth v position) (nth v (+ position 1)) ))
          (+ position 1)))

(defn backward-combine [v, position]
   (vec-remove
          (assoc v position (str (nth v position) (nth v (- position 1)) ))
          (- position 1)))

(defn full-combine [v, position]
   (vec-remove (vec-remove
          (assoc v position (str (nth v (- position 1)) (nth v position) (nth v (+ 1 position))))
          (+ position 1)) (- position 1)))

(defn pre-filter-clean-helper [crude]
  ; handels everything that is not a vector within current layer of ast
  (loop [index 0
         current-parse crude]

    (if (>= index (- (count current-parse) 1))
      current-parse
      (if (or (= (type (nth current-parse index)) clojure.lang.Keyword)
              (vector? (nth current-parse index))
              (= (nth current-parse index) "[") (= (nth current-parse index) "]")
              (= (nth current-parse index) "{") (= (nth current-parse index) "}")
              (= (nth current-parse index) "(") (= (nth current-parse index) ")")
              (= (nth current-parse index) " "))
          (recur (inc index) current-parse)
          (cond
            (and (vector? (nth current-parse (+ index 1)))
                 (vector? (nth current-parse (- index 1))))
                (recur (inc index) current-parse)

            (vector? (nth current-parse (- index 1)))
                (cond
                 ; forward combine
                 (and (not (= (type (nth current-parse (+ index 1))) clojure.lang.Keyword))
                      (not (= (nth current-parse (+ index 1)) " "))
                      (not (= (nth current-parse (+ index 1)) "(")) (not (= (nth current-parse (+ index 1)) ")"))
                      (not (= (nth current-parse (+ index 1)) "{")) (not (= (nth current-parse (+ index 1)) "}"))
                      (not (= (nth current-parse (+ index 1)) "[")) (not (= (nth current-parse (+ index 1)) "]")))
                   (recur 0 (forward-combine current-parse index))
                  :else
                      ; if we cant combine anything
                    (recur (inc index) current-parse))
            (vector? (nth current-parse (+ index 1)))
                (cond
                  ; reverse combine
                  (and (not (= (type (nth current-parse (- index 1))) clojure.lang.Keyword))
                       (not (= (nth current-parse (- index 1)) " "))
                       (not (= (nth current-parse (- index 1)) "(")) (not (= (nth current-parse (- index 1)) ")"))
                       (not (= (nth current-parse (- index 1)) "{")) (not (= (nth current-parse (- index 1)) "}"))
                       (not (= (nth current-parse (- index 1)) "[")) (not (= (nth current-parse (- index 1)) "]")))
                    (recur 0 (backward-combine current-parse index))
                  :else
                      ; if we cant combine anything
                      (recur (inc index) current-parse))
            :else
                (cond
                 ; forward combine
                  (and (not (= (type (nth current-parse (+ index 1))) clojure.lang.Keyword))
                       (not (= (nth current-parse (+ index 1)) " "))
                       (not (= (nth current-parse (+ index 1)) "(")) (not (= (nth current-parse (+ index 1)) ")"))
                       (not (= (nth current-parse (+ index 1)) "{")) (not (= (nth current-parse (+ index 1)) "}"))
                       (not (= (nth current-parse (+ index 1)) "[")) (not (= (nth current-parse (+ index 1)) "]")))
                    (recur 0 (forward-combine current-parse index))
                  ; reverse combine
                  (and (not (= (type (nth current-parse (- index 1))) clojure.lang.Keyword))
                       (not (= (nth current-parse (- index 1)) " "))
                       (not (= (nth current-parse (- index 1)) "(")) (not (= (nth current-parse (- index 1)) ")"))
                       (not (= (nth current-parse (- index 1)) "{")) (not (= (nth current-parse (- index 1)) "}"))
                       (not (= (nth current-parse (- index 1)) "[")) (not (= (nth current-parse (- index 1)) "]")))
                    (recur 0 (backward-combine current-parse index))
                  ; total combine
                  (and (not (= (type (nth current-parse (+ index 1))) clojure.lang.Keyword))
                       (not (= (nth current-parse (+ index 1)) " "))
                       (not (= (nth current-parse (+ index 1)) "(")) (not (= (nth current-parse (+ index 1)) ")"))
                       (not (= (nth current-parse (+ index 1)) "{")) (not (= (nth current-parse (+ index 1)) "}"))
                       (not (= (nth current-parse (+ index 1)) "[")) (not (= (nth current-parse (+ index 1)) "]"))
                       (not (= (type (nth current-parse (- index 1))) clojure.lang.Keyword))
                       (not (= (nth current-parse (- index 1)) " "))
                       (not (= (nth current-parse (- index 1)) "(")) (not (= (nth current-parse (- index 1)) ")"))
                       (not (= (nth current-parse (- index 1)) "{")) (not (= (nth current-parse (- index 1)) "}"))
                       (not (= (nth current-parse (- index 1)) "[")) (not (= (nth current-parse (- index 1)) "]")))
                    (recur 0 (full-combine current-parse index))
                  :else
                      ; if we cant combine anything
                      (recur (inc index) current-parse)) )))))

(defn pre-filter-clean [crude]
  (into [] (for [x (pre-filter-clean-helper crude)]
      (if (vector? x)
           (pre-filter-clean x)
          x ) ) ))

(defn filter-clean [refined]
  (into [] (for [x (into [] (filter #(not (or (= " " %) (= "(" %) (= ")" %) (= "{" %) (= "}" %) (= "\n" %)
                                              (= "]" %) (= "[" %) )) refined))]
      (if (vector? x)
           (filter-clean x)
          x ) ) ))

(defn create-ast [foppl-code]
  (filter-clean (pre-filter-clean (crude-parse (str foppl-code ))))
  )
