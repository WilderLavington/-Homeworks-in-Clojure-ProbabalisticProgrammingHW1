(ns foppl-compiler.partial-evaluation-test
  (:require [foppl-compiler.partial-evaluation :refer [partial-evaluation deterministic?
                                                       fixed-point-simplify]]
            [clojure.test :refer [deftest testing is]]))



(deftest partial-evaluation-test
  (testing "Testing partial evaluation of FOPPL expressions."
    (is (= '(let [s (first [5 (bernoulli 0.9)])] (- a 1 21))
           (partial-evaluation '(let [s (first [5 (bernoulli 0.9)])]
                                  (- a
                                     (first (rest (rest [1 1 1 1])))
                                     (get {:a (* 3 7)} :a))))))
    (is (= '(rest (rest data))
           (partial-evaluation '(rest (rest data)))))))


(deftest deterministic?-test
  (testing "Testing the filtering of deterministic functions"
    (is (deterministic? '+))

    (is (deterministic? '(+ 1 2)))

    (is (not (deterministic? '(bernoulli 0.8))))
    ))


(deftest fix-point-test
  (testing "Testing fix-point evaluation."
    (is (= '(7)
           (fixed-point-simplify '(let [a 5]
                                    (first [(let [b 7]
                                              (rest [a b]))])))))))
