(defproject foppl-compiler "0.1.0-SNAPSHOT"
  :description "First order probabilistic programming language compiler."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0-beta1"]
                 [instaparse "1.4.9"]
                 [anglican "1.0.0"]]
  :main foppl-compiler.core
  :min-lein-version "2.0.0")
