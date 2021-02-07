(ns funlisp.core
  (:gen-class)
  (:require [funlisp.eval-simple :as evaler]
            [funlisp.parser-simple :as parser]
            [clojure.string :as string]))

(comment
 (defn get-test-read-fn
   [to-read]
   (def remaining (cons 'placeholder to-read))
   (fn []
     (do
       (def remaining (rest remaining))
       (first remaining))))
 )

(defn repl
  [read-fn print-fn parse-fn eval-fn env]
  (let [to-execute (read-fn)]
    (if (nil? to-execute)
      nil
      (let [parsed (parser/parse to-execute)]
        (if (false? parsed)
          (do
            (print-fn "ERROR: cannot parse")
            (repl read-fn print-fn parse-fn eval-fn env))
          (let [[result next-env] (eval-fn parsed env)]
            (do
              (print-fn result)
              (repl read-fn print-fn parse-fn eval-fn next-env))))))))

(defn verbose-slurp
  [fpath]
  (println (string/join ["Reading definitions from: " fpath]))
  (slurp fpath))

(defn -main
  "Start a Funlisp REPL"
  [& args]
  (println "WELCOME TO FUNLISP")
  (let [init-code (parser/parse
                   (verbose-slurp "resources/examples.funlisp"))]
    ;(println "Read code:")
    ;(println init-code)
    ;(println "Funlisp REPL (read-eval-print loop):")
    (repl read-line
          (fn [out] (println " >" out))
          parser/parse
          evaler/eval-exp-list
          (second ; <- extract environment
           (evaler/eval-exp-list init-code {})))))
