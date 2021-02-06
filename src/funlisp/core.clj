(ns funlisp.core
  (:gen-class)
  (:require [funlisp.eval-simple :as evaler]
            [funlisp.parser-simple :as parser]))

(defn get-test-read-fn
  [to-read]
  (def remaining (cons 'placeholder to-read))
  (fn []
    (do
      (def remaining (rest remaining))
      (first remaining))))

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
          (let [[result next-env] (eval-fn (first parsed) env)]
            (do
              (print-fn result)
              (repl read-fn print-fn parse-fn eval-fn next-env))))))))

(defn -main
  "Start a Funlisp REPL"
  [& args]
  (println "Funlisp REPL (read-eval-print loop):")
  (repl read-line
        (fn [out] (println " >" out))
        parser/parse
        evaler/eval-exp
        {}))
