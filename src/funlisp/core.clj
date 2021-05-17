(ns funlisp.core
  (:gen-class)
  (:require [funlisp.eval-simple :as evaler]
            [funlisp.redlisp-compilation.eval-1 :as rlisp-eval1]
            [funlisp.redlisp-compilation.eval-cps :as rlisp-eval-cps]
            [funlisp.redlisp-compilation.eval-df :as rlisp-eval-df]
            [funlisp.redlisp-compilation.eval-dfstack :as rlisp-eval-dfstack]
            [funlisp.redlisp-compilation.eval-dfstep :as rlisp-eval-dfstep]
            [funlisp.redlisp-compilation.eval-dualstack :as rlisp-eval-dualstack]
            [funlisp.redlisp-compilation.eval-stack :as rlisp-eval-stack]
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

(defn cons?
  [thing]
  (and (map? thing)
       (thing :cons?)))

(declare prettify)

(defn cons->list
  [cpair]
  (cond
    (= 'nil cpair) '()
    (not (map? cpair)) false
    (= 'nil (cpair :cdr)) (list (prettify (cpair :car)))
    true (let [rest-list (cons->list (cpair :cdr))]
           (if rest-list
             (cons (prettify (cpair :car))
                   (cons->list (cpair :cdr)))
             false))))

(defn cons-prettify
  [cpair]
  (let [list-form (cons->list cpair)]
    (if list-form
      list-form
      (list (prettify (cpair :car))
            '.
            (prettify (cpair :cdr))))))

(defn prettify
  [r]
  (cond
    (string? r) (string/join ["\"" r "\""])
    (map? r) (cond
               (r :function?)
               {:function? true
                :body (or (r :body) (r :body-exp))
                :env "__FUNLISP_CLOSURE__"}
               (r :cons?)
               (cons-prettify r)
               true r)
    true r))

(defn -main
  "Start a Funlisp REPL"
  [& args]
  (println "WELCOME TO FUNLISP")
  (let [init-code (parser/parse
                   (verbose-slurp "resources/examples.funlisp"))]
    ;(println "Read code:")
    ;(println init-code)
    (println "READY. Funlisp REPL; type expressions and press enter to evaluate:")
    (repl read-line
          (fn [out] (println " >" (prettify out)))
          parser/parse
          rlisp-eval-dualstack/eval-exp-list ; <- change eval function here!
          {})))
          ;(second ; <- extract environment
          ; (rlisp-eval1/eval-exp-list init-code {})))))
