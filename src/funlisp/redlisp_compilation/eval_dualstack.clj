(ns funlisp.redlisp-compilation.eval-dualstack
  (:gen-class))

;; eval_dfstep, but with:
;; - the stack split into separate instruction/expression and value stacks
;;   (called the estack and vstack respectively)
;; - environment handling merged with the stack

;; Helper functions:
(defn starts-with? [l el] (= (first l) el)) ; does collection l start with el?
(defn third [coll] (nth coll 2)) ; third element in a collection
(defn fourth [coll] (nth coll 3)) ; ...
(defn function
  "Return the data structure we will use to represent a function with
    a certain body, environment, and argument names"
  [body env arg-names]
  {:function? true
   :body-exp body
   :env env ; -> the environment the function was defined in
   ; (we store the environment so the language supports closures)
   :args arg-names})
(defn cons-pair
  "Return the data structure for a cons pair."
  [a b]
  {:cons? true
   :car a ; car/cdr are the standard Lisp terms
   :cdr b})

; environment functions:
(defn environment
  [[vname & vnames] [val & vals]]
  (if (nil? vname)
    {:env? true}
    (assoc (environment vnames vals)
           vname val)))

(defn environment? [env]
  (and (map? env)
       (env :env?)))

(defn vstack->env
  ([[v & vs] acc]
  (if (nil? v)
    acc
    (recur
      vs
      (if (environment? v)
        (merge v acc) ; note: acc takes precedence
        acc))))
  ([vstack]
   (vstack->env vstack (environment [] []))))

(defn search-envs
  [val-stack vname]
  (if (empty? val-stack)
    nil
    (let [[v & vs] val-stack]
      (if (and (map? v)
               (v :env?)
               (v vname))
        (v vname)
        (recur vs vname)))))

(defn step
  [estack vstack] ; <- form of a step
  (let [[e & es] estack
        [v & vs] vstack]
    (cond
      ; move any literal from the E-stack to the V-stack:
      (or (true? e) (false? e) (nil? e)   ; true/false/nil
          (number? e)                     ; numbers
          (and (map? e)
               (or (e :cons?)             ; cons pair
                   (e :function?))))      ; function literal
      [es (conj vstack e)]
      ; we have a variable first on the E-stack, so find its value:
      (symbol? e)
      [(conj es [:VAR-LOOKUP {:var e}])
       vstack]
      ; if:
      (starts-with? e 'if)
      [(conj es
             [:TEST {:consequent (third e)
                     :alternate (fourth e)}]
             (second e))
       vstack]
      ; def:
      (starts-with? e 'def)
      [(conj es
             [:DEFINE {:vname (second e)}]
             (third e))
       vstack]
      ; +:
      (starts-with? e '+)
      [(conj es
             [:PLUS {}]
             (second e)
             (third e))
       vstack]
      ; =:
      (starts-with? e '=)
      [(conj es
             [:EQUALS {}]
             (second e)
             (third e))
       vstack]
      ; cons:
      (starts-with? e 'cons)
      [(conj es
             [:CONS {}]
             (second e)
             (third e))
       vstack]
      ; first:
      (starts-with? e 'first)
      [(conj es
             [:FIRST {}]
             (second e))
       vstack]
      ; rest:
      (starts-with? e 'rest)
      [(conj es
             [:REST {}]
             (second e))
       vstack]
      ; lambda:
      (starts-with? e 'lambda)
      [(conj es
             (function (first (rest (rest e))) ; note: only 1-expression functions!
                       (vstack->env vstack) ; merges all envs below on vstack
                       (second e)))
       vstack] ; (the literal is placed on the vstack by the first cond clause)
      ; functions:
      (not (keyword? (first e))) ; -> assume it's a function
      [(-> es
           (conj [:APPLY {}])
           (conj (first e))
           (into (rest e)))
       vstack]
      ; non-expression stack instructions:
      (keyword? (first e))
      (let [type (first e)
            params (second e)
            [v1 v2 & vss] vstack]
        (case type
          :SWAP [es
                 (conj vss
                       v1
                       v2)]
          :POP [es
                vs]
          :VAR-LOOKUP [es
                       (conj vstack
                             (search-envs vstack (params :var)))]
          :TEST (if v ; -> assume there's a boolean on top of vstack
                  [(conj es (params :consequent))
                   vs]
                  [(conj es (params :alternate))
                   vs])
          :DEFINE [es
                   (conj vs
                         (environment [(params :vname)]
                                      [v])
                         v)] ; put value on stack - useful
          :PLUS [es
                 (conj vss (+ v1 v2))]
          :EQUALS [es
                   (conj vss (= v1 v2))]
          :CONS [es
                 (conj vss (cons-pair v1 v2))]
          :FIRST [es
                  (conj vs (v :car))]
          :REST [es
                 (conj vs (v :cdr))]
          :APPLY ; vstack contents: (func arg1 arg2 ... argn [rest of stack])
          [(conj es
                 [:POP {}] ; remove environment
                 [:SWAP {}] ; switch order of env and function return value
                 (v :body-exp)) ; only 1-expression functions!
           (let [argnum (count (v :args))
                 argvals (take argnum vs)]
             (conj (->> vs ; vs = (arg1 arg2 ... argn [rest of stack])
                        (drop argnum))
                   (merge (v :env)
                          (environment (v :args)
                                       argvals))))])))))

(defn driver
  [estack vstack]
  (if (empty? estack)
    [(first vstack) (rest vstack)]
    (let [[estack2 vstack2]
          (step estack vstack)]
      (println "---")
      (println estack)
      (println vstack)
      (println "---")
      (driver estack2 vstack2))))

(defn eval-exp-list
  ; This function is what the REPL in core/repl hooks to
  [exps env]
  (driver (into '() exps)
          (if (map? env) ; for compatibility with the repl function in core.clj
            (list (environment (keys env) (vals env)))
            env)))
