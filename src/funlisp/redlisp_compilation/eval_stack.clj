(ns funlisp.redlisp-compilation.eval-stack
  (:gen-class))

;; eval_dualstack, but with the compilation step
;; (i.e., putting instructions into the instruction/expression stack)
;; happening before the abstract stack machine actually starts
;; doing calculations on the value stack.

;; Helper functions:
(defn starts-with? [l el] (= (first l) el)) ; does collection l start with el?
(defn third [coll] (nth coll 2)) ; third element in a collection
(defn fourth [coll] (nth coll 3)) ; ...
(defn function
  "Return the data structure we will use to represent a function with
    a certain body, environment, and argument names"
  [body env args]
  {:function? true
   :body body
   :env env
   :args args})
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

(defn compile
  [e] ; compile :: expression -> instructions
  ; move any literal from the E-stack to the V-stack:
  (cond
    (or (true? e) (false? e) (nil? e)   ; true/false/nil
        (number? e))                    ; numbers
    [[:PUSH {:val e}]]
    ; we have a variable first on the E-stack, so find its value:
    (symbol? e)
    [[:VAR-LOOKUP {:var e}]]
    ; test
    (not (coll? e))
    (throw (Exception. "Undefined non-coll in compile"))
    ; if:
    (starts-with? e 'if)
    (concat (compile (second e))
            [[:TEST {:consequent (compile (third e))
                     :alternate (compile (fourth e))}]])
    ; def:
    (starts-with? e 'def)
    (concat (compile (third e))
            [[:DEFINE {:vname (second e)}]])
    ; +:
    (starts-with? e '+)
    (concat (compile (third e))
            (compile (second e))
            [[:PLUS {}]])
    ; =:
    (starts-with? e '=)
    (concat (compile (third e))
            (compile (second e))
            [[:EQUALS {}]])
    ; cons:
    (starts-with? e 'cons)
    (concat (compile (third e))
            (compile (second e))
            [[:CONS {}]])
    ; first:
    (starts-with? e 'first)
    (concat (compile (second e))
            [[:FIRST {}]])
    ; rest:
    (starts-with? e 'rest)
    (concat (compile (second e))
            [[:REST {}]])
    ; lambda:
    (starts-with? e 'lambda)
    (concat [[:MAKE-LAMBDA
              {:args (second e)
               :code (concat (compile (first (rest (rest e)))) ; function body
                             [])}]])
    ; functions:
    true ; -> assume it's a function
    (concat (apply concat (map compile (rest e)))
            (compile (first e))
            [[:APPLY {}]
             [:SWAP {}]
             [:POP {}]])))

(defn step ; step function for an abstract stack machine
  [insts vstack]
  (let [[ins & inss] insts
        [v & vs] vstack
        [v1 v2 & vss] vstack
        type (first ins)
        params (second ins)]
    (case type
      :PUSH [inss
             (conj vstack (params :val))]
      :SWAP [inss
             (conj vss v1 v2)]
      :POP [inss vs]
      :VAR-LOOKUP [inss
                   (conj vstack
                         (search-envs vstack (params :var)))]
      :TEST (if v ; -> assume there's a boolean on top of vstack
              [(concat (params :consequent)
                       inss)
               vs]
              [(concat (params :alternate)
                       inss)
               vs])
      :DEFINE [inss
               (conj vs
                     (environment [(params :vname)]
                                  [v])
                     v)] ; put value on stack - useful
      :PLUS [inss
             (conj vss (+ v1 v2))]
      :EQUALS [inss
               (conj vss (= v1 v2))]
      :CONS [inss
             (conj vss (cons-pair v1 v2))]
      :FIRST [inss
              (conj vs (v :car))]
      :REST [inss
             (conj vs (v :cdr))]
      :MAKE-LAMBDA [inss
                    (conj vstack
                          (function (params :code)
                                    (vstack->env vstack)
                                    (params :args)))]
      :APPLY ; vstack contents: (lambda arg1 arg2 ... argn [rest of stack])
      [(concat (v :body)
               inss)
       (let [argnum (count (v :args))
             argvals (take argnum vs)]
         (conj (->> vs ; vs = (arg1 arg2 ... argn [rest of stack])
                    (drop argnum))
               (merge (v :env)
                      (environment (v :args)
                                   argvals))))])))

(defn driver
  [instructions vstack]
  (println "---")
  (println instructions)
  (println vstack)
  (if (empty? instructions)
    [(first vstack) (rest vstack)]
    ; Note: if exps is empty, then the above line will
    ;       pop things (generally envs) off the stack, so e.g. vars lost
    ; It's not a bug, it's a feature!!!
    (let [[instructions2 vstack2]
          (step instructions vstack)]
      (driver instructions2 vstack2))))

(defn eval-exp-list
  ; This function is what the REPL in core/repl hooks to
  [exps env]
  (let [instructions (apply concat (map compile exps))]
    (println "COMPILED TO:")
    (println instructions)
    (driver instructions
            (if (map? env) ; for compatibility with the repl function in core.clj
              (list (environment (keys env) (vals env)))
              env))))
