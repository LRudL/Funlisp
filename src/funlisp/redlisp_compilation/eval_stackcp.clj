(ns funlisp.redlisp-compilation.eval-stackcp
  (:gen-class))

;; eval_stack, but with a "flat" instruction list and a code pointer on it,
;; with code pointer shenanigans replacing instruction list manipulation
;; (i.e. after compilation, the instruction list does not change)

(def label-atom (atom 0))
(defn get-label []
  (swap! label-atom inc))

;; Helper functions:
(defn starts-with? [l el] (= (first l) el)) ; does collection l start with el?
(defn third [coll] (nth coll 2)) ; third element in a collection
(defn fourth [coll] (nth coll 3)) ; ...
(defn function
  "Return the data structure we will use to represent a function with
    a certain body, environment, and argument names"
  [loc env args]
  {:function? true
   :loc-label loc
   :env env
   :args args})
(defn cons-pair
  "Return the data structure for a cons pair."
  [a b]
  {:cons? true
   :car a ; car/cdr are the standard Lisp terms
   :cdr b})
(defn concatv
  [& args]
  (vec (apply concat args)))

(defn return-address
  [loc]
  {:return-address? true
   :i loc})

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

(defn precompile
  [e] ; precompile :: expression -> (function definitions, instructions)
  ; move any literal from the E-stack to the V-stack:
  (cond
    (or (true? e) (false? e) (nil? e)   ; true/false/nil
        (number? e))                    ; numbers
    [[] [[:PUSH e]]]
    ; we have a variable first on the E-stack, so find its value:
    (symbol? e)
    [[] [[:VAR-LOOKUP e]]]
    ; test
    (not (coll? e))
    (throw (Exception. "Undefined non-coll in precompile"))
    ; do:
    (starts-with? e 'do)
    (let [precomps (map precompile (rest e))]
      [(apply concatv (map first precomps))
       (apply concatv (map second precomps))])
    ; if:
    (starts-with? e 'if)
    (let [[fdefs1 code-predicate] (precompile (second e))
          [fdefs2 code-consequent] (precompile (third e))
          [fdefs3 code-alternate] (precompile (fourth e))
          label-alt (get-label)
          label-after (get-label)]
      [(concatv fdefs1 fdefs2 fdefs3)
       (concatv code-predicate
               [[:TEST label-alt]]
               (code-consequent)
               [[:GOTO label-after]
                [:LABEL label-alt]]
               (code-alternate)
               [[:LABEL label-after]])])
    ; def:
    (starts-with? e 'def)
    (let [[fdefs vcode] (precompile (third e))]
      [fdefs
       (concatv vcode
               [[:DEFINE (second e)]])])
    ; +:
    (starts-with? e '+)
    (let [[fdefs1 c1] (precompile (second e))
          [fdefs2 c2] (precompile (third e))]
      [(concatv fdefs1 fdefs2)
       (concatv c1 c2 [[:PLUS]])])
    ; =:
    (starts-with? e '=)
    (let [[fdefs1 c1] (precompile (second e))
          [fdefs2 c2] (precompile (third e))]
      [(concatv fdefs1 fdefs2)
       (concatv c1 c2 [[:EQUALS]])])
    ; cons:
    (starts-with? e 'cons)
    (let [[fdefs1 c1] (precompile (second e))
          [fdefs2 c2] (precompile (third e))]
      [(concatv fdefs1 fdefs2)
       (concatv c1 c2 [[:CONS]])])
    ; first:
    (starts-with? e 'first)
    (let [[fdefs c] (precompile (second e))]
      [fdefs
       (concatv c [[:FIRST]])])
    ; rest:
    (starts-with? e 'rest)
    (let [[fdefs c] (precompile (second e))]
      [fdefs
       (concatv c [[:REST]])])
    ; lambda:
    (starts-with? e 'lambda)
    (let [label (get-label)
          [fdefs ce] (precompile (first (rest (rest e))))]
      [(concatv fdefs
               [[:LABEL label]]
               ce
               [[:RETURN]])
       [[:MAKE-LAMBDA
         label ; location of code
         (second e)]]]) ; arguments
    ; functions:
    true ; -> assume it's a function
    (let [[fun-fdefs fun-code] (precompile (first e))
          arg-precomps (map precompile (rest e))
          arg-fdefs (apply concatv (map first arg-precomps))
          arg-code (apply concatv (map second arg-precomps))]
      [(concatv fun-fdefs
               arg-fdefs)
       (concatv arg-code
               fun-code
               [[:APPLY]
                [:SWAP]
                [:POP]])])))

(defn label-mapping
  ([[inst & insts] i mp]
   (if (empty? inst)
     mp
     (recur
       insts
       (inc i)
       (if (= (first inst) :LABEL)
         (assoc mp (second inst) i)
         mp))))
  ([instructions]
   (label-mapping instructions 0 {})))

(defn compile [e]
  (let [[function-definitions main-code] (precompile e)
        code (concatv main-code
                     [[:HALT]]
                     function-definitions)]
    [code
     (label-mapping code)]))

(defn step ; step function for an abstract stack machine
  [label-lookup nth-ins cp vstack]
  (let [[v & vs] vstack
        [v1 v2 & vss] vstack
        [v1 v2 v3 & vsss] vstack
        ins (nth-ins cp)
        type (first ins)
        param (second ins)]
    (case type
      :PUSH [(inc cp)
             (conj vstack param)]
      :SWAP [(inc cp)
             (conj vss v1 v2)]
      :POP [(inc cp) vs]
      :VAR-LOOKUP [(inc cp)
                   (conj vstack
                         (search-envs vstack param))]
      :TEST (if v
              [(inc cp)
               vs]
              [(label-lookup param) ; = next code pointer
               vs])
      :DEFINE [(inc cp)
               (conj vs
                     (environment [param] ; = variable name
                                  [v])
                     v)] ; put value on stack - useful
      :PLUS [(inc cp)
             (conj vss (+ v1 v2))]
      :EQUALS [(inc cp)
               (conj vss (= v1 v2))]
      :CONS [(inc cp)
             (conj vss (cons-pair v1 v2))]
      :FIRST [(inc cp)
              (conj vs (v :car))]
      :REST [(inc cp)
             (conj vs (v :cdr))]
      :LABEL [(inc cp)
              vstack]
      :GOTO [(label-lookup param)
             vstack]
      :RETURN [(v3 :i)
               (conj vsss v1)] ; <- drop v2 (environment) and v3 (return addr.)
      :MAKE-LAMBDA [(inc cp)
                    (conj vstack
                          (function (second ins) ; = location of func code
                                    (vstack->env vstack)
                                    (third ins)))]
      :APPLY ; vstack contents: (lambda arg1 arg2 ... argn [rest of stack])
      [(label-lookup (v :loc-label)) ; location of the function code
       (let [argnum (count (v :args))
             argvals (take argnum vs)]
         (conj (->> vs ; vs = (arg1 arg2 ... argn [rest of stack])
                    (drop argnum))
               (return-address (inc cp))
               (merge (v :env)
                      (environment (v :args)
                                   argvals))))])))

(defn driver
  [c i label-map vstack]
  ; c is the instruction vector;
  ; because this is Clojure it's also a function from index -> element
  (println "---")
  (println (c i))
  (println vstack)
  (if (= (first (c i)) :HALT)
    [(first vstack) (rest vstack)]
    (let [[i2 vstack2]
          (step label-map c i vstack)]
      (driver c i2 label-map vstack2))))

(defn eval-exp-list
  ; This function is what the REPL in core/repl hooks to
  ; NOTE: chaining of environments together over many calls to
  ;       eval-exp-list works with simple values but not functions;
  ;       calling a function defined outside current eval-exp-list call
  ;       will break things. Write your REPL programs as a single line.
  [exps env]
  (let [master-exp (concat '(do) exps)
        [instructions label-map] (compile master-exp)]
    (println "COMPILED TO:")
    (println instructions)
    (driver instructions
            0
            label-map
            (if (map? env) ; for compatibility with the repl function in core.clj
              (list (environment (keys env) (vals env)))
              env))))
