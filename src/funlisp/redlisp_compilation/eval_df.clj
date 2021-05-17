(ns funlisp.redlisp-compilation.eval-df
  (:gen-class))

;; eval_cps, defunctionalised

; This will be our main evaluation function:
(declare eval-exp) ; note: returns a pair [result environment]
(declare apply-cnt)

;; Helper functions:
(defn starts-with? [l el] (= (first l) el)) ; does collection l start with el?
(defn third [coll] (nth coll 2)) ; third element in a collection
(defn fourth [coll] (nth coll 3)) ; ...
(defn function
  "Return the data structure we will use to represent a function with
    a certain body, environment, and argument names"
  [body env arg-names]
  {:function? true
   :body body
   :env env ; -> the environment the function was defined in
   ; (we store the environment so the language supports closures)
   :args arg-names})
(defn cons-pair
  "Return the data structure for a cons pair."
  [a b]
  {:cons? true
   :car a ; car/cdr are the standard Lisp terms
   :cdr b})

(defn eval-exp-list
  "Evaluates a list of expressions in sequence, with the start environment env,
  but with any changes to the environment made by one expression being passed
  on to the remaining expressions in the sequence. Returns a [value environment]
  pair like eval-exp."
  ([exps env cnt]
   (cond
     (empty? exps) (apply-cnt cnt ['nil env])
     (empty? (rest exps)) (eval-exp (first exps) env cnt)
     true (eval-exp-list (rest exps)
                         (second ; <- this selects the new environment
                                 (eval-exp (first exps) env cnt))
                         cnt)))
  ([exps env] ; -> this is what the REPL uses
              (eval-exp-list exps env {:type :ID})))

(defn apply-cnt
  [cnt [exp env]]
  (println "APPLY-CNT")
  (println cnt)
  (println exp)
  (println env)
  (println "---")
  (case (cnt :type)
    :ID [exp env]
    :VAR-LOOKUP (apply-cnt (cnt :cnt)
                           [(env exp) ; variable lookup
                            env])
    :IF (if exp
                (eval-exp (cnt :consequent) env (cnt :cnt))
                (eval-exp (cnt :alternate)  env (cnt :cnt)))
    :DEF (apply-cnt (cnt :cnt)
                    ['nil
                     (assoc env (cnt :vname) exp)])
    :OUTER+ (eval-exp (cnt :e2) env
                      {:type :INNER+
                       :cnt (cnt :cnt)
                       :v1 exp})
    :INNER+ (apply-cnt (cnt :cnt)
                       [(+ (cnt :v1) exp)
                        env])
    :OUTER= (eval-exp (cnt :e2) env
                      {:type :INNER=
                       :cnt (cnt :cnt)
                       :v1 exp})
    :INNER= (apply-cnt (cnt :cnt)
                       [(= (cnt :v1) exp)
                        env])
    :OUTER-CONS (eval-exp (cnt :e2) env
                          {:type :INNER-CONS
                           :cnt (cnt :cnt)
                           :v1 exp})
    :INNER-CONS (apply-cnt (cnt :cnt)
                           [(cons-pair (cnt :v1) exp)
                            env])
    :CAR (apply-cnt (cnt :cnt)
                    [(exp :car)
                     env])
    :CDR (apply-cnt (cnt :cnt)
                    [(exp :cdr)
                     env])
    :FUNC (apply-cnt {:type :FUNC-ARGS
                      :cnt (cnt :cnt)
                      :raw-args (cnt :raw-args)
                      :args []
                      :lambda exp}
                     [exp env])
    :FUNC-ARGS (if (empty? (cnt :raw-args))
                 (apply-cnt {:type :APPLY-FN
                             :cnt (cnt :cnt)
                             :args (cnt :args)
                             :lambda (cnt :lambda)}
                            [exp env])
                 (eval-exp (first (cnt :raw-args)) env
                           {:type :FUNC-ARG-APPEND
                            :cnt (cnt :cnt)
                            :raw-args (rest (cnt :raw-args))
                            :args (cnt :args)
                            :lambda (cnt :lambda)}))
    :FUNC-ARG-APPEND (apply-cnt {:type :FUNC-ARGS
                                 :cnt (cnt :cnt)
                                 :raw-args (cnt :raw-args)
                                 :args (conj (cnt :args)
                                             exp)
                                 :lambda (cnt :lambda)}
                                [exp env])
    :APPLY-FN (eval-exp-list ((cnt :lambda) :body)
                             (merge env
                                    ((cnt :lambda) :env)
                                    (zipmap ((cnt :lambda) :args)
                                            (cnt :args)))
                             (cnt :cnt))))

(defn eval-exp
  "Returns [value new-environment], where value is the value of evaluating
  expression exp in environment env, "
  [exp env cnt]
  (println "EVAL-EXP")
  (println exp)
  (println env)
  (println "---")
  (cond
    ; true/false/nil
    (or (true? exp) (false? exp) (nil? exp))
    (apply-cnt cnt [exp env])
    ; number
    (number? exp)
    (apply-cnt cnt [exp env])
    ; symbol -> assume it's a variable and return its value:
    (symbol? exp)
    (apply-cnt {:type :VAR-LOOKUP
                :cnt cnt}
               [exp env])
    ; if:
    (starts-with? exp 'if)
    (eval-exp (second exp) env
              {:type :IF
               :cnt cnt
               :consequent (third exp)
               :alternate (fourth exp)})
    ; def
    (starts-with? exp 'def)
    (eval-exp (third exp) env
              {:type :DEF
               :cnt cnt
               :vname (second exp)})
    ; +
    (starts-with? exp '+)
    (eval-exp (second exp) env
              {:type :OUTER+
               :cnt cnt
               :e2 (third exp)})
    ; =
    (starts-with? exp '=)
    (eval-exp (second exp) env
              {:type :OUTER=
               :cnt cnt
               :e2 (third exp)})
    ; cons (add an element to the front of a list)
    (starts-with? exp 'cons)
    (eval-exp (second exp) env
              {:type :OUTER-CONS
               :cnt cnt
               :e2 (third exp)})
    ; first
    (starts-with? exp 'first)
    (eval-exp (second exp) env
              {:type :CAR
               :cnt cnt})
    ; rest
    (starts-with? exp 'rest)
    (eval-exp (second exp) env
              {:type :CDR
               :cnt cnt})
    ; lambda, for creating a function, e.g. (lambda (x y) (+ x y)) for x -> x+y
    (starts-with? exp 'lambda)
    (apply-cnt cnt
               [(function (rest (rest exp)) ; function body, ((+ x y)) above
                          env ; store definition environment for use in closures
                          (second exp)) ; argument names, (x y) above
                env])
    :else ; functions
    (eval-exp (first exp) env
              {:type :FUNC
               :cnt cnt
               :raw-args (rest exp)})))
