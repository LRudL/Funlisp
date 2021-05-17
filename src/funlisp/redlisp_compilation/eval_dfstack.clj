(ns funlisp.redlisp-compilation.eval-dfstack
  (:gen-class))

;; eval_df, but interpreting the continuations as
;; conses of a list type, so we get a list = stack instead of
;; a nested mess of conses

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
  ([exps env cnt-stack]
   (cond
     (empty? exps) (apply-cnt cnt-stack ['nil env])
     (empty? (rest exps)) (eval-exp (first exps) env cnt-stack)
     true (eval-exp-list (rest exps)
                         (second ; <- this selects the new environment
                                 (eval-exp (first exps) env cnt-stack))
                         cnt-stack)))
  ([exps env] ; -> this is what the REPL uses
              (eval-exp-list exps env '())))
; ^ important: stack must be initialised with empty list, not vector
; (because we want conj to push to front, not back, so we can take
;  rest in O(1) with the rest function.)

(defn apply-cnt
  [cnt-stack [exp env]]
  (println (map first cnt-stack)) ; prints the stack labels
  (if (empty? cnt-stack) ; equivalent to (cnt :type) = :ID in eval_df
    [exp env]
    (let [type (first (first cnt-stack))
          params (second (first cnt-stack))]
      (case type
        :VAR-LOOKUP (apply-cnt (rest cnt-stack)
                               [(env exp)
                                env])
        :IF (if exp
              (eval-exp (params :consequent) env cnt-stack)
              (eval-exp (params :alternate)  env cnt-stack))
        :DEF (apply-cnt (rest cnt-stack)
                        ['nil
                         (assoc env (params :vname) exp)])
        :OUTER+ (eval-exp (params :e2) env
                          (conj (rest cnt-stack)
                                [:INNER+
                                 {:v1 exp}]))
        :INNER+ (apply-cnt (rest cnt-stack)
                           [(+ (params :v1) exp)
                            env])
        :OUTER= (eval-exp (params :e2) env
                          (conj (rest cnt-stack)
                                [:INNER=
                                 {:v1 exp}]))
        :INNER= (apply-cnt (rest cnt-stack)
                           [(= (params :v1) exp)
                            env])
        :OUTER-CONS (eval-exp (params :e2) env
                              (conj (rest cnt-stack)
                                    [:INNER-CONS
                                     {:v1 exp}]))
        :INNER-CONS (apply-cnt (rest cnt-stack)
                               [(cons-pair (params :v1) exp)
                                env])
        :CAR (apply-cnt (rest cnt-stack)
                        [(exp :car)
                         env])
        :CDR (apply-cnt (rest cnt-stack)
                        [(exp :cdr)
                         env])
        :FUNC (apply-cnt (conj (rest cnt-stack)
                               [:FUNC-ARGS
                                {:raw-args (params :raw-args)
                                 :args []
                                 :lambda exp}])
                         [exp env])
        :FUNC-ARGS (if (empty? (params :raw-args))
                     (apply-cnt (conj (rest cnt-stack)
                                      [:APPLY-FN
                                       {:args (params :args)
                                        :lambda (params :lambda)}])
                                [exp env])
                     (eval-exp (first (params :raw-args)) env
                               (conj (rest cnt-stack)
                                     [:FUNC-ARG-APPEND
                                      {:raw-args (rest (params :raw-args))
                                       :args (params :args)
                                       :lambda (params :lambda)}])))
        :FUNC-ARG-APPEND (apply-cnt (conj (rest cnt-stack)
                                          [:FUNC-ARGS
                                           {:raw-args (params :raw-args)
                                           :args (conj (params :args)
                                                       exp)
                                           :lambda (params :lambda)}])
                                  [exp env])
        :APPLY-FN (eval-exp-list ((params :lambda) :body)
                                 (merge env
                                        ((params :lambda) :env)
                                        (zipmap ((params :lambda) :args)
                                                (params :args)))
                                 (rest cnt-stack))))))

(defn eval-exp
  "Returns [value new-environment], where value is the value of evaluating
  expression exp in environment env, "
  [exp env cnt-stack]
  (cond
    ; true/false/nil
    (or (true? exp) (false? exp) (nil? exp))
    (apply-cnt cnt-stack [exp env])
    ; number
    (number? exp)
    (apply-cnt cnt-stack [exp env])
    ; symbol -> assume it's a variable and return its value:
    (symbol? exp)
    (apply-cnt (conj cnt-stack
                     [:VAR-LOOKUP {}])
               [exp env])
    ; if:
    (starts-with? exp 'if)
    (eval-exp (second exp) env
              (conj cnt-stack
                    [:IF {:consequent (third exp)
                          :alternate (fourth exp)}]))
    ; def
    (starts-with? exp 'def)
    (eval-exp (third exp) env
              (conj cnt-stack
                    [:DEF {:vname (second exp)}]))
    ; +
    (starts-with? exp '+)
    (eval-exp (second exp) env
              (conj cnt-stack
                    [:OUTER+ {:e2 (third exp)}]))
    ; =
    (starts-with? exp '=)
    (eval-exp (second exp) env
              (conj cnt-stack
                    [:OUTER= {:e2 (third exp)}]))
    ; cons (add an element to the front of a list)
    (starts-with? exp 'cons)
    (eval-exp (second exp) env
              (conj cnt-stack
                    [:OUTER-CONS {:e2 (third exp)}]))
    ; first
    (starts-with? exp 'first)
    (eval-exp (second exp) env
              (conj cnt-stack [:CAR {}]))
    ; rest
    (starts-with? exp 'rest)
    (eval-exp (second exp) env
              (conj cnt-stack [:CDR {}]))
    ; lambda, for creating a function, e.g. (lambda (x y) (+ x y)) for x -> x+y
    (starts-with? exp 'lambda)
    (apply-cnt cnt-stack
               [(function (rest (rest exp)) ; function body, ((+ x y)) above
                          env ; store definition environment for use in closures
                          (second exp)) ; argument names, (x y) above
                env])
    :else ; functions
    (eval-exp (first exp) env
              (conj cnt-stack
                    [:FUNC {:raw-args (rest exp)}]))))
