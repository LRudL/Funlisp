(ns funlisp.redlisp-compilation.eval-dfstep
  (:gen-class))

;; eval_dfstack, but with apply-cnt and eval-exp combined into one function.

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

(defn step
  [cnt-stack [exp env] tag] ; <- form of a step
  (case tag ; tag determines whether we simulate an eval or an apply step
    :eval (cond
            ; true/false/nil
            (or (true? exp) (false? exp) (nil? exp))
            [cnt-stack [exp env] :apply]
            ; number
            (number? exp)
            [cnt-stack [exp env] :apply]
            ; symbol -> assume it's a variable and return its value:
            (symbol? exp)
            [(conj cnt-stack
                   [:VAR-LOOKUP {}])
             [exp env]
             :apply]
            ; if:
            (starts-with? exp 'if)
            [(conj cnt-stack
                   [:IF {:consequent (third exp)
                         :alternate (fourth exp)}])
             [(second exp) env]
             :eval]
            ; def
            (starts-with? exp 'def)
            [(conj cnt-stack
                   [:DEF {:vname (second exp)}])
             [(third exp) env]
             :eval]
            ; +
            (starts-with? exp '+)
            [(conj cnt-stack
                   [:OUTER+ {:e2 (third exp)}])
             [(second exp) env]
             :eval]
            ; =
            (starts-with? exp '=)
            [(conj cnt-stack
                   [:OUTER= {:e2 (third exp)}])
             [(second exp) env]
             :eval]
            ; cons (add an element to the front of a list)
            (starts-with? exp 'cons)
            [(conj cnt-stack
                   [:OUTER-CONS {:e2 (third exp)}])
             [(second exp) env]
             :eval]
            ; first
            (starts-with? exp 'first)
            [(conj cnt-stack [:CAR {}])
             [(second exp) env]
             :eval]
            ; rest
            (starts-with? exp 'rest)
            [(conj cnt-stack [:CDR {}])
             [(second exp) env]
             :eval]
            ; lambda, for creating a function, e.g. (lambda (x y) (+ x y)) for x -> x+y
            (starts-with? exp 'lambda)
            [cnt-stack
             [(function (rest (rest exp)) ; function body, ((+ x y)) above
                        env ; store definition environment for use in closures
                        (second exp)) ; argument names, (x y) above
              env]
             :apply]
            :else ; functions
            [(conj cnt-stack
                   [:FUNC {:raw-args (rest exp)}])
             [(first exp) env]
             :eval])
    :apply (if (empty? cnt-stack) ; equivalent to (cnt :type) = :ID in eval_df
             [exp env]
             (let [type (first (first cnt-stack))
                   params (second (first cnt-stack))]
               (case type
                 :VAR-LOOKUP [(rest cnt-stack)
                              [(env exp) env]
                              :apply]
                 :IF (if exp
                       [cnt-stack [(params :consequent) env] :eval]
                       [cnt-stack [(params :alternate ) env] :eval])
                 :DEF [(rest cnt-stack)
                       ['nil (assoc env (params :vname) exp)]
                       :apply]
                 :OUTER+ [(conj (rest cnt-stack)
                                [:INNER+
                                 {:v1 exp}])
                          [(params :e2) env]
                          :eval]
                 :INNER+ [(rest cnt-stack)
                          [(+ (params :v1) exp)
                           env]
                          :apply]
                 :OUTER= [(conj (rest cnt-stack)
                                [:INNER=
                                 {:v1 exp}])
                          [(params :e2) env]
                          :eval]
                 :INNER= [(rest cnt-stack)
                          [(= (params :v1) exp)
                           env]
                          :apply]
                 :OUTER-CONS [(conj (rest cnt-stack)
                                    [:INNER-CONS
                                     {:v1 exp}])
                              [(params :e2) env]
                              :eval]
                 :INNER-CONS [(rest cnt-stack)
                              [(cons-pair (params :v1) exp)
                               env]
                              :apply]
                 :CAR [(rest cnt-stack)
                       [(exp :car) env]
                       :apply]
                 :CDR [(rest cnt-stack)
                       [(exp :cdr)
                        env]
                       :apply]
                 :FUNC [(conj (rest cnt-stack)
                              [:FUNC-ARGS
                               {:raw-args (params :raw-args)
                                :args []
                                :lambda exp}])
                        [exp env]
                        :apply]
                 :FUNC-ARGS (if (empty? (params :raw-args))
                              [(conj (rest cnt-stack)
                                     [:APPLY-FN
                                      {:args (params :args)
                                       :lambda (params :lambda)}])
                               [exp env]
                               :apply]
                              [(conj (rest cnt-stack)
                                     [:FUNC-ARG-APPEND
                                      {:raw-args (rest (params :raw-args))
                                       :args (params :args)
                                       :lambda (params :lambda)}])
                               [(first (params :raw-args)) env]
                               :eval])
                 :FUNC-ARG-APPEND [(conj (rest cnt-stack)
                                         [:FUNC-ARGS
                                          {:raw-args (params :raw-args)
                                           :args (conj (params :args)
                                                       exp)
                                           :lambda (params :lambda)}])
                                   [exp env]
                                   :apply]
                 :APPLY-FN
                 [(rest cnt-stack)
                  [(first ((params :lambda) :body)) ; note semantics change!!!
                   (merge env
                          ((params :lambda) :env)
                          (zipmap ((params :lambda) :args)
                                  (params :args)))]
                  :eval])))))

(defn driver
  [cnt-stack [exp env] tag]
  (if (and (empty? cnt-stack)
           (= tag :apply)) ; equivalent to reaching the ID continuation
    [exp env]
    (let [[next-stack [next-exp next-env] next-tag]
          (step cnt-stack [exp env] tag)]
      (println [cnt-stack [exp env] tag])
      (driver next-stack [next-exp next-env] next-tag))))

(defn eval-exp-list
  "Evaluates a list of expressions in sequence, with the start environment env,
   but with any changes to the environment made by one expression being passed
   on to the remaining expressions in the sequence. Returns a [value environment]
   pair like eval-exp."
  ([exps env cnt-stack]
   (cond
     (empty? exps) (driver cnt-stack
                           ['nil env]
                           :eval)
     (empty? (rest exps)) (driver cnt-stack
                                  [(first exps) env]
                                  :eval)
     true (eval-exp-list (rest exps)
                         (second ; <- this selects the new environment
                                 (driver cnt-stack
                                         [(first exps) env]
                                         :eval))
                         cnt-stack)))
  ([exps env] ; -> this is what the REPL hooks to
              (eval-exp-list exps env '())))
; ^ important: stack must be initialised with empty list, not vector
; (because we want conj to push to front, not back, so we can take
;  rest in O(1) with the rest function.)

;; TODO: the way environments work is wrong (also in previous files)
; for example:
; (def a 1)
; (def func (lambda (x) (def a 333)))
; (func)
; a
; > 333
