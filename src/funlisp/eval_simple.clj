(ns funlisp.eval-simple
  (:gen-class))

(declare eval-exp)
(declare result)

(defn eval-if
  [pred consequent alternate env]
  (if (first (eval-exp pred env))
    (first (eval-exp consequent env))
    (first (eval-exp alternate env))))

(defn def-in-env
  [vname exp env]
  (assoc env
   vname
   (first (eval-exp exp env))))

(defn var-lookup [vname env] (env vname))

(defn function
  [body env arg-names]
  {:body body
   :env env
   :arg-names arg-names})

(defn eval-exp-list
  [exps env]
  (cond
    (empty? exps) 'nil
    (empty? (rest exps)) (first
                          (eval-exp (first exps)
                                    env))
    true (eval-exp-list (rest exps)
                        (second (eval-exp (first exps) env)))))

(defn apply-fn
  [func arg-vals env]
  (eval-exp-list
  (func :body)
  (merge (merge env (func :env))
            (zipmap (func :arg-names) arg-vals))))

(defn starts-with? [l el] (= (first l) el))
(defn third [coll] (nth coll 2))
(defn fourth [coll] (nth coll 3))

(defn eval-exp
  ; returns [expression new-environment]
  [exp env]
  (cond
    (string? exp) [exp env] ; string
    (number? exp) [exp env] ; num
    (symbol? exp) [(var-lookup exp env) env] ;symbol
    (starts-with? exp 'quote) [(second exp) env]; quotes
    ; if
    (starts-with? exp 'if)
    [(eval-if
      (second exp)
      (third exp)
      (fourth exp)
      env)
     env]
    ; def
    (starts-with? exp 'def)
    ['nil
     (def-in-env
       (second exp)
       (third exp)
       env)]
    ; +
    (starts-with? exp '+)
    [(+ (result (second exp) env)
        (result (third exp) env))
     env]
    ; =
    (starts-with? exp '=)
    [(= (result (second exp) env)
        (result (third exp) env))
     env]
    ; first
    (starts-with? exp 'first)
    [(result (second exp) env)
     env]
    ; rest
    (starts-with? exp 'rest)
    [(result (second exp) env)
     env]
    ; cons
    (starts-with? exp 'cons)
    [(conj (eval-exp (third exp) env)
           (eval-exp (second exp) env))
     env]
    ; print
    (starts-with? exp 'print)
    (do
      (println (result (second exp) env))
      ['nil env])
    ; lambda
    (starts-with? exp 'lambda)
    [(function (rest (rest exp)) env (second exp))
     env]
    ; function application
    true [(apply-fn
           (result (first exp) env)
           (map (fn [aexp] (result aexp env))
                (rest exp))
           env)
          env]))

(defn result
  [exp env]
  (first (eval-exp exp env)))
