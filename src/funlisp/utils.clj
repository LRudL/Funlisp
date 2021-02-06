(ns funlisp.utils
  (:gen-class))

  (defn match-expr
    [exp [pattern consequent]]
    `((= ~exp ~pattern) ~consequent))

  (defn adjacents-paired
    [coll]
    (if (empty? coll)
      '()
      (conj (adjacents-paired (rest (rest coll)))
            (list (first coll) (fnext coll)))))

  (defmacro match
    [exp & cases]
    `(cond
       ~@(apply concat
           (map (partial match-expr exp) ; note: exp evaluated many times
                (adjacents-paired cases)))))
