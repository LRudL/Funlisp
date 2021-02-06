(ns funlisp.parser
  (:gen-class)
  (:require [clojure.string :refer [join]]
            [clojure.set :refer [union]]
            [funlisp.utils :refer [match]]))

(defn in?
  [list element]
  (cond
    (empty? list) false
    (= (first list) element) true
    :else (recur (rest list) element)))


(def whitespace?
  (partial in? (list \tab \space \newline \return)))

(def symchar?
  (partial
   in?
   (concat (seq "abcdefghijklmnopqrstuvwxyz")
           (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
           (seq "1234567890_-+/*.?!<>#%^&$:"))))

(def numchar?
  (partial in? (seq "1234567890")))

(def opening-par? (partial in? (seq "([")))
(def closing-par? (partial in? (seq ")]")))

(defn consumer-parser
  [take? transform-func]
  (fn [partial-parse]
    (let [cseq (partial-parse :chars)]
      (if (take? (first cseq))
        (merge partial-parse
               {:chars (drop-while take? cseq)
                :parsed (transform-func
                         (take-while take? cseq)
                         (partial-parse :parsed))})
        false))))




(defn report-error [& strs] (throw (Throwable. (join " " strs))))


(def whitespace-char #{\tab \space \newline \return})
(def digit-char
  (set "0123456789"))
(def minus-char #{\-})
(def dot-char #{\. \,})
(def quote-char #{\"})
(def special-char #{\! \$ \% \^ \& \* \# \- \.})
(def letter-char
  (set "abcdefghijklmnoprqstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def symbol-char
  (union letter-char digit-char special-char))
(def non-quote-char
  (union symbol-char whitespace-char))

(defn empty-fn [x] '())
(defn id-fn [x] x)
(defn chop-heads [sq] (butlast (rest sq)))

(comment

 (parser Lisp
   Whitespace = (some whitespace-char)
   -> empty-fn |
   Number = (maybe minus-char) (some digit-char) (maybe dot-char (some digit-char))
   -> (fn [chars] (read-string (apply str chars))) |
   String = quote-char (some non-quote-char) quote-char
   -> (fn [chars] (apply str (chop-heads chars))) |
   Symbol = letter (maybe (some symchar))
   -> symbol |
   Expression = (or List Symbol String Number)
   -> id-fn |
   List = open-par (some (or Expression List)) close-par
   -> (fn [elms] (apply list (chop-heads elms))) |
   Lisp = (seq (or Expression List))
   )

 )

(defmacro parser
  [id & spec])

(defn multitaker
  [breakers seq]
  ; note: trouble if seq starts with a breaker
  (take-nth 2 (partition-by breakers seq)))

(defn picker
  [locs]
  (fn [struct] (map (partial nth struct) locs)))

(def implicit-kws #{'some 'maybe})
(def non-implicit-kws #{'seq 'or})
(def all-struct-kws (union implicit-kws non-implicit-kws))

(defn add-implicit-seqs
  [structure]
  (let [transducer
        (map (fn [comp]
               (if (list? comp)
                 (add-implicit-seqs comp)
                 comp)))]
    (cond
      (non-implicit-kws (first structure))
      (conj (sequence transducer (rest structure))
            (first structure))
      (implicit-kws (first structure))
      (conj (list
             (conj (sequence transducer (rest structure))
                   'seq))
            (first structure))
      :else
      (conj (sequence transducer structure)
            'seq))))

(defn eval-non-kws
  [structure]
  (map (fn [comp]
         (if (seq? comp)
           (eval-non-kws comp)
           (if (or (all-struct-kws comp)
                   (not (symbol? comp)))
             comp
             (eval comp))))
       structure))

(defn parse-grammar-specs
  [specs]
  (if (empty? specs)
    nil
    (let [[name structure fn-spec] (multitaker #{'= '->} (first specs))]
      (merge
       {(first name) {:structure (add-implicit-seqs structure)
                      :processor (eval (first fn-spec))}}
       (parse-grammar-specs (rest specs))))))

(defn parse-grammar-spec
  [speclist]
  (parse-grammar-specs
    (multitaker #{'|} speclist)))

(def first-true (partial some (fn [x] x)))

(defn first-true-or-f
  [l]
  (let [res (first-true l)]
    (if (= res nil) false res)))


(declare
 match-or-struct match-some-struct match-maybe-struct match-seq-struct)

(defn match-struct
  [struct-specs [cseq parsed] struct]
  (cond
    (list? struct) ; -> we have a non-atomic struct
    (let [args [struct-specs [cseq parsed] struct]]
      (match (first struct)
             'or (apply match-or-struct args)
             'some (apply match-some-struct args)
             'maybe (apply match-maybe-struct args)
             'seq (apply match-seq-struct args)))
    (and (symbol? struct) ; -> struct is name for something in struct-specs
         (not (nil? (struct-specs struct))))
    (match-struct struct-specs [cseq parsed] (struct-specs struct))
    (set? struct) ; -> struct is a set of characters
    (if (struct (first cseq))
      [(rest cseq) (conj parsed (first cseq))]
      false)
    :else ; -> assume it's a variable symbol that can be evalled into a set
    (match-struct struct-specs [cseq parsed] (eval struct))))

(defn match-or-struct
  [spec [cseq parsed] struct]
  (first-true-or-f
   (map (partial match-struct spec [cseq parsed])
        (rest struct))))

(defn match-some-struct
  [spec [cseq parsed] struct]
  (or
   (let [res (match-struct spec [cseq parsed] (second struct))]
     (if res
       (match-struct res struct)
       false))
   (if (empty? parsed) false [cseq parsed])))

(defn match-maybe-struct
  [spec [cseq parsed] struct]
  (or
   (match-struct spec [cseq parsed] (second struct))
   [cseq parsed]))

(defn match-seq-struct
  [spec [cseq parsed] struct]
  (if (empty? (rest struct))
    [cseq parsed]
    (let [[id comp & rest-comps] struct
          res (match-struct [cseq parsed] comp)]
      (if res
        (match-struct spec res (conj rest-comps id))
        false))))

(defn parse-with-struct
  [string struct process-fn]
  (let [res (match-struct [(seq string) '()] struct)]
    (if res
      [(first res) (process-fn (reverse (second res)))]
      false)))








(comment
; parse-grammar-spec should create something like:
 #{{:name Number
    :structure [(maybe -) (some digit) (maybe dot digit)]
    :process-fn read-string
    }
   {:name List
    :structure [open-par (some Expression) close-par]
    :process-fn list
    }})








(comment

(defn pp [txt] [(seq txt) '()])

(defmacro get-char-parser
  [set]
  `(fn [[cseq# acc#]]
     (if (empty? cseq#)
       false
       (if (~set (first cseq#))
         [(rest cseq#) (conj acc# (first cseq#))]
         false))))

(defmacro get-some-parser
  [parser]
  `(fn [partial-parse#]
     ((fn [partial-parse# n#]
        (let [res# (~parser partial-parse#)]
          (if res#
            (recur res# (+ n# 1))
            (if (> n# 0)
              partial-parse#
              false))))
      partial-parse#
      0)))

(defmacro get-seq-parser
  [[seq-label & ]]
  `(fn [[cseq# acc#]]
     (if (empty? ~seq-comps)
       acc#
       (let [fres#
             (get-char-parser ~(first seq-comps))]
         (if fres#
           (get-seq-parser
            (concat [~seq-label]
                    ~(rest seq-comps)))
           false)))))


(def whitespace-parse
  (consumer-parser whitespace?
                   (fn [taken acc] acc)))

(defn seq-to-str [char-seq] (apply str char-seq))

(def symbol-parse
  (consumer-parser
   symchar?
   (fn [taken acc]
     (conj acc
           (symbol (seq-to-str taken))))))

(defn try-parsers
  [parsers partial-parse]
  (let [res ((first parsers) partial-parse)]
    (cond
      res res
      (empty? (rest parsers)) false
      :else (recur (rest parsers) partial-parse))))

(declare lisp-parse)

(defn list-parse
  [partial-parse]
  (let [cseq (partial-parse :chars)
        parsed (partial-parse :parsed)]
    (cond
      (opening-par? (first cseq))

      :else false)))

(defn exp-parse
  [partial-parse]
  (let [cseq (partial-parse :chars)]
    (cond
      (empty? cseq) partial-parse
      (closing-par? (first cseq)) partial-parse
      :else (let [next-pp
                 (try-parsers (list
                               whitespace-parse
                               symbol-parse
                               list-parse)
                              partial-parse)]
             (if next-pp
               (lisp-parse next-pp)
               (throw
                 (Exception. "Failed to parse")))))))


(declare rec-reverse)

(defn pp-of-text
  [text]
  {:chars (seq text)
   :parsed '()})

(defn parse
  [text]
  (rec-reverse
   ((lisp-parse (pp-of-text text)) :parsed)))

(defn rec-reverse
  [nested-list]
  (reverse (map (fn [el]
                  (if (coll? el)
                    (rec-reverse el)
                    el))
                nested-list))))
