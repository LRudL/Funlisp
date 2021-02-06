(ns funlisp.parser-simple
  (:gen-class)
  (:require [clojure.string :refer [split]]))

(defn list-to-str [l] (apply str l))

(defn separate-chars
  [sep-char-set cseq acc str-mode]
  (let [next-str-mode
        (if (= (first cseq) \")
          (not str-mode)
          str-mode)]
    (cond
      (empty? cseq)
      (list-to-str acc)
      (and (not str-mode) (sep-char-set (first cseq)))
      (separate-chars sep-char-set
                      (rest cseq)
                      (conj acc \space (first cseq) \space)
                      next-str-mode)
      :else
      (separate-chars sep-char-set
                      (rest cseq)
                      (conj acc (first cseq))
                      next-str-mode))))

(defn process-token
  [token]
  (cond
    (= (first token) \")
    (list-to-str (butlast (rest token)))
    (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \-} (first token))
    (read-string token)
    (= token "(")
    '-LEFT-
    (= token ")")
    '-RIGHT-
    :else
    (symbol token)))

(defn tokenize
  [cseq]
  (map process-token
       (filter (fn [tkn] (not (re-matches #"\s*" tkn)))
               (split (separate-chars #{\( \)} cseq [] false) #"\s+"))))

(defn check-parentheses
  [tokens n]
  (cond
    (empty? tokens)
    (if (= n 0) true false)
    (< n 0)
    false
    (= (first tokens) '-LEFT-)
    (check-parentheses (rest tokens) (+ n 1))
    (= (first tokens) '-RIGHT-)
    (check-parentheses (rest tokens) (- n 1))
    :else
    (check-parentheses (rest tokens) n)))

(defn build-syntax
  [tokens acc]
  (if (empty? tokens)
    acc
    (cond
      (= (first tokens) '-LEFT-)
      (let [[remaining next-list] (build-syntax (rest tokens) [])]
        (build-syntax remaining (conj acc next-list)))
      (= (first tokens) '-RIGHT-)
      [(rest tokens) acc]
      :else
      (build-syntax (rest tokens) (conj acc (first tokens))))))

(defn parse
  [string]
  (let [tokens (tokenize (str string))]
    (if (check-parentheses tokens 0)
      (build-syntax tokens [])
      false)))
