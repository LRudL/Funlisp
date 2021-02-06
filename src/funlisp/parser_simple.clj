(ns funlisp.parser-simple
  (:gen-class)
  (:require [clojure.string :refer [split]]))

;; One of the simplest possible parsers.

(defn list-to-str [l] (apply str l)) ; hack for converting a char list to a str

(defn separate-chars
  "Separates the characters in sep-char-set in the character sequence cseq;
  acc is an accumulator (initialize to an empty vector []), set str-mode
  to false to denote starting outside a string."
  [sep-char-set cseq acc str-mode]
  (let [next-str-mode ; figure out whether or not to toggle str-mode
        (if (= (first cseq) \") ; do we see a quote character?
          (not str-mode)
          str-mode)]
    (cond
      ; if empty, return the accumulator as a string:
      (empty? cseq)
      (list-to-str acc)
      ; we're approaching a character to be separated; add spaces around it:
      (and (not str-mode) (sep-char-set (first cseq)))
      (separate-chars sep-char-set
                      (rest cseq)
                      (conj acc \space (first cseq) \space)
                      next-str-mode)
      :else ; for all other characters, do nothing:
      (separate-chars sep-char-set
                      (rest cseq)
                      (conj acc (first cseq))
                      next-str-mode))))

(defn process-token
  "Converts a string representing a token into the token's representation
  in the syntax tree."
  [token]
  (cond
    ; if it's a string, chop off the quote characters at the start and end:
    (= (first token) \")
    (list-to-str (butlast (rest token)))
    ; try to read as a number if it starts with a digit or minus:
    (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \-} (first token))
    (read-string token)
    ; parentheses conversion to symbols:
    (= token "(") '-LEFT-
    (= token ")") '-RIGHT-
    :else ; everything else is converted into a symbol:
    (symbol token)))

(defn tokenize
  "Takes a sequence of characters and converts it into a nested list of tokens."
  [cseq]
  (map process-token
       (filter
        (fn [tkn] (not (re-matches #"\s*" tkn))) ; filter out pure whitespace
        (split (separate-chars #{\( \)} cseq [] false) ; spaces around parens
               #"\s+")))) ; find tokens by splitting on whitespace

(defn check-parentheses
  "Takes a list of tokens and a starting paren depth (pass 0) and returns:
  true if the parentheses are balanced; false otherwise."
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
  "Takes a list of tokens and builds up a nested vector using -LEFT- and -RIGHT-
  (the token forms of parentheses)."
  [tokens acc]
  (if (empty? tokens)
    acc
    (cond
      ; If we have a (, then recursively get the entire list starting at it:
      (= (first tokens) '-LEFT-)
      (let [[remaining next-list] (build-syntax (rest tokens) [])]
        (build-syntax remaining (conj acc next-list)))
      ; If we have a ), we've hit the end of a recursive call from the above
      ; case, so return what we have so far
      (= (first tokens) '-RIGHT-)
      [(rest tokens) acc]
      :else ; Otherwise, add the token to the accumulator and keep iterating:
      (build-syntax (rest tokens) (conj acc (first tokens))))))

(defn parse
  "Takes something that can be converted to a string, and returns:
  false if the parentheses are unmatched;
  a nested vector of tokens."
  [string]
  (let [tokens (tokenize (str string))] ; take str so that can e.g. pass number
    (if (check-parentheses tokens 0)
      (build-syntax tokens [])
      false)))
