(ns parsers.core)

(def zero (fn [s] []))

(def item
  (fn [input]
    (if (empty? input)
      []
      [[(first input) (vec (rest input))]])))

(defn result [value]
  (fn [input] [[value input]]))

(defn bind [parser-a f]
  (fn [input]
    (let [results (parser-a input)]
      (into (empty results) (mapcat
             (fn [[result remaining]] ((f result) remaining))
             results)))))

(defmacro parse
  ([f p] `(bind ~p (fn [x#] (result (~f x#)))))
  ([f p & ps] `(bind ~p (fn [x#] (parse (partial ~f x#) ~@ps)))))

(defn satisfy? [predicate]
  (bind item (fn [input]
    (if (predicate input)
      (result input)
      zero))))

(defn chr [c]
  (satisfy? (fn [input] (= c input))))

(def digit (satisfy? (fn [c] (Character/isDigit c))))

(def lower (satisfy? (fn [c] (Character/isLowerCase c))))

(def upper (satisfy? (fn [c] (Character/isUpperCase c))))

(defn plus [p q]
  (fn [input]
    (vec (concat (p input) (q input)))))

(def letter (plus lower upper))

(def alphanum (plus letter digit))

(def word
  (plus
    (bind letter (fn [x]
      (bind word (fn [s]
        (result (vec (cons x s)))))))
    (result "")))

(defn many [parser]
  (plus
    (bind parser (fn [x]
      (bind (many parser) (fn [xs]
        (result (cons x xs))))))
    (result "")))

(defn to-digit [chr]
  (Character/digit chr 10))

(defn to-number [digits]
  (if (empty? digits)
    []
    (reduce (fn [acc val] (+ (* 10 acc) (to-digit val))) 0 digits)))

(def natural
  (parse to-number (many digit)))

(defn to-negative [_ y]
  (if (number? y) (- y)))

(def negative
  (parse to-negative (chr \-) natural))

(def integer (plus natural negative))


