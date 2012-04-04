(ns parsers.core)

(def zero  (fn [s] []))

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
     (vec (mapcat
          (fn [[result remaining]] ((f result) remaining))
          results)))))

(defn satisfy? [predicate]
  (bind
    item
    (fn [input]
      (if (predicate input)
        (result input)
        zero ))))

(defn chr[c]
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
    (bind
      letter
      (fn [x]
        (bind word
          (fn [s]
            (result (vec (cons x s)))))))
    (result "")))
