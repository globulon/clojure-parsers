4(ns parsers.core)

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

(defn parse
  ([f ps] (parse f [] ps))
  ([f args ps]
   (if (empty? ps)
     (result (apply f args))
     (bind
       (first ps)
       (fn [x] (parse f (conj args x) (rest ps)))))))

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

(defn ++ [p q]
  (fn [input]
    (vec (concat (p input) (q input)))))

(def letter (++ lower upper))

(def alphanum (++ letter digit))

(def word
  (++
    (bind letter (fn [x]
      (bind word (fn [s]
        (result (vec (cons x s)))))))
    (result "")))

(defn many [parser]
  (++
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
  (parse to-number [(many digit)]))

(defn to-negative [_ y]
  (if (number? y)
    (- y)
    y))

(def negative
  (parse to-negative [(chr \-) natural]))

(def integer (++ negative natural))

(defn string[[head & tail]]
  (if (nil? head)
    (result "")
    (parse
      (comp vec cons)
      [(chr head) (string tail)])))

(defn sepby1[with-parser separator]
  (parse
    (fn [n ns] (into [] (cons n ns)))
    [with-parser
     (many (parse (fn [_ n] n) [separator with-parser]))]))

(defn bracket [open with-parser close]
  (parse
    (fn [_ content _] content)
    [open with-parser close]))

(def integers
  (parse
    (fn [_ x _] x )
    [
      (chr \[)
      (sepby1 integer (chr \,))
      (chr \])]))

