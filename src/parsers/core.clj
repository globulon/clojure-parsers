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

(defn mbind
  ([f ps] (mbind f [] ps))
  ([f args ps]
   (if (empty? ps)
     (result (apply f args))
     (bind
       (first ps)
       (fn [x] (mbind f (conj args x) (rest ps)))))))

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

(def character (satisfy? (fn [c] (not= c \newline))))

(def space
  (satisfy? (fn [c] (or
                      (= \space c)
                      (= \newline c)
                      (= \tab c)))))

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

(defn many1 [parser]
    (bind parser (fn [x]
      (bind (many parser) (fn [xs]
        (result (cons x xs)))))))


(defn to-digit [chr]
  (Character/digit chr 10))

(defn to-number [digits]
  (if (empty? digits)
    []
    (read-string (apply str digits))))

(def nat
  (mbind to-number [(many digit)]))

(defn to-negative [_ y]
  (if (number? y)
    (- y)
    y))

(def negative
  (mbind to-negative [(chr \-) nat]))

(def integer (++ negative nat))

(defn string[[head & tail]]
  (if (nil? head)
    (result "")
    (mbind
      (comp vec cons)
      [(chr head) (string tail)])))

(defn sepby1[with-parser separator]
  (mbind
    (fn [n ns] (into [] (cons n ns)))
    [with-parser
     (many (mbind (fn [_ n] n) [separator with-parser]))]))

(defn bracket [open with-parser close]
  (mbind
    (fn [_ content _] content)
    [open with-parser close]))

(def integers
  (mbind
    (fn [_ x _] x )
    [
      (chr \[)
      (sepby1 integer (chr \,))
      (chr \])]))

;;repetition with meaningful operators

(def addop
  (++
    (mbind (fn [_] +) [(chr \+)])
    (mbind (fn [_] -) [(chr \-)])))

(defn chainl1 [parser operations]
  (mbind
    (fn [x fys] (reduce
                  (fn [acc [f y]]
                    (if (vector? y) acc (f acc y)))
                  x fys))
    [ parser
      (many (mbind vector [operations parser]))]))

(defn expr [input]
  (let [factor (++
                (bracket (chr \() expr (chr \)))
                nat)]
    ((chainl1 factor  addop) input)))

(def spaces (many space))

(def comments
  (mbind
    (fn [pre text] (apply str (concat pre text)))
    [(string ";;") (many character)]))

(def junk
  (mbind
    (fn [_ _] "")
    [spaces (++ comments (result ""))]))

(defn parse [with-parser]
  (mbind
    (fn [_ data] data)
    [junk with-parser]))

(defn token [with-parser]
  (mbind
    (fn [data _] data)
    [with-parser junk]))

(def natural (token nat))

