(ns patterns.core)

(defprotocol Functor
  (fmap [source f]))

(defprotocol Monad
  (return [source data])
  (flat-map [source]))

(extend-protocol Functor
  clojure.lang.IPersistentCollection
  (fmap [source f]
    (into (empty source) (for [x source] (f x)))))

(extend-protocol Monad
  clojure.lang.IPersistentCollection

  (return [source data]
    (into (empty source) data))

  (flat-map [source f]
    (into (empty source)(mapcat f source))))



;(defn bind [m f]
;  (let [structure (fmap m f)
;        join]))