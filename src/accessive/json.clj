(ns accessive.json
  "Fast extraction from JSON strings."

  (:require [clojure.string :as s]
            [potemkin :refer [def-map-type]]
            [let-else :refer [let?]]
            [criterium.core :refer :all]
            ))


;; All the macros except cget* assume that the current char array is called cs.


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn get-json-end ^long [^chars cs ^long i]) ; hinted version of declare


(defmacro cget*
  [cs i]
  `(char (aget ~cs ~i)))


(defmacro cget
  [i]
  `(cget* ~'cs ~i))


(defmacro ws
  "Skips ws."
  [i]
  `(loop [i# ~i]
     (if (case (cget i#)
           (\space \tab \newline \return \, \:) false
           true)
       i#
       (recur (inc i#)))))


(defmacro skip-to-array-end
  "Starts at the [."
  [i]
  `(loop [i# (ws (inc ~i))]
     (if (= (cget i#) \])
       (inc i#)
       (recur (ws (get-json-end ~'cs i#))))))


(defmacro skip-to-map-end
  "Starts at the {."
  [i]
  `(loop [i# (ws (inc ~i))]
     (if (= (cget i#) \})
       (inc i#)
       (recur (->> (get-json-end ~'cs i#) ; skip the key
                   (ws)
                   (get-json-end ~'cs)   ; skip the value
                   (ws))))))


(defmacro skip-to-string-end
  "Starts at the opening double quote."
  [i]
  `(loop [i# (inc ~i)
          escaped?# (boolean false)]
    (let [c# (cget i#)]
      (if (and (= c# \")
               (boolean (not escaped?#)))
        (inc i#)
        (recur (inc i#)
               (and (= c# \\)
                    (boolean (not escaped?#))))))))


(defn skip-to-string-end-with-compare
  "i starts at the opening double quote.
  k is a char array to compare the string at i with.
  len is the length of k (so we don't have to keep recomputing it).
  Regardless of the success or failure of the comparison,
  always skip to the end of the string.
  Returns the index after the string, negative if the match failed."
  ^long [^chars cs ^long i ^chars k ^long len]
  (loop [i (inc i)
         ki 0
         escaped? (boolean false)
         fail? (boolean false)]
    (let [c (cget i)]
      (if (and (= c \")
               (boolean (not escaped?)))

        (if (or fail?
                (boolean (not= ki len)))
          (- (inc i))
          (inc i))

        (recur (inc i)
               (inc ki)
               (and (= c \\)
                    (boolean (not escaped?)))
               (or fail?
                   (>= ki len)
                   (boolean (not= c (cget* k ki)))))))))


(defmacro skip-token
  "JSON only allows null, true, false and numbers.
  Starts at the second character of the token."
  [i]
  `(loop [i# ~i]
     (if (case (cget i#)
           (\space \tab \newline \return \, \] \}) true
           false)
       i#
       (recur (inc i#)))))


(defn get-json-end
  ^long [^chars cs ^long i]
  (case (cget i)
    \[ (skip-to-array-end i)
    \] -1
    \{ (skip-to-map-end i)
    \} -1
    \" (skip-to-string-end i)
    (skip-token (inc i))       ; must be null, true, false or a number
    ))


(defn get-nth
  "Starts at the opening char, but we're not certain that it is [."
  ^long [^chars cs ^long i ^long n]
  (if (= (cget i) \[)        ; there should be a [
    (loop [i (ws (inc i))
           n n]
      (if (zero? n)
        i
        (if (= (cget i) \]) ; there shouldn't be a ]
          -1
          (recur (ws (get-json-end cs i))
                 (dec n)))))
    -1))


(defn get-val-for-key
  "Starts at the opening char, but we're not certain that it is {.
  k is a byte array to compare to map keys."
  ^long [^chars cs ^long i ^chars k]
  (let [len (alength k)]
    (if (= (cget i) \{)       ; there should be a {
      (loop [i (ws (inc i))]
        (if (= (cget i) \}) ; there shouldn't be a }
          -1
          (let [i (skip-to-string-end-with-compare cs i k len) ; compare and skip the key
                ]
            ;; neg i indicates failed key match
            (if (pos? i)
              (ws i)
              (recur (ws (get-json-end cs (ws (- i)))) ; skip the value
                     )))))
      -1)))


(defmacro get-nth-or-val-for-key
  [cs i k]
  `(let [val-for-key#
        (fn [s#]
          (->> (s/escape s# {\" "\\\"" \\ "\\\\"})
               (.toCharArray)
               (get-val-for-key ~cs ~i)))]
    (long
     (cond
      (and (integer? ~k) (not (neg? ~k)))
      (get-nth ~cs ~i ~k)

      (string? ~k)
      (val-for-key# ~k)

      (keyword? ~k)
      (val-for-key# (name ~k))

      :default
      (throw (RuntimeException. "Keys must be indices, keywords or strings."))))))


(defn get-in-json*
  "i starts at the first non-ws char."
  [^chars cs ^long i ks not-found]
  (loop [i i
         ks ks]
    (if (empty? ks)
      (let [end (get-json-end cs i)]
        (if (neg? end)
          not-found
          (String. (java.util.Arrays/copyOfRange cs i end))))

      (let [k (first ks)
            i (get-nth-or-val-for-key cs i k)]
        (if (neg? i)
          not-found
          (recur (long (ws i)) (rest ks)))))))


(defn get-in-json
  "Analogous to get-in, but extracts a substring from a JSON in string form.
  Keys are indices, keywords or strings."
  ([json-str ks] (get-in-json json-str ks nil))

  ([^String json-str ks not-found]
     (let [cs (.toCharArray json-str)]
       (get-in-json* cs (ws 0) ks not-found))))


(defn get-tree-in-json*
  "i starts at the first non-ws char."
  [^chars cs ^long i tree read-fn]
  (let [f (fn [[k v]]
            (let [i (get-nth-or-val-for-key cs i k)]
              [k (when-not (neg? i)
                   (get-tree-in-json* cs i v read-fn))]))

        kv-pairs (seq tree)]

    (if (nil? kv-pairs)
      (let [end (get-json-end cs i)]
        (when-not (neg? end)
          (read-fn
           (String. (java.util.Arrays/copyOfRange cs i end)))))
      (into {} (map f kv-pairs)))))


(defn get-tree-in-json
  "Extract multiple substrings simultaneously from a JSON in string form.
  Keys are indices, keywords or strings.
  tree is a map of keys, whose values are maps of keys, etc., with
  nils or empty maps at the leaves.

  read-fn is a fn to apply to each extracted string, e.g. read-string or
  json/parse-string. read-fn defaults to identity.

  Subtrees are omitted when their keys are not found."
  ([json-str tree]
     (get-tree-in-json json-str tree identity))

  ([^String json-str tree read-fn]
     (let [cs (.toCharArray json-str)]
       (get-tree-in-json* cs (ws 0) tree read-fn))))


(def-map-type LazyMap [m]
  (get [_ k default-value]
    (if (contains? m k)
      (let [v (get m k)]
        (if (instance? clojure.lang.Delay v)
          @v
          v))
      default-value))
  (assoc [_ k v]
    (LazyMap. (assoc m k v)))
  (dissoc [_ k]
     (LazyMap. (dissoc m k)))
  (keys [_]
    (keys m)))


(defn get-lazy-tree-in-json*
  "i starts at the first non-ws char."
  [^chars cs i-delay tree read-fn]
  (let [f (fn [[k v]]
            (let [i-delay-2
                  (delay
                   (let? [^long i @i-delay
                          :is-not neg? :else i]
                     (get-nth-or-val-for-key cs i k)))]
              [k (get-lazy-tree-in-json* cs i-delay-2 v read-fn)]))

        kv-pairs (seq tree)]

    (if (nil? kv-pairs)
      (delay
        (let? [^long i @i-delay
               :is-not neg?
               end (get-json-end cs i)
               :is-not neg?]
          (read-fn
           (String. (java.util.Arrays/copyOfRange cs i end)))))

      (LazyMap. (into {} (map f kv-pairs))))))


(defn get-lazy-tree-in-json
  "Almost the same behavior as get-tree-in-json, but returns the tree
  of results immediately. Values in the tree are delays that are
  dereferenced automatically upon access. This potentially allows
  quicker access to some values extracted from very large JSONs.

  Unlike get-tree-in-json, a complete tree is always returned, even when
  keys are not found."
  ([json-str tree]
     (get-lazy-tree-in-json json-str tree identity))

  ([^String json-str tree read-fn]
     (let [cs (.toCharArray json-str)]
       (get-lazy-tree-in-json* cs (delay (ws 0)) tree read-fn))))
