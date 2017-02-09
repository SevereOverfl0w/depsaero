(ns depaero.dependencies
  (:require
    [clojure.walk :as walk]
    [com.stuartsierra.dependency :as dep]
    [depaero.tagify :as tagify]))

(defn pathify
  "Return a list of tuples. Each tuple will be of a key-path (similar to what get-in takes) and the value at that point.
  NOTE: get-in will not necessarily work on the data structure as get-in doesn't work on lists.
  NOTE: Has a special case for tags (could this be removed?) (tag-kvs)
  NOTE: May have a limit due to recursion not being in the tail position."
  ([f x] (pathify f (f) x))
  ([f pk ox]
   (let [x (if (tagify/tag-wrapper? ox) (:value ox) ox)]
     (if-some [kvs (cond
                     (map? x)
                     x
                     (sequential? x)
                     (map-indexed vector x))]
       (conj
         (mapcat
           (fn [[k v]]
             (pathify f (f pk k) v))
           kvs)
         [pk (if (tagify/tag-wrapper? ox) ox x)])
       [[pk x]]))))

(defn shorter-variations
  "Given a sequence '(1 2 3) return a sequence of all shorter
  possible sequences: '((1 2) (1))"
  [xs]
  (loop [r '()
         ys (butlast (seq xs))]
    (if (seq ys)
      (recur (conj r ys)
             (butlast ys))
      r)))

(defn ref-dependency
  [ref*]
  (:value ref*))

(defn ref-dependencies
  "Recursively checks a ref for nested dependencies"
  [ref*]
  (let [nested-deps (sequence
                      (comp (filter tagify/tag-wrapper-of-ref?)
                            (map :value))
                      (:value ref*))]
    (concat
      (when-some [r (ref-dependency ref*)] [r])
      (when (seq nested-deps)
        (concat nested-deps
                (->> nested-deps
                     (mapcat ref-dependencies)))))))

(defn config->ref-graph
  [config]
  (reduce
    (fn [graph [k v]]
      (as-> graph %
        (reduce (fn [acc sk] (dep/depend acc sk k)) % (shorter-variations k))
        (reduce (fn [acc d] (dep/depend acc k d)) % (ref-dependencies v))))
    (dep/graph)
    (filter
      (comp tagify/tag-wrapper-of-ref? second)
      (pathify conj [] config))))

(defn- uber-get
  "Can handle vectors, sequences and maps"
  [m ks not-found]
  (loop [sentinel (Object.)
         m m
         ks (seq ks)]
    (if ks
      (let [m (cond
                (map? m)
                (get m (first ks) sentinel)
                (sequential? m)
                (nth m (first ks) sentinel))]
        (if (identical? sentinel m)
          not-found
          (recur sentinel m (next ks))))
      m)))

(defn uber-assoc-in
  "Works on maps, vectors & lists. Preserves type"
  [m [k & ks] v]
  (let [assocfn
        (cond
          (map? m)
          assoc
          (vector? m)
          assoc
          (sequential? m)
          (fn [m k v]
            (map-indexed
              (fn [[i iv]]
                (if (= i k)
                  v
                  iv))))
          :else
          (throw (ex-info "Unknown assocfn" {:m m :type (type m)})))]
    (if ks
      (assocfn m k (uber-assoc-in (uber-get m [k] nil) ks v))
      (assocfn m k v))))

(defn resolve-refs
  "Resolves refs & any necessary tags in order to do it's job"
  [config]
  (reduce
    (fn [acc ks]
      (let [gks (map
                  (fn [x]
                    ;; there's a sub-ref here, it should already be a value
                    ;; due to recursive deps being resolved for us
                    (if (tagify/tag-wrapper-of-ref? x)
                      (uber-get acc (ref-dependency x) nil)
                      x))
                  (let [gks (tagify/ks->tag-wrapper-ks acc ks uber-get)]
                    (if (= (last gks) :value)
                      (butlast gks)
                      gks)))
            b (uber-get acc gks nil)]
        (cond
          (tagify/tag-wrapper-of-ref? b)
          (uber-assoc-in acc gks (uber-get acc (tagify/ks->tag-wrapper-ks acc (ref-dependency b) uber-get) nil))
          (tagify/tag-wrapper? b)
          (uber-assoc-in acc gks (tagify/resolve-tag acc (:tag b) (:value b)))
          :else ;; nothing to do, resolved value already (except with a nested tagify/tag-wrapper?, or is it resolved?!)
          acc)))
    config
    (dep/topo-sort (config->ref-graph config))))
