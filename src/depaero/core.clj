(ns depaero.core
  (:require
    [clojure.walk :as walk]
    [clojure.java.io :as io]
    [com.stuartsierra.dependency :as dep]
    [clojure.edn :as edn]))

(defmulti resolve-tag (fn [config tag value] tag))

(defmethod resolve-tag 'join
  [config tag value]
  (apply str value))

(defmethod resolve-tag 'int
  [config tag value]
  (Integer/parseInt value))

(defrecord Goat [tg value])

(defn tag
  [tg value]
  (->Goat tg value))

(defn goat?
  [x]
  (= (type x) Goat))

(defn goat-ref?
  [x]
  (= (:tg x) 'ref))

(defn pathify
  ([f x] (pathify f (f) x))
  ([f pk ox]
   (let [x (if (goat? ox) (:value ox) ox)]
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
         [pk (if (goat? ox) ox x)])
       [[pk x]]))))

(defn uber-get
  [m ks not-found]
  (loop [sentinel (Object.)
         m m
         ks (seq ks)]
    (if ks
      (let [m (cond
                ; (goat? (get m (first ks)))
                ; (get-in m [(first ks) :value] sentinel)
                (map? m)
                (get m (first ks) sentinel)
                (sequential? m)
                (nth m (first ks) sentinel))]
        (if (identical? sentinel m)
          not-found
          (recur sentinel m (next ks))))
      m)))

(defn ks->goat-ks
  [m ks]
  (reduce
    (fn [acc k]
      (if (goat? (try (uber-get m (conj acc k) nil)
                      (catch Throwable e
                        nil)))
        (-> acc
            (conj k)
            (conj :value))
        (conj acc k)))
    []
    ks))

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

(defn shorter-variations
  [xs]
  (loop [r '()
         ys (seq xs)]
    (if (seq ys)
      (recur (conj r ys)
             (butlast ys))
      r)))

(defn ref-dependency
  [ref*]
  (:value ref*))

(defn filter-refs
  [paths]
  (sequence (comp
              (filter (comp goat? second))
              (filter (comp goat-ref? second)))
            paths))

(defn ref-dependencies
  "Recursively checks a ref for nested dependencies"
  [ref*]
  (let [nested-deps (sequence
                      (comp (filter goat?)
                            (filter goat-ref?)
                            (map :value))
                      (:value ref*))]
    (concat
      (when-some [r (ref-dependency ref*)] [r])
      (when (seq nested-deps)
        (concat nested-deps
                (->> nested-deps
                     (mapcat ref-dependencies)))))))

(defn shorter-depedencies
  "Takes a key path and returns the dependencies that are implicit due to key
  path.

  e.g. in {:f {:a {:b 1}} [:f] depends on [:f :a] and [:f :a :b]"
  ;; TODO: need this also return that [:f :a] depends on [:f :a :b] ?
  [k]
  (butlast (sort-by count (shorter-variations k))))

(defn config->ref-graph
  [config]
  (reduce
    (fn [graph [k v]]
      (as-> graph %
        (reduce (fn [acc sk] (dep/depend acc sk k)) % (shorter-depedencies k))
        (reduce (fn [acc d] (dep/depend acc k d)) % (ref-dependencies v))))
    (dep/graph)
    (filter-refs (pathify conj [] config))))

(defn resolve-refs
  "Resolves refs & any necessary tags in order to do it's job"
  [config]
  (reduce
    (fn [acc ks]
      (let [gks (map
                  (fn [x]
                    ;; there's a sub-ref here, it should already be a value
                    ;; due to recursive deps being resolved for us
                    (if (goat-ref? x)
                      (uber-get acc (ref-dependency x) nil)
                      x))
                  (let [gks (ks->goat-ks acc ks)]
                    (if (= (last gks) :value)
                      (butlast gks)
                      gks)))
            b (uber-get acc gks nil)]
        (cond
          (and (goat? b) (goat-ref? b))
          (uber-assoc-in acc gks (uber-get acc (ks->goat-ks acc (ref-dependency b)) nil))
          (goat? b)
          (uber-assoc-in acc gks (resolve-tag acc (:tg b) (:value b)))
          :else ;; nothing to do, resolved value already (except with a nested goat?)
          acc)))
    config
    (dep/topo-sort (config->ref-graph config))))

(defn resolve-tags
  [config]
  (walk/postwalk
    (fn [x]
      (if (goat? x)
        (resolve-tag config (:tg x) (:value x))
        x))
    config))

(comment
  (clojure.pprint/pprint
    (-> (edn/read
          {:eof nil :default tag}
          (-> "foo.edn" io/reader clojure.lang.LineNumberingPushbackReader.))
        (resolve-refs)
        (resolve-tags))))
