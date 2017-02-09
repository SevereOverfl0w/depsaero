(ns depaero.tagify
  (:require
    [clojure.walk :as walk]))

(defrecord TagWrapper
  [tag value])

(defn tag-wrapper
  "Call from :default of clojure.edn to wrap all tags it encounters & cannot handle itself.
   TODO: Anything special needed to support a mirror of dynamic var related to *tags*?"
  [tag value]
  (->TagWrapper tag value))

(defn tag-wrapper?
  [x]
  (= (type x) TagWrapper))

(defn tag-wrapper-of?
  [x tag]
  (and (tag-wrapper? x)
       (= (:tag x) tag)))

(defn tag-wrapper-of-ref?
  [x]
  (tag-wrapper-of? x 'ref))

(defn ks->tag-wrapper-ks
  "Given a map and a ks, add `:value` at locations of tag-wrappers"
  [m ks get-inner]
  (reduce
    (fn [acc k]
      (if (tag-wrapper? (try (get-inner m (conj acc k) nil)
                             (catch Throwable e
                               nil)))
        (-> acc
            (conj k)
            (conj :value))
        (conj acc k)))
    []
    ks))

(defmulti resolve-tag (fn [config tag value] tag))

(defmethod resolve-tag 'join
  [config tag value]
  (apply str value))

(defmethod resolve-tag 'int
  [config tag value]
  (Integer/parseInt value))

(defn resolve-tags
  "Resolve tags in a data structure, doesn't take notice of any ordering guarantees, this function should be called _after_ reoslving anything ordered, to fix up anything you didn't handle"
  [config]
  (walk/postwalk
    (fn [x]
      (if (tag-wrapper? x)
        (resolve-tag config (:tag x) (:value x))
        x))
    config))
