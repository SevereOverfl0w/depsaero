(ns
  depaero.profile
  "Resolve the #profile tag, but leave others intact, useful when hiding data
  (re-encoding) & preventing resolution of inner tags early"
  (:require
    [clojure.walk :as walk]
    [depaero.tagify :as tagify]))

(defn resolve-profile
  [tagged-config profile]
  (walk/postwalk
    (fn [x]
      (if (tagify/tag-wrapper-of? x 'profile)
        (get (:value x) profile)
        x))
    tagged-config))
