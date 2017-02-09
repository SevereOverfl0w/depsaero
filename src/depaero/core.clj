(ns depaero.core
  (:require
    [depaero.tagify :as tagify]
    [depaero.dependencies :as dependencies]
    [clojure.java.io :as io]
    [clojure.edn :as edn]))

(defn wrap-data-readers
  [m]
  (into {} (map (fn [[k v]] [k tagify/tag-wrapper]) m)))

(defn read-config
  [file]
  (-> (edn/read
        {:eof nil
         ;; temporarily rebind *data-readers* to our wrapper so we can do things
         ;; like: #db/id [#ref [:a]] and other funkage
         :readers  (wrap-data-readers (merge default-data-readers *data-readers*))
         :default tagify/tag-wrapper}
        (-> file io/reader clojure.lang.LineNumberingPushbackReader.))
      (dependencies/resolve-refs)
      (tagify/resolve-tags)))

(comment
  (clojure.pprint/pprint
    (read-config "foo.edn")))
