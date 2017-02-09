(ns depaero.core
  (:require
    [depaero.tagify :as tagify]
    [depaero.dependencies :as dependencies]
    [clojure.java.io :as io]
    [clojure.edn :as edn]))

(defn read-config
  [file]
  (-> (edn/read
        {:eof nil :default tagify/tag-wrapper}
        (-> file io/reader clojure.lang.LineNumberingPushbackReader.))
      (dependencies/resolve-refs)
      (tagify/resolve-tags)))

(comment
  (clojure.pprint/pprint
    (read-config "foo.edn")))
