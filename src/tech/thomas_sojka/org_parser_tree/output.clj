(ns tech.thomas-sojka.org-parser-tree.output
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [rhizome.viz :as viz]))

(defn store-tree [path data]
  (binding [*print-level* nil
            *print-length* nil]
    (io/make-parents path)
    (pp/pprint data (io/writer path))))

(defn vizualize-tree [root]
  (viz/view-tree (comp sequential? :children) :children
                 root
                 :node->descriptor (fn [n] {:label (:title n)})))
