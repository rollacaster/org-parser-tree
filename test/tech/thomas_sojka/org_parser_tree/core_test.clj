(ns hello-word.core
  (:require [clojure.zip :as z]
            [tech.thomas-sojka.org-parser-tree.core :refer [parse-tree]]
            [tech.thomas-sojka.org-parser-tree.stratify :refer [stratify]]
            [tech.thomas-sojka.org-parser-tree.transform :refer [transform]]))

(defmethod transform :keyword-line [[_ [_ keyword-key] [_ keyword-value]]]
  {:type :keyword-line
   :key keyword-key
   :value keyword-value})

(defmethod stratify :keyword-line [org-tree {:keys [key value]}]
  (z/replace org-tree (assoc (z/node org-tree) :keywords {(keyword key) value})))

(parse-tree "#+TAGS: WORK(w)
* 2021")
