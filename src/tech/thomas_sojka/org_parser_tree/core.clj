(ns tech.thomas-sojka.org-parser-tree.core
  (:require [clojure.zip :as z]
            [org-parser.parser :as parser]
            [tech.thomas-sojka.org-parser-tree.stratify :refer [stratify]]
            [tech.thomas-sojka.org-parser-tree.transform :refer [transform]]))

(defn build-tree [lines]
  (reduce
   (fn [org-tree line]
     ((comp (partial stratify org-tree) transform) line))
   (z/zipper (comp sequential? :children)
             :children
             (fn [node children] (assoc node :children children))
             {:title "root" :level 0 :children []})
   lines))

(defn parse-tree [org-file-string]
  (->> org-file-string
       parser/org
       (drop 1)
       build-tree
       z/root))


