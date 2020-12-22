(ns tech.thomas-sojka.org-parser-tree.main
  (:require [tech.thomas-sojka.org-parser-tree.core :as org-parse-tree]))

(defn main []
  (let [journal "* 2019
** Unordered
*** Sometime
**** Clojure Spec basics :LEARN:TUTORIAL:
** 2019-01 January
*** KW01
**** Build a snowman :SOCIAL:
**** Game night :SOCIAL:"]
    (->> journal
         org-parse-tree/parse-tree
         org-parse-tree/vizualize-tree)
    (->> journal
         org-parse-tree/parse-tree
         (org-parse-tree/store-tree "resources/journal.edn"))))
