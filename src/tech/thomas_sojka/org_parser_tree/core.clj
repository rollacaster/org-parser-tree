(ns tech.thomas-sojka.org-parser-tree.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [org-parser.parser :as parser]
            [rhizome.viz :as viz]
            [tech.thomas-sojka.org-parser-tree.headline :as headline]))

(defn build-tree [headlines]
  (reduce
   (fn [org-tree headline]
     (let [{:keys [type]} headline]
       (case type
         :head-line
         (let [previous-level (count (:stars (z/node org-tree)))
               current-level (count (:stars headline))]
           (cond
             (= previous-level current-level)
             (z/insert-right org-tree headline)
             :else
             (let [next (apply comp (repeat (inc (- previous-level current-level))
                                            (if (> previous-level current-level) z/up z/down)))]
               (-> org-tree
                   next
                   (z/append-child (merge headline {:children []}))
                   z/down
                   z/rightmost))))
         :content-line
         (z/edit org-tree update :content str (:content headline))
         :list-item-line
         (z/edit org-tree update :list (fn [list] (if (coll? list) (conj list (:list-item headline)) [(:list-item headline)])))
         org-tree)))
   (z/zipper (comp sequential? :children)
             :children
             (fn [node children] (assoc node :children children))
             {:title "root" :stars "" :children []})
   headlines))

(defn store-tree [path data]
  (binding [*print-level* nil
            *print-length* nil]
    (io/make-parents path)
    (pp/pprint data (io/writer path))))

(defn vizualize-tree [root]
  (viz/view-tree (comp sequential? :children) :children
                 root
                 :node->descriptor (fn [n] {:label (:title n)})))

(defn parse-tree [org-file-string]
  (->> org-file-string
       parser/org
       (drop 1)
       (remove (fn [[type]] (= type :empty-line)))
       (map headline/parse)
       build-tree
       z/root))

(comment
  (let [journal "* 2019
** Unordered
*** Sometime
**** Clojure Spec basics :LEARN:TUTORIAL:
** 2019-01 January
*** KW01
**** Build a snowman :SOCIAL:
**** Game night :SOCIAL:"]
    (->> journal
         parse-tree
         vizualize-tree)
    (->> journal
         parse-tree
         (store-tree "resources/journal.edn"))))


