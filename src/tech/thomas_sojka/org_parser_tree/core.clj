(ns tech.thomas-sojka.org-parser-tree.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [org-parser.parser :as parser]
            [rhizome.viz :as viz]
            [tech.thomas-sojka.org-parser-tree.headline :as headline]))

(defn build-tree [headlines]
  (reduce
   (fn [org-tree line]
     (let [{:keys [type]} line
           edit (fn [& args] (apply z/edit org-tree update args))]
       (case type
         :head-line
         (let [previous-level (:level (z/node org-tree))
               current-level (:level line)]
           (cond
             (= previous-level current-level)
             (-> org-tree
                 (z/insert-right line)
                 z/rightmost)
             :else
             (let [next (apply comp (repeat (inc (- previous-level current-level))
                                            (if (> previous-level current-level) z/up z/down)))]
               (-> org-tree
                   next
                   (z/append-child (merge line {:children []}))
                   z/down
                   z/rightmost))))
         :content-line
         (edit :content str (:content line))
         :list-item-line
         (edit :list (fn [list] (if (coll? list) (conj list (:list-item line)) [(:list-item line)])))
         org-tree)))
   (z/zipper (comp sequential? :children)
             :children
             (fn [node children] (assoc node :children children))
             {:title "root" :level 0 :children []})
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


