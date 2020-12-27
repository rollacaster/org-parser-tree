(ns tech.thomas-sojka.org-parser-tree.stratify
  (:require [clojure.zip :as z]))

(defmulti stratify (fn [_ {:keys [type]}] type))
(defmethod stratify :head-line [org-tree headline]
  (let [previous-level (:level (z/node org-tree))
        current-level (:level headline)]
    (cond
      (= previous-level current-level)
      (-> org-tree
          (z/insert-right (merge headline {:children []}))
          z/rightmost)
      :else
      (let [next (apply comp (repeat (+ (- previous-level current-level) (if (> previous-level current-level) 1 0))
                                     (if (> previous-level current-level) z/up z/down)))]
        (-> org-tree
            next
            (z/append-child (merge headline {:children []}))
            z/down
            z/rightmost)))))
(defn edit [org-tree & args] (apply z/edit org-tree update args))
(defmethod stratify :content-line [org-tree content-line]
  (edit org-tree :content str (:content content-line)))
(defmethod stratify :list-item-line [org-tree list-item-line]
  (edit org-tree :list (fn [list] (if (coll? list) (conj list (:list-item list-item-line)) [(:list-item list-item-line)]))))
(defmethod stratify :default [org-tree _] org-tree)

