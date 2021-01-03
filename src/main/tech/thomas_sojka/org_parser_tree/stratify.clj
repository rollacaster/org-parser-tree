(ns tech.thomas-sojka.org-parser-tree.stratify
  (:require [clojure.string :as str]
            [clojure.zip :as z]))

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

(defmethod stratify :list-item-line [org-tree list-item-line]
  (edit org-tree :list (fn [list] (if (coll? list) (conj list (:list-item list-item-line)) [(:list-item list-item-line)]))))
(defn property [property-content-line]
  (let [[_ name value]
        (re-find
         #":(.*):\s(.*)"
         property-content-line)]
    (hash-map (keyword (str/trim name))
              (str/trim value))))
(defmethod stratify :content-line [org-tree {:keys [content]}]
  (if (:drawer (z/node org-tree))
    (z/replace org-tree (update-in (z/node org-tree) [:drawer :node-properties] merge (property content)))
    (edit org-tree :content str content)))
(defmethod stratify :drawer-begin-line [org-tree drawer-begin-line]
  (z/replace org-tree (assoc (z/node org-tree) :drawer {:name (:drawer drawer-begin-line)
                                                        :node-properties {}})))
(defmethod stratify :drawer-end-line [org-tree _]
  (let [node (z/node org-tree)]
    (z/replace org-tree (-> node
                            (assoc (:name (:drawer node)) (:node-properties (:drawer node)))
                            (dissoc :drawer)))))
(defmethod stratify :default [org-tree _] org-tree)

