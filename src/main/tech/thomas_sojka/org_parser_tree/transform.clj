(ns tech.thomas-sojka.org-parser-tree.transform
  (:require [clojure.string :as str]))

(defmulti post-transform :type)
(defmethod post-transform :default [line] line)

(defmulti transform (fn [[type]] type))
(defmethod transform :head-line [[type & content]]
  (let [[[_ stars] [_ & title] [_ & tags]] content
        title (str/join " " title)]
    (post-transform {:title title :type type :level (count stars) :tags (set tags)})))
(defmethod transform :content-line [[type & content]]
  (post-transform {:type type :content (str/join " " (map str/trim content))}))
(defmethod transform :list-item-line [[type & content]]
  (let [[[_ counter] [_ suffix] [_ contents]] content]
    (post-transform {:type type :list-item (str counter suffix " " contents)})))
(defmethod transform :default [line] line)
(defmethod transform :drawer-begin-line [[_ [_ drawer-name]]]
  (if (= drawer-name "END")
    {:type :drawer-end-line}
    {:type :drawer-begin-line
     :drawer (keyword (str/lower-case drawer-name))}))
