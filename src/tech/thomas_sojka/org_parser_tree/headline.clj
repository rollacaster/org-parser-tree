(ns tech.thomas-sojka.org-parser-tree.headline
  (:require [clojure.string :as str]))

(defn org-link->clj [headline]
  (if (str/starts-with? (:title headline) "[[")
    (let [[link description]
          (drop 1
                (re-find
                 (re-matcher
                  #"\[\[(.*)\]\[(.*)\]\]"
                  (:title headline))))]
      (-> headline
          (assoc :link link)
          (assoc :title description)))
    headline))

(defn get-people [{:keys [title] :as headline} tags]
  (if (and (some #{"SOCIAL"} tags) (str/includes? title "with"))
    (let [[title people] (str/split title #" with ")]
      (-> headline
          (assoc :title (str/replace title (str " with " people) ""))
          (assoc :people (set (map str/trim (str/split people #","))))))
    headline))

(defn parse [line]
  (let [[type & content] line]
    (case type
      :head-line (let [[[_ stars] [_ & title] [_ & tags]] content
                       title (str/join " " title)]
                   (-> {:title title :type type :stars stars :tags (set tags)}
                       org-link->clj
                       #_(get-people tags)))
      :content-line {:type type
                     :content (str/join " " (map str/trim content))}
      :list-item-line
      (let [[[_ counter] [_ suffix] [_ contents]] content]
        {:type type :list-item (str counter suffix " " contents)})
      line)))

