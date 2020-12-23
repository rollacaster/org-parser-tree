(ns tech.thomas-sojka.org-parser-tree.main
  (:require [clojure.string :as str]
            [tech.thomas-sojka.org-parser-tree.core :as org-parse-tree]
            [tech.thomas-sojka.org-parser-tree.transform
             :refer
             [post-transform]]))

(defn transform-link [{:keys [title] :as headline}]
  (let [re-org-link #"\[\[(.*)\]\[(.*)\]\]"]
      (if (str/includes? title "[[")
        (let [[link description]
              (drop 1 (re-find (re-matcher re-org-link title)))]
          (-> headline
              (assoc :link link)
              (assoc :title (str/replace title re-org-link description))))
        headline)))

(defn transform-people [{:keys [title tags] :as headline}]
  (if (and (some #{"SOCIAL"} tags) (str/includes? title "with"))
    (let [[title people] (str/split title #" with ")]
      (-> headline
          (assoc :title (str/replace title (str " with " people) ""))
          (assoc :people (set (map str/trim (str/split people #","))))))
    headline))

(defmethod post-transform :head-line [head-line]
  ((comp transform-link transform-people) head-line))

(comment
  (let [journal "* KW01
** Build a [[https://en.wikipedia.org/wiki/Snowman][snowman]] with Joe, Jack, Jill :SOCIAL:
*** Game night :SOCIAL:"]
    (->> journal
         org-parse-tree/parse-tree
         (org-parse-tree/store-tree "resources/customizations.edn"))))
