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

(defmethod post-transform :head-line [head-line]
  (transform-link head-line))

(let [journal "* KW01
** Build a [[https://en.wikipedia.org/wiki/Snowman][snowman]]"]
  (->> journal
       org-parse-tree/parse-tree
       (org-parse-tree/store-tree "resources/customizations.edn")))
