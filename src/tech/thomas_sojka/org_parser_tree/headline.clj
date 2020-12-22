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
          (assoc :people
           (into [] (map str/trim (str/split people #","))))))
    headline))

(defn parse [headline]
  (let [[type [_ stars] [key & title] [_ & tags]] headline]
    (if (= :title key)
      (let [title (str/join " " title)]
        (-> {:title title :type type :stars stars :tags tags}
            #_org-link->clj
            #_(get-people tags)))
      headline)))

