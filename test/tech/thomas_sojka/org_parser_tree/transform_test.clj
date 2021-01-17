(ns tech.thomas-sojka.org-parser-tree.transform-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tech.thomas-sojka.org-parser-tree.core :refer [parse-tree]]
            [tech.thomas-sojka.org-parser-tree.transform :refer [post-transform]]))

(deftest transform-test
  (testing "Add link transformation to headlines"
    (let [transform-link
          (fn[{:keys [title] :as headline}]
            (let [re-org-link #"\[\[(.*)\]\[(.*)\]\]"]
              (if (str/includes? title "[[")
                (let [[link description]
                      (drop 1 (re-find (re-matcher re-org-link title)))]
                  (-> headline
                      (assoc :link link)
                      (assoc :title (str/replace title re-org-link description))))
                headline)))
          journal "
* KW01
** Build a [[https://en.wikipedia.org/wiki/Snowman][snowman]]"]
      (defmethod post-transform :head-line [head-line]
        (transform-link head-line))
      (is (= (parse-tree journal)
             {:children
              '({:tags #{},
                 :type :head-line,
                 :title "KW01",
                 :level 1,
                 :children
                 ({:tags #{},
                   :type :head-line,
                   :title "Build a snowman",
                   :level 2,
                   :link "https://en.wikipedia.org/wiki/Snowman",
                   :children []})}),
              :title "root",
              :level 0})))))
