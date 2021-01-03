(ns tech.thomas-sojka.org-parser-tree.stratify-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.zip :as z]
            [tech.thomas-sojka.org-parser-tree.core :refer [parse-tree]]
            [tech.thomas-sojka.org-parser-tree.stratify :refer [stratify]]))

(deftest stratify-test
  (testing "Concats list-items with \\n"
    (defmethod stratify :list-item-line [org-tree list-item-line]
      (z/edit org-tree update :content str (:list-item list-item-line) "\n"))
    (let [journal "
**** Learned new clojure tricks                                       :LEARN:
    After reading the Clojure style guide I learned:
    - Use sets as function
    - Use =list*= for nested cons
    - Use =Constructor.= instead of =new="]
      (is (= (parse-tree journal)
             {:children
              '({:tags #{"LEARN"},
                :type :head-line,
                :title "Learned new clojure tricks",
                :level 4,
                :children [],
                :content
                "After reading the Clojure style guide I learned:- Use sets as function- Use =list*= for nested cons- Use =Constructor.= instead of =new="}),
              :title "root",
              :level 0}))))
  (testing "Parses drawers"
    (is (= (parse-tree "
* Test
:PROPERTIES:
:language: en
:source:   npm
:created-at: [2017-06-18]
:END:")
           {:children
            '({:title "Test",
               :type :head-line,
               :level 1,
               :tags #{},
               :children [],
               :properties {:language "en", :source "npm", :created-at "[2017-06-18]"}}),
            :title "root",
            :level 0}))))
