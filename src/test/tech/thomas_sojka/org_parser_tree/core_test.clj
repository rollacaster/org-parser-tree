(ns tech.thomas-sojka.org-parser-tree.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.zip :as z]
            [tech.thomas-sojka.org-parser-tree.core :as org-parse-tree]
            [tech.thomas-sojka.org-parser-tree.stratify :refer [stratify]]
            [tech.thomas-sojka.org-parser-tree.transform :refer [transform]]))

(deftest core-test
  (testing "add new implementation for :drawer-begin-line"
    (defmethod transform :drawer-begin-line [[_ [_ drawer-name]]]
      {:type :drawer-begin-line
       :drawer (keyword (str/lower-case drawer-name))})
    (defmethod stratify :drawer-begin-line [org-tree drawer-begin-line]
      (z/replace org-tree (assoc (z/node org-tree) (:drawer drawer-begin-line) [])))
    (is (= (org-parse-tree/parse-tree "
* Test
:MY-DRAWER:
:END:")
           {:children
            '({:tags #{},
              :type :head-line,
              :title "Test",
              :level 1,
              :children [],
              :my-drawer [],
              :end []}),
            :title "root",
            :level 0}))))

