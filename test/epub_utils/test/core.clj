(ns epub-utils.test.core
  (:use [epub-utils.core]
        [clojure.test]))

(deftest test-subordinate-or-equal?
  (are [h1 h2 res] (= (subordinate-or-equal? h1 h2) res)
       :h1 :h2 true
       :h2 :h1 false
       :h1 :h3 false
       :h2 :h2 true))
