(ns epub-utils.test.core
  (:use [epub-utils.core]
        [clojure.test]))

(deftest test-directly-inferior?
  (are [h1 h2 res] (= (directly-inferior? h1 h2) res)
       :h1 :h2 true
       :h2 :h1 false
       :h1 :h3 false))
