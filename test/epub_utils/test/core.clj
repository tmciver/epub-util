(ns epub-utils.test.core
  (:use [epub-utils.core]
        [clojure.test]))

(deftest test-subordinate-or-equal?
  (are [h1 h2 res] (= (subordinate-or-equal? h1 h2) res)
       {:tag :h1} {:tag :h2} true
       {:tag :h2} {:tag :h1} false
       {:tag :h1} {:tag :h3} false
       {:tag :h2} {:tag :h2} true))

(deftest test-subordinate-heading?
  (are [h1 h2 res] (= (subordinate-heading? h1 h2) res)
       {:tag :h1} {:tag :h2} true
       {:tag :h2} {:tag :h1} false
       {:tag :h1} {:tag :h3} true
       {:tag :h2} {:tag :h2} false))

(deftest test-directly-subordinate-heading?
  (are [h1 h2 res] (= (directly-subordinate-heading? h1 h2) res)
       {:tag :h1} {:tag :h2} true
       {:tag :h3} {:tag :h4} true
       {:tag :h2} {:tag :h1} false
       {:tag :h1} {:tag :h3} false
       {:tag :h2} {:tag :h2} false))

(deftest test-get-top-level-sibling-headings
  (are [headings res] (= (get-top-level-sibling-headings headings) res)
       [{:tag :h1} {:tag :h2} {:tag :h3} {:tag :h2} {:tag :h1} {:tag :h2}]
       [{:tag :h1} {:tag :h1}]))

(deftest test-direct-child-headings
  (are [heading headings res] (= (direct-child-headings heading headings) res)
       {:tag :h2,
        :attrs {:class "calibre1"},
        :content ["4 Causes of Complexity"]}
       [{:tag :h2,
         :attrs {:class "calibre1"},
         :content ["4 Causes of Complexity"]}
        {:tag :h3,
         :attrs {:class "calibre1", :id "section-4.1"},
         :content ["4.1 Complexity caused by State"]}
        {:tag :h4,
         :attrs {:id "section-4.1.1", :class "calibre1"},
         :content ["4.1.1 Impact of State on\n      Testing"]}
        {:tag :h4,
         :attrs {:id "section-4.1.2", :class "calibre1"},
         :content ["4.1.2 Impact of State on Informal\n      Reasoning"]}
        {:tag :h3,
         :attrs {:id "section-4.2", :class "calibre1"},
         :content ["4.2 Complexity caused by Control"]}
        {:tag :h3,
         :attrs {:id "section-4.3", :class "calibre1"},
         :content ["4.3 Complexity caused by Code\n      Volume"]}
        {:tag :h3,
         :attrs {:id "section-4.4", :class "calibre1"},
         :content ["4.4 Other causes of complexity"]}]
       [{:tag :h3,
         :attrs {:class "calibre1", :id "section-4.1"},
         :content ["4.1 Complexity caused by State"]}
        {:tag :h3,
         :attrs {:id "section-4.2", :class "calibre1"},
         :content ["4.2 Complexity caused by Control"]}
        {:tag :h3,
         :attrs {:id "section-4.3", :class "calibre1"},
         :content ["4.3 Complexity caused by Code\n      Volume"]}
        {:tag :h3,
         :attrs {:id "section-4.4", :class "calibre1"},
         :content ["4.4 Other causes of complexity"]}]))

(deftest test-split-when
  (are [pred coll res] (= (split-when pred coll) res)
       not= [1 1 1 2 3] [[1 1 1] [2 3]]))