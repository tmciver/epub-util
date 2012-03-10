(ns epub-utils.core
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn headings [page]
  (html/select page #{[:h1] [:h2] [:h3] [:h4] [:h5]}))

(defn- heading-to-val
  "Returns a value corresponding to the heading ordinal, e.g. :h2 -> 2"
  [h]
  (->> h
       name
       second
       str
       Integer.))

(defn subordinate-or-equal?
  "Returns true if the second entry represents a heading that is directly
subordinate or equal to the first entry."
  [h1 h2]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (or (= (- h2val h1val) 1)
        (= h2val h1val))))

(defn split-when-not-equal
  "Splits the given collection at the point at which successive elements become
  unequal. For example, [:a :a :b :c] becomes [[:a :a] [:b :c]] and [:a :b :c]
  becomes [[:a] [:b :c]]."
  [coll]
  (loop [l (vector (first coll))
         r (rest coll)]
    (if (and (not (empty? r)) (= (last l) (first r)))
      (recur (conj l (first r)) (rest r))
      [l (vec r)])))

(defn heading-to-navpoint
  "Converts a map representing an Enlive heading tag to a map representing an
  Enlive ePUB TOC NavPoint tag."
  [h]
  {:tag :navPoint
   :content [{:tag :navLabel
              :content [{:tag :text
                         :content (:content h)}]}
             {:tag :content
              :attrs {:src ""}}]})

(defn headings-to-navpoints
  "Converts a seq of enlive html headings and returns a nested map structure
  of navpoint maps."
  [hs]
  )

(defn fatten
  "Opposite of flatten. ;)  Takes a flat seq and constructs a nested vector such
  that a member 'b' of the seq is a child of the preceding member 'a' if
  subordinate? returns non-nil and consecutive members are considered siblings
  if siblings? returns non-nil."
  [s subordinate? siblings?]
  (letfn [(fatn [coll res prev]
            (when-let [a (first coll)]))]))