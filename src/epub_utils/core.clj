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

(defn directly-inferior?
  "Returns true if the second entry represents a heading that is directly
inferior to the first heading."
  [h1 h2]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (and (< h1val h2val)
         (= (- h2val h1val) 1))))

(defn child-or-sibling?
  "Returns true if the second entry is directly inferior to or a sibling of the
  first entry."
  [h1 h2]
  (or (directly-inferior? h1 h2)
      (let [h1val (heading-to-val h1)
            h2val (heading-to-val h2)]
        (= h1val h2val))))

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

