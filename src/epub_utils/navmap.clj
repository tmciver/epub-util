(ns epub-utils.navmap
  (:require [net.cgrand.enlive-html :as enlive]
            [hiccup.core :as hiccup])
  (:import java.io.File))

(defn headings [page]
  (enlive/select page #{[:h1] [:h2] [:h3] [:h4] [:h5]}))

(defn- heading-to-val
  "Returns a value corresponding to the heading ordinal, e.g. :h2 -> 2"
  [h]
  (->> h
       name
       second
       str
       Integer.))

(defn subordinate-heading?
  "Returns true if the second entry represents a heading that is subordinate to
the first entry."
  [{h1 :tag} {h2 :tag}]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (< h1val h2val)))

(defn directly-subordinate-heading?
  "Returns true if the second entry represents a heading that is directly
subordinate to the first entry."
  [{h1 :tag} {h2 :tag}]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (= (- h2val h1val) 1)))

(defn direct-child-headings
  "Returns the direct child headings of the given heading. The heading and its
  children must be found in coll.  Child headings are considered to bo those
  headings immediately following heading and before a heading of equal or
  greater rank to heading and which are directly subordinate to heading."
  [heading coll]
  (let [descendents (->> coll
                         (drop-while #(not= heading %))
                         rest
                         (take-while #(subordinate-heading? heading %)))]
    (when descendents
      (filter #(directly-subordinate-heading? heading %) descendents))))

(defn get-top-level-sibling-headings
  "Returns a collection of headings that are the highest rank found in coll or
an empty collection if none."
  [coll]
  (let [hirank (apply min (map #(heading-to-val (:tag %)) coll))]
    (filter #(= hirank (heading-to-val (:tag %))) coll)))

(defn nest-headings
  "Takes a seq of enlive heading maps and nests each heading's children using
  keyword :children."
  [headings]
  (let [tls (get-top-level-sibling-headings headings)
        nest (fn nest [heading]
               (let [children (direct-child-headings heading headings)]
                 (if (empty? children)
                   heading
                   (assoc heading :children (map nest children)))))]
    (map nest tls)))

(defn headings-to-navpoints
  "Converts a collection of nested headings to a collection of hiccup-style
  vectors representing navpoints."
  [headings]
  (let [navpoint (fn navpoint
                   [heading]
                   (let [htext (.replaceAll (first (:content heading)) "\\s+" " ")]
                     [:navPoint
                      [:navLabel
                       [:text htext]]
                      [:content {:src ""}
                       (when-let [children (:children heading)]
                         (vec (map navpoint children)))]]))]
    (map navpoint headings)))

(defn extract-navpoints
  "Takes a path to a file as a string and returns a collection of hiccup-style
  vectors representing navPoints."
  [filepath]
  (let [res (enlive/html-resource (File. filepath))
        hs (headings res)
        nested-hs (nest-headings hs)]
    (headings-to-navpoints nested-hs)))