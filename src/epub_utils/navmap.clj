(ns epub-utils.navmap
  (:require [net.cgrand.enlive-html :as enlive]
            [hiccup.core :as hiccup])
  (:import [java.io File StringWriter StringReader]
           [org.w3c.tidy Tidy]))

(defn headings
  "Takes one or more strings representing paths to files from which all html
  headings will be returned in the order in which they are encountered. Key
  :src-file is added to the enlive tag map whose value is the source file's
  name (to be used downstream)."
  [& filepaths]
  (let [htags #{[:h1] [:h2] [:h3] [:h4] [:h5] [:h6]}
        files (map #(File. %) filepaths)
        hs (->> files
                (map #(enlive/html-resource %))
                (map #(enlive/select % htags)))
        add-src-file (fn [headings file]
                       (map #(assoc % :src-file (.getName file)) headings))
        hs-w-src (map add-src-file hs files)
        add-po (fn [playorder heading]
                 (assoc heading :playorder playorder))]
    (->> hs-w-src
         (apply concat)
         (map add-po (range)))))

(defn- heading-to-val
  "Returns a value corresponding to the heading ordinal, e.g. :h2 -> 2"
  [h]
  (->> h
       name
       second
       str
       Integer.))

(defn subordinate-heading?
  "Returns true if the second argument represents a heading that is subordinate
to the first argument. Arguments should be maps that have a :tag key whose value
is a keyword of the form :hn where n is an integer. These keywords represent
html heading tags."
  [{h1 :tag} {h2 :tag}]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (< h1val h2val)))

(defn directly-subordinate-heading?
  "Returns true if the second argument represents a heading that is directly
subordinate to the first argument. Arguments should be maps that have a :tag key
whose value is a keyword of the form :hn where n is an integer. These keywords
represent html heading tags."
  [{h1 :tag} {h2 :tag}]
  (let [h1val (heading-to-val h1)
        h2val (heading-to-val h2)]
    (= (- h2val h1val) 1)))

(defn direct-child-headings
  "Returns the direct child headings of the given heading. The heading and its
  children must be found in coll.  Child headings are considered to be those
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
  "Takes a collection of enlive heading maps and nests each heading's children
  using keyword :children."
  [headings]
  (let [tls (get-top-level-sibling-headings headings)
        nest (fn nest [heading]
               (let [children (direct-child-headings heading headings)]
                 (if (empty? children)
                   heading
                   (assoc heading :children (map nest children)))))]
    (map nest tls)))

(defn headings-to-navmap
  "Converts a collection of nested headings to a hiccup-style
  vectors representing navpoints. filename is the name of the file used to
  generate the headings. This is needed to set the src attribute of the
  navpoints content tag."
  [headings]
  (let [navpoint (fn navpoint
                   [heading]
                   (let [htext (.replaceAll (first (:content heading)) "\\s+" " ")]
                     [:navPoint
                      {:playOrder (:playorder heading)
                       :id (str "navpoint-id-" ((comp :id :attrs) heading))}
                      [:navLabel
                       [:text htext]]
                      [:content {:src (str (:src-file heading) "#" ((comp :id :attrs) heading))}]
                      (when-let [children (:children heading)]
                        (map navpoint children))]))]
    [:navMap (map navpoint headings)]))

(defn tidy
  "Takes a string of html and 'tidies' it up using JTidy. Returns the tidied
  html."
  [htmlstr]
  (let [tide (doto (Tidy.)
               (.setSmartIndent true)
                                        ;               (.setXmlOut true)
                                        ;               (.setXHTML false)
                                        ;               (.setTrimEmptyElements true)
               (.setShowWarnings true)
               (.setQuiet false))
        swrtr (StringWriter.)
        srdr (StringReader. htmlstr)]
    (.parse tide srdr swrtr)
    (str swrtr)))