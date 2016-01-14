(ns azuzu.xml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defn dbg [node]
  (if (associative? node)
    (xml/emit-element (dissoc node :content))
    (xml/emit-element node))
  node)

(defn ->str [node]
  (clojure.string/trim
    (with-out-str
      (if (associative? node)
        (xml/emit-element (dissoc node :content))
        (xml/emit-element node)))))

(defn dbg-z [zipper]
  (do (dbg (zip/node zipper))
      zipper))

(defn tag
  "Returns the tag that the zipper is pointing to as a string"
  [zipper]
  (->str (zip/node zipper)))

(defn children
  "Returns the children of the tag the zipper is pointing to as a string"
  [zipper]
  (map #(->str %)
       (zip/children zipper)))

(defn tag+children
  "Returns the tag that the zipper is pointing to, as well as its children, as a string"
  [zipper]
  [(tag zipper) (children zipper)])

(defn tag+child1
  "Returns the tag that the zipper is pointing to, as well as its first child, as a string"
  [zipper]
  [(tag zipper) (first (children zipper))])

(defn tag+child2
  "Returns the tag that the zipper is pointing to, as well as its second child, as a string"
  [zipper]
  [(tag zipper) (second (children zipper))])

(defn tag+childN
  "Returns the tag that the zipper is pointing to, as well as its nth child, as a string"
  [zipper n]
  [(tag zipper) (nth (children zipper) n)])


(defn first-tag [xml]
  (->> xml zip/xml-zip zip/children first))

(defn children-tags [xml]
  (->> xml zip/xml-zip zip/children))

