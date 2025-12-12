(ns aoc2025.day02
  (:require [aoc2025.util :refer [def-dev]]
            [better-cond.core :as b]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str]
            [clojure.tools.namespace.repl :as nst]
            [com.rpl.specter :as s]
            [criterium.core :as c]
            [instaparse.core :as i]))

(def text-input
  (-> "day02.input.txt" io/resource slurp str/trim))

(def grammar "
S = range {',' range}
range = min '-' max
number = #'[1-9]' {#'[0-9]'}
min = number
max = number
")

(def parse
  (i/parser grammar :output-format :enlive))

(defn get-lower-bound [s]
  (b/cond
    :let [s-count (count s)
          n (parse-long s)]
    (< n 11) 1
    (odd? s-count) (let [up (->> s-count (math/pow 10) long)
                         div (->> (/ (inc s-count) 2)
                                  (math/pow 10)
                                  long)
                         ;; _ (def-dev foobar2 up div)
                         ]
                     (/ up div))
    :let [half (/ s-count 2)
          pref-int (parse-long (subs s 0 half))
          suf-int (parse-long (subs s half))
          ;; _ (def-dev foobar prefix suffix pref-int suf-int)
          ]
    (<= suf-int pref-int) pref-int
    :else (inc pref-int)))

(defn get-upper-bound [s]
  (b/cond
    :let [s-count (count s)
          n (parse-long s)]
    (< n 11) 0
    (odd? s-count) (let [up (->> s-count (math/pow 10) long)
                         div (->> (/ (inc s-count) 2)
                                  (math/pow 10)
                                  long)
                         ;; _ (def-dev foobar2 up div)
                         ]
                     (dec (/ up div)))
    :let [half (/ s-count 2)
          pref-int (parse-long (subs s 0 half))
          suf-int (parse-long (subs s half))
          ;; _ (def-dev foobar prefix suffix pref-int suf-int)
          ]
    (< suf-int pref-int) (dec pref-int)
    :else pref-int))

(let [input-data (->> (parse text-input)
                      (s/select [:content s/ALL map? :content
                                 (s/subselect s/ALL map? :content s/ALL :content)])
                      (s/transform [s/ALL s/ALL] (partial apply str)))]
  (defn solve-part-1 []
    (->> input-data
         (transduce
           (comp
             (map (fn [[min max]]
                    [(get-lower-bound min) (get-upper-bound max)]))
             (filter (fn [[lower upper]]
                       (<= lower upper)))
             (mapcat (fn [[lower upper]]
                       (range lower (inc upper))))
             (map #(-> (str % %) parse-long)))
           +))))

(comment

  (solve-part-1 #_text-input)

  (c/bench (solve-part-1))

  (nst/refresh)

  (->> (str/split text-input #",")
       (map #(str/split % #"-"))
       (s/transform [s/ALL s/ALL]
                      count
                      #_(-> % parse-long math/log10 inc math/floor long))
       vec)

  ;; Alternative with hiccup parser output
  ;; Less readable...
  (->> ((i/parser grammar :output-format :hiccup) text-input)
       (s/select [s/ALL vector?
                  (s/subselect s/ALL vector? s/ALL vector?
                               (s/subselect s/ALL string?))])
       (s/transform [s/ALL s/ALL] (partial apply str)))

  )
