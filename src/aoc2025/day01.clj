(ns aoc2025.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as i]
            [com.rpl.specter :as s]))

(def text-input
  (-> "day01.input.txt" io/resource slurp str/trim))

(comment
  (-> text-input (str/split #"\n"))
  )

(def grammar "
S = rotation {newline rotation}
newline = '\n'
direction = 'L' | 'R'
number = #'[1-9]' {#'[0-9]'}
rotation = direction number
")

(def parse
  (i/parser grammar :output-format :enlive))

(comment
  (->> (parse text-input)
       (s/select [:content s/ALL #(= :rotation (:tag %)) :content])
       (map (fn [[dir num]]
              {:direction (case (-> dir :content first)
                            "L" :left
                            "R" :right)
               :number (->> num :content (apply str) parse-long)})))
  )
