(ns aoc2025.day02
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str]
            [com.rpl.specter :as s]
            [instaparse.core :as i]))

(comment
  (clojure.repl.deps/sync-deps)
  )

(def input (-> "day02.input.txt" io/resource slurp str/trim))

(def grammar "
S = range {',' range}
range = min '-' max
number = { '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' }
min = number
max = number
")

(def parse (i/parser grammar))

(comment

  (parse input)

  (->> (str/split input #",")
       (map #(str/split % #"-"))
       #_(s/transform [s/ALL s/ALL]
                    count
                    #_(-> % parse-long math/log10 inc math/floor long))
       vec)
  )

