(ns aoc2025.day11
  (:require [aoc2025.util :as util]
            [better-cond.core :as b]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.rpl.specter :as s]
            [instaparse.core :as i]
            [clojure.set :as set]
            [clojure.math :as math])
  (:import [java.util.concurrent Executors]))

(def text-input
  (-> "day11.input.txt" io/resource slurp str/trim))

(def init-state
  {:parsed-input nil
   :src->dst nil})

(defonce *state
  (atom init-state))

(comment (reset! *state init-state))

(def grammar "
S = link {newline link}
newline = '\n'
whitespace = ' '
separator = ':'
link = source separator {whitespace destination}
device = #'[a-z]{3}'
source = device
destination = device
")

(def parse
  (i/parser grammar :output-format :enlive))

(defn ast->map [parsed-input]
  (->> parsed-input
       (s/select [:content s/ALL #(= :link (:tag %))
                  :content (s/filterer #(contains? #{:source :destination} (:tag %)))
                  (s/multi-path
                    [(s/filterer #(= :source (:tag %))) s/ALL :content s/ALL :content s/FIRST]
                    [(s/filterer #(= :destination (:tag %)))
                     (s/subselect s/ALL :content s/ALL :content s/FIRST)])])
       (partition 2)
       (walk/prewalk #(if (string? %) (keyword %) %))
       (map vec)
       (into {})))

(comment
  (let [parsed-input (parse text-input)
        src->dst (ast->map parsed-input)]
    (swap! *state assoc
           :parsed-input parsed-input
           :src->dst src->dst))
  )

(def max-iter 1000)

(defn solve-day-11-part-1 [src->dst]
  (loop [iteration 0
         cnt 0
         paths [:you]]
    (b/cond
      (= iteration max-iter) ::error
      (empty? paths) cnt
      :let [next-paths (into [] (mapcat src->dst) paths)
            arrivals (reduce (fn [acc device]
                               (if (= :out device) (inc acc) acc))
                             0
                             next-paths)
            next-paths' (into [] (remove #(= :out %)) next-paths)]
      :else (recur (inc iteration) (+ cnt arrivals) next-paths'))))

(comment
  (-> @*state :src->dst solve-day-11-part-1)
  )

;; Part 2

(comment

  ;; Steps
  (def steps)
  (->>
    (let [{:keys [src->dst]} @*state
          max-iter 10000
          start :svr
          end :out]
      (loop [iteration 0
             cnt 0
             paths #{start}
             path-seq [#{start}]]
        (b/cond
          (= iteration max-iter) ::error
          (empty? paths) {:cnt cnt
                          :iterations iteration
                          :path-seq path-seq}
          :let [next-paths (into #{} (mapcat src->dst) paths)
                path-seq' (conj path-seq next-paths)
                cnt' (if (contains? next-paths end)
                       (inc cnt)
                       cnt)
                next-paths' (disj next-paths end)]
          :else (recur (inc iteration) cnt' next-paths' path-seq'))))
    :path-seq
    ;; (map count)
    (map-indexed (fn [index set]
                   [index (not-empty (set/intersection set #{:dac :fft}))])))

  (def map-steps
    (let [{:keys [src->dst]} @*state
          max-iter 10000
          start :svr
          end :out]
      (loop [iteration 0
             previous-counts {start 1}
             total-counts [{start 1}]]
        (b/cond
          (= iteration max-iter) ::error
          :let [next-counts (reduce (fn [acc [device _]]
                                      (let [next-devices (get src->dst device)]
                                        (reduce (fn [acc' next-device]
                                                  (update acc' next-device (fnil inc 0)))
                                                acc
                                                next-devices)))
                                    {}
                                    previous-counts)]
          (empty? next-counts) total-counts
          :let [total-counts' (conj total-counts next-counts)]
          :else (recur (inc iteration) next-counts total-counts')))))

  (count steps)
  (count map-steps)
  (= steps
     (->> map-steps
          (mapv #(-> % keys set))))

  (->> map-steps
       (mapv #(select-keys % [:fft :dac])))

  )
