(ns aoc2025.day11
  (:require [aoc2025.util :as util]
            [better-cond.core :as b]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.rpl.specter :as s]
            [datomic.api :as d]
            [instaparse.core :as i]))

(comment

  1
  :foo
  "foo"
  {:foo 1}
  [:foo :bar]
  #{:foo :bar}

  (+ 1 2 3 4)

  (+ 1 (* 2 3))

  )

;; Local state

(def init-state
  {:parsed-input nil
   :src->dst nil
   :dst->src nil
   :conn nil})

(defonce *state
  (atom init-state))

(comment (reset! *state init-state))

;; Input parsing

(def text-input
  (-> "day11.input.txt" io/resource slurp str/trim))

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
  ;; (def steps)
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
    #_(map-indexed (fn [index set]
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

  #_(= steps
     (->> map-steps
          (mapv #(-> % keys set))))

  (->> map-steps
       (mapv #(select-keys % [:fft :dac])))

  )

;; Reverse the adjacency matrix... with Datomic

(def db-uri "datomic:mem://aoc-2025-day-11")

(defn get-db! []
  (let [conn (:conn @*state)]
    (assert conn)
    (d/db conn)))

(defn transact-idents [conn]
  @(d/transact conn [{:db/ident :node/name
                      :db/valueType :db.type/keyword
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :node/relates-to
                      :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many}]))

(comment
  (do (d/create-database db-uri)
      (let [conn (d/connect db-uri)]
        (swap! *state assoc :conn conn)
        (transact-idents conn)))
  (d/delete-database db-uri)
  @*state
  (swap! *state assoc :conn nil)
  (get-db!)
  (vec (d/datoms (get-db!) :eavt))
  )

(defn transact-devices! [{:keys [conn src->dst]}]
  (letfn [(to-tempid [kwd]
            (str (name kwd) "-tempid"))]
    (-> src->dst
        (->> (mapcat (fn [[src dests]]
                       (let [tempid (to-tempid src)]
                         (into [[:db/add tempid :node/name src]]
                               (for [dest dests]
                                 [:db/add tempid :node/relates-to (to-tempid dest)]))))))
        (conj [:db/add (to-tempid :out) :node/name :out])
        (->> (d/transact conn))
        deref)))

(comment

  (transact-devices! @*state)

  (let [db (get-db!)]
    (->> (d/datoms (get-db!) :eavt)
         (mapv (fn [{:keys [e a v]}]
                 [e (d/ident db a) v]))))

  (->> (d/q '[:find ?dst-kwd (distinct ?src-kwd)
              :where
              [?src :node/relates-to ?dst]
              [?src :node/name  ?src-kwd]
              [?dst :node/name ?dst-kwd]
              ]
            (get-db!))
       (into {} (map (juxt first (comp vec second))))
       (swap! *state assoc :dst->src)
       )

  (:dst->src @*state)

  )

(defn count-routes [{:keys [d->ds start end max-depth]}]
  (letfn [(follow-next-devices [device depth]
            (apply + (for [device' (get d->ds device)]
                       (check-device device' depth))))
          (check-device [device depth]
            (cond
              (= depth max-depth) 0
              (= end device) 1
              :else (follow-next-devices device (inc depth))))]
    (check-device start 0)))

(comment

  ;; svr -> fft

  (count-routes {:d->ds (:dst->src @*state)
                 :start :fft
                 :end :svr
                 :max-depth 12})

  7180

  ;; fft -> dac

  (def inter-result
    (future
      (count-routes {:d->ds (:dst->src @*state)
                     :start :dac
                     :end :fft
                     :max-depth 17})))

  inter-result

  {15 1218656
   16 4003876
   17 5553940
   18 5553940
   19 5553940}

  ;; dac -> out

  (count-routes {:d->ds (:src->dst @*state)
                 :start :dac
                 :end :out
                 :max-depth 11})

  {11 13391
   12 13391
   13 13391}

  (* 7180 5553940 13391) ;; => 533996779677200

  )
