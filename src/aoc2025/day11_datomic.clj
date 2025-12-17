(ns aoc2025.day11-datomic
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]
            [datomic.api :as d]
            [instaparse.core :as i]))

(defonce *state
  (atom {:conn nil
         :parsed-input nil}))

(def db-uri "datomic:mem://aoc-2025-day-11")

(defn get-db! []
  (let [conn (:conn @*state)]
    (assert conn)
    (d/db conn)))

(defn transact-idents [conn]
  @(d/transact conn [{:db/ident :node/name
                      :db/valueType :db.type/string
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

(def text-input
  (-> "day11.input.txt" io/resource slurp str/trim))

(def grammar "
S = relation {'\n' relation}
relation = source ':' {targets}
targets = target+
target = ' ' dest
source = #'[a-z]'+
dest = #'[a-z]'+
")

(def parse
  (i/parser grammar :output-format :enlive))

(comment
  (parse text-input)
  (swap! *state assoc :parsed-input (parse text-input))
  (->> (:parsed-input @*state)
       (s/select [:content s/ALL :content not-empty (s/subselect s/ALL map?)])
       (map (fn [[src & dests]]
              (let [src' (-> src :content str/join)
                    dests' (->> dests
                                (s/select [s/ALL :content s/ALL :content s/ALL map? :content])
                                (map str/join))]
                [src' dests'])))
       (mapcat (fn [[src dests]]
                 (let [tempid (str src "-tempid")]
                   (into [[:db/add tempid :node/name src]]
                         (for [dest dests]
                           [:db/add tempid :node/relates-to [:node/name dest]])))))
       (d/transact (:conn @*state))
       deref
       )

  (d/q '[:find (count ?e) .
         :where
         [?me :node/name "you"]
         [?me :node/relates-to ?e]
         [?e :node/relates-to ?ee]
         [?ee :node/name "out"]
         ]
       (get-db!))

  )
