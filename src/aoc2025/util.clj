(ns aoc2025.util)

(defmacro def-syms [& syms]
  `(do ~@(for [sym syms]
           `(def ~sym ~sym))))

(defmacro def-map [name & syms]
  (let [hm (->> syms
                (map #(do [(keyword %) %]))
                (into {}))]
    `(def ~name ~hm)))

(comment

  (def foo 1)
  (def bar 2)
  (def-dev foobar
    foo bar)

  )

(defmacro vals-to-map [& syms]
  (->> syms
       (map #(do [(keyword %) %]))
       (into {})))
