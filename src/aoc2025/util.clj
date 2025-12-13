(ns aoc2025.util)

(defmacro def-dev [name & syms]
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

(defmacro print-vals [& syms]
  (let [hm (->> syms
                (map #(do [(keyword %) %]))
                (into {}))]
    `(println ~hm)))
