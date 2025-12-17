(ns aoc2025.day11-failures)

(comment

  ;; Out of memory
  #_(let [{:keys [src->dst]} @*state]
    (loop [iteration 0
           cnt 0
           paths [{:device :svr
                   :dac? false
                   :fft? false}]]
      (b/cond
        (= iteration max-iter) ::error
        (empty? paths) cnt
        :let [next-paths (into []
                               (mapcat (fn [{:keys [device] :as m}]
                                         (for [device (get src->dst device)]
                                           (cond-> (assoc m :device device)
                                             (= :dac device) (assoc :dac? true)
                                             (= :fft device) (assoc :fft? true)))))
                               paths)
              arrivals (reduce (fn [acc m]
                                 (if (and (= :out (:device m))
                                          (:dac? m)
                                          (:fft? m))
                                   (inc acc)
                                   acc))
                               0
                               next-paths)
              next-paths' (into [] (remove #(= :out (:device %))) next-paths)
              _ (when (> arrivals 0)
                  (println arrivals))]
        :else (recur (inc iteration) (+ cnt arrivals) next-paths'))))

  (def part2-task
    (future
      (let [{:keys [src->dst]} @*state
            max-depth 1000]
        (letfn [(follow-next-devices [{:keys [device] :as m}]
                  (apply + (for [device' (get src->dst device)]
                             (check-device (assoc m :device device')))))
                (check-device [{:keys [device depth dac? fft?] :as m}]
                  (b/cond
                    (= depth max-depth) (throw (Exception. "Max depth"))
                    (and (= :out device) dac? fft?) 1
                    (= :out device) 0
                    (= :dac? device) (follow-next-devices (assoc m :dac? true :depth (inc depth)))
                    (= :fft device) (follow-next-devices (assoc m :fft? true :depth (inc depth)))
                    :else (follow-next-devices (assoc m :depth (inc depth)))))]
          (check-device {:device :svr
                         :dac? false
                         :fft? false
                         :depth 0})))))
  part2-task

  ;; Steps
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

  #_(def exec
    (Executors/newVirtualThreadPerTaskExecutor))
  #_(def part2-par-task
    (let [{:keys [src->dst]} @*state]
      (letfn [(follow-next-devices [{:keys [device] :as m}]
                (->> (get src->dst device)
                     (mapv (fn [device']
                             (.submit exec ^Callable #(check-device (assoc m :device device')))))
                     (mapv deref)
                     (apply +)))
              (check-device [{:keys [device dac? fft?] :as m}]
                (b/cond
                  (and (= :out device) dac? fft?) 1
                  (= :out device) 0
                  (= :dac? device) (follow-next-devices (assoc m :dac? true))
                  (= :fft device) (follow-next-devices (assoc m :fft? true))
                  :else (follow-next-devices m)))]
        (.submit exec ^Callable #(check-device {:device :svr
                                                :dac? false
                                                :fft? false})))))

  )

(comment

  (let [max-i 7]
    (time
      (loop [i 1
             all-sets #{(get steps 0)}]
        (b/cond
          (= max-i i) all-sets
          :let [step-set (get steps i)]
          :else (recur
                  (inc i)
                  (reduce (fn [acc paths-set]
                            (reduce (fn [acc' device]
                                      (let [paths-set' (conj paths-set device)]
                                        (conj acc' paths-set')))
                                    acc
                                    step-set))
                          #{}
                          all-sets))))))

(let [{:keys [src->dst]} @*state
        max-iter 11]
    (time
      (loop [iteration 0
             final-count 0
             device-paths {{:visited-devices #{:svr} :last-device :svr} 1}]
        (b/cond
          (= iteration max-iter) {:device-path device-paths}
          :let [next-vals (reduce (fn [acc [{:keys [visited-devices last-device] :as step} cnt]]
                                    (let [next-devices (get src->dst last-device)]
                                      (reduce (fn [[final-count' device-paths'] next-device]
                                                (b/cond
                                                  (and (= :out next-device)
                                                       (not-empty (set/intersection visited-devices #{:dac :fft})))
                                                  [(+ cnt final-count') (dissoc device-paths' step)]
                                                  ;; ---
                                                  (= :out next-device) [final-count' (dissoc device-paths' step)]
                                                  :let [next-step {:visited-devices (conj visited-devices next-device)
                                                                   :last-device next-device}]
                                                  :else [final-count' (assoc device-paths' next-step
                                                                             (if-let [previous-cnt (get device-paths' next-step)]
                                                                               (+ previous-cnt cnt)
                                                                               cnt))]))
                                              acc
                                              next-devices)))
                                  [final-count {}]
                                  device-paths)
                [final-count' device-paths'] next-vals
                #_#__ (do (pp/pprint (util/vals-to-map previous-counts next-counts))
                          (println "---"))
                ]
          (empty? device-paths) {:final-count final-count}
          :else (recur (inc iteration) final-count' device-paths')))))

  )
