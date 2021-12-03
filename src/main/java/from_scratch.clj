(ns from-scratch)
(defn new-world-init
  "Returns a population of agents with random tags and tolerances, and
  scores of zero, along with tallies of donations and failures."
  [{:keys [population-size]}]
  {:agents    (vec (repeatedly 9
                               (fn []
                                 {:other_donate       (if (> (rand) 0.5) 1 0)
                                  :other_reject       (if (> (rand) 0.5) 1 0)
                                  :self_donate        (if (> (rand) 0.5) 1 0)
                                  :self_reject        (if (> (rand) 0.5) 1 0)
                                  :initiate           (if (> (rand) 0.5) 0.8 0)
                                  :reciprocate        (if (> (rand) 0.5) 1 0)
                                  :neighbors          (start-graph 9)
                                  :score     0})))
   :donations 0
   :failures  0}
  )




(defn new-world-add
  "Returns a population of agents with random tags and tolerances, and
  scores of zero, along with tallies of donations and failures."
  [{:keys [population-size]}]
  {:agents    (vec (repeatedly 9
                               (fn []
                                 {:other_donate       (if (> (rand) 0.5) 1 0)
                                  :other_reject       (if (> (rand) 0.5) 1 0)
                                  :self_donate        (if (> (rand) 0.5) 1 0)
                                  :self_reject        (if (> (rand) 0.5) 1 0)
                                  :initiate           (if (> (rand) 0.5) 0.8 0)
                                  :reciprocate        (if (> (rand) 0.5) 1 0)
                                  :neighbors          (start-graph 9)
                                  :score     0})))
   :donations 0
   :failures  0}
  )