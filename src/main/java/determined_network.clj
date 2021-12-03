(ns determined_network)

;;; Lee Spector, lspector@amherst.edu, 2016-2021

;;; This file presents a minimal implementation, in Clojure, of the simulation described in:
;;; Riolo, Rick L., Cohen, Michael D., & Axelrod, Robert (2001), Evolution of cooperation without reciprocity,
;;; Nature 414: 441-43.
;;;
;;; By changing the `minimum-tolerance` parameter one can produce results similar to those described in:
;;; Roberts, G., & Sherratt, T. N. (2002). Does similarity breed cooperation? Nature, 418, 499–500.

;;; A world is a map containing a population (vector) of agents, along with counts of donations and failures.
;;; An agent is a tag, a  tolerance, and a score.
;;; New agents are initialized to random tags and tolerances, and scores of 0.


(defn new-world
  "Returns a population of agents with random tags and tolerances, and
  scores of zero, along with tallies of donations and failures."
  [{:keys [population-size num-interactions]}]
  (println num-interactions)
  {:agents    (vec (repeatedly population-size
                               (fn []
                                 {:other_donate       (if (> (rand) 0.5) 1 0)
                                  :other_reject       (if (> (rand) 0.5) 1 0)
                                  :self_donate        (if (> (rand) 0.5) 1 0)
                                  :self_reject        (if (> (rand) 0.5) 1 0)
                                  :score     0
                                  :buddies (vec (repeatedly num-interactions
                                                            (fn []
                                                              (rand-int population-size))
                                                            )
                                                )
                                  }
                                 )))
   :donations 0
   :failures  0}
  )

;;; The `maybe-donate` function handles single donation attempts, for a given donor and recipient.

(defn maybe-donate
  "Returns the world after a donation attempt by the agent at donor-index."
  [donor-index recipient-index
   {:keys [agents donations failures]}                      ;; world
   {:keys [cost benefit]}]                                  ;; params
  (if (= donor-index recipient-index)                       ;; return the world if donor and recipient are same
    {:agents agents :donations donations :failures failures}
    (let [donor (nth agents donor-index)
          recipient (nth agents recipient-index)]
      (if (> (/ (+ (:other_donate donor) (* (:self_reject donor) 0.5))
                   (+ (:self_donate donor) (* (:other_reject donor) 1.5)) ;;donor's gratitude
                   )
                (rand))
        {:agents    (-> agents
                        (assoc donor-index
                               (assoc donor :score (- (:score donor) cost)))
                        (assoc recipient-index
                               (assoc recipient :score (+ (:score recipient) benefit))))
         :donations (inc donations)
         (inc (:self_donate donor))
                    (inc (:other_donate recipient))
         :failures  failures
         }
        {:agents    agents
         :donations donations
         (inc (:self_reject donor))
                    (inc (:other_reject recipient))
         :failures  (inc failures)}
        )                                          ;; placeholder threshold

      )
    )
  )

;;; The `conduct-pairings` function handles all of the possible donations from one given donor in a generation.

(defn conduct-pairings
  "Returns the world after all pairings in which the indexed donor attempts to make a donation."
  [donor-index world params]
  (nth (iterate #(maybe-donate donor-index
                               (:buddies (nth (:agents world) ))
                               %
                               params)
                world)
       (:pairings params)))

;;; The `all-donations` function handles all of the donations for a complete generation.

(defn all-donations
  "Returns the population after all donation attempts by all agents."
  [world params]
  (reduce (fn [w i]
            (conduct-pairings i w params))
          (assoc world :donations 0 :failures 0) ;; start with donation and failure counts of zero
          (range (:population-size params))))

;;; When tolerances are mutated they are adjusted by Gaussian noise, and there's no built-in Clojure function for
;;; this, so we include some utilities to give us the needed random numbers:

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (*' (Math/sqrt (*' -2.0 (Math/log (rand))))
      (Math/cos (*' 2.0 Math/PI (rand)))))

(defn noise
  "Returns gaussian noise of mean 0, std dev 0.01."
  []
  (* 0.01 (gaussian-noise-factor)))

;;; The `reproduction` function produces the next generation by conducting tournaments (based on scores) and
;;; mutating the winners.

(defn reproduction
  "Returns a population of children produced from the given population."
  [{:keys [agents donations failures]} ;; world
   {:keys [mutation-rate minimum-tolerance]}] ;; params
  {:agents    (mapv (fn [a]
                      (let [competitor (rand-nth agents)
                            winner (if (>= (:score a) (:score competitor))
                                     a
                                     competitor)
                            loser (if (< (:score a) (:score competitor))
                                    a
                                    competitor)
                            new_agent (assoc loser :other_donate (:other_donate winner)
                                                   :self_donate (:self_donate winner)
                                                   :other_reject (:other_reject winner)
                                                   :self_reject (:self_reject winner)
                                                      )
                           ]
                        (assoc new_agent :score 0))
                      )
                    agents)
   :donations donations
   :failures  failures})

;;; Finally, the `run-simulation` function runs a loop of generations (donations followed by reproduction),
;;; returning the donation rate for each generation:

(defn run-simulation
  "Runs a simulation of the Riolo et al system."
  [params]
  (loop [generation 1
         w (new-world params)
         rates []]
    (if (> generation (:generations params))
      rates                                                    ;;rates
      (let [next-world (reproduction (all-donations w params) params)
            d (:donations next-world)
            f (:failures next-world)
            rate (float (/ d (+ d f)))]

        (recur (inc generation)
               next-world
               (conj rates rate)
               )
        )
      )
    )
  )




;;; Evaluate something like this to run a simulation:

(run-simulation {:population-size   100
                   :cost              0.1
                   :benefit           1.0
                   :pairings          10
                   :mutation-rate     0.1
                   :generations       100   ; higher numbers generally used here, but often the pattern emerges early
                   :minimum-tolerance 0
                   :num-interactions  5
                 })    ; 0 in Riolo et al., but -0.000001 in Roberts & Sherratt

