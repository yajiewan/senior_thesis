(ns jason_thesis_basic_20211013)
(require '[clojure.set :as s])

; ------------------------------- Helper functions -------------------------------
(defn -main
  []
;This gives me the strategy matrix each generation for 100 generations
(let [cd 50
      cc 0
      po 0
      g (generate-world 4 100)
      rec (resource-initiate g 0)
      strat (strategy-initiate g cd cc po)]
  (run-simulation-strategy g rec strat 100 20 5 1))
  )



; Helper method that handles a single donation between two agents
(defn donation-update
  [g rec donor-index recipient-index b c]
  (assoc
    (assoc rec donor-index
               (- (nth rec donor-index) c)) recipient-index
                                            (+ (nth rec recipient-index) b)))


; Handles a chain of helping behavior and updates the resource matrix.
(defn help-pass-on
  [g resource-matrix strategy-matrix donor-index recipient-index b c]
  (if (< (rand) (first (nth strategy-matrix donor-index)))
    ;If this help is passed on, find another neighbor and recur
    (help-pass-on g (donation-update g resource-matrix donor-index recipient-index b c)
                  strategy-matrix
                  recipient-index
                  (first (take 1 (shuffle (nth g recipient-index)))) b c)
    ;If this help is not passed on, end the chain and return the resource matrix
    (donation-update g resource-matrix donor-index recipient-index b c)))


;Initiate the fully connected adjacency matrix of size m
(defn start-graph
  [m]
  (mapv #(vec (s/difference (set (range %1)) #{%2}))
        (repeat (+ 1 (* 2 m)) (+ 1 (* 2 m)))
        (range (+ 1 (* 2 m)))))

;Get the vector of m nodes to attach to for any new node.
(defn get-append-list
  [g m]
  (loop [rand-vec []
         result-vec []]
    (if (= m (count (distinct result-vec)))
      (loop [a []
             j 0]
        (if (>= j (count result-vec))
          (distinct a)
          (recur
            (conj a (first (nth result-vec j)))
            (inc j))))
      (recur
        rand-vec
        (conj result-vec
              (take 1 (flatten (shuffle (mapv #(repeat %1 (conj rand-vec %2))
                                              (mapv #(count %) g)
                                              (range (count g)))))))))))


;Create links between the new point and existing points.
(defn add-new-point
  [append-list g i]
  (conj (mapv (fn [index node]
                (if (some #{index} append-list)
                  (conj node i)
                  node))
              (range)
              g)
        append-list))





; ------------------------------- Functions to create the world and run through generations -------------------------------
; Create the adjacency matrix
(defn generate-world
  [m size]
  (loop [g (start-graph m)
         i (+ 1 (* 2 m))]
    (if (>= i size)
      g
      (recur
        (let [list (get-append-list g m)]
          (add-new-point list g i))
        (inc i)))))


; Generate the vector containing the amount of resource of all agents.
(defn resource-initiate
  [g start-resource]
  (vec (repeat (count g) start-resource)))


; Select a random agent to attempt to initiate help, meaning selecting a classical defector agent will do nothing.
; If the agent decides to help, it will randomly select one of its neighbors and donate to that neighbor.
; The neighbor will then decide whether to pass on the help or not (randomly donates to one of its neighbors).
; Returns the resource vector after all agents attempted to initiate help.
(defn resource-update
  [g resource-matrix strategy-matrix b c]
  (loop [i 0
         rec resource-matrix]
    (if (>= i (count g))
      rec
      (recur (inc i)
             (let [donor-index i
                   recipient-index (first (take 1 (shuffle (nth g donor-index))))]
               (if (< (rand) (second (nth strategy-matrix donor-index)))
                 (help-pass-on g (donation-update g rec donor-index recipient-index b c) strategy-matrix donor-index recipient-index b c)
                 rec))))))

; Generate the strategy matrix for all agents: Specifically, all agents have two attributes:
; [probability-to-pass-on-help probability-to-initiate-help]
(defn strategy-initiate
  [g cd cc po]
    (shuffle (concat (repeatedly cd (fn [] (vec [0 0]))) ; Classical defector: Never initates help or passes on
                     (repeatedly cc (fn [] (vec [0 1]))) ; Classical cooperator: Always initiates help but never passes on
                     (repeatedly po (fn [] (vec [0.8 0]))) ; Passer-on:  High probability to pass on help but never initiates help
                     (repeatedly (- (count g) (+ po (+ cd cc))) (fn [] (vec [0.8 1])))))) ; Generous cooperator: Always initiates help and very likely to pass on


; Returns the strategy vector for a given number of agents
; The paper I'm replicating chose 20, 200, 2,000 agents out of a population of 10,000 to update in each generation
; at the end of each generation by 100% imitation learning from its most successful neighbor
(defn strategy-update
  [g resource-vector strategy-vector update-number]
  (loop [i 0
         strategy strategy-vector
         agents-to-update (take update-number (shuffle (range (count g))))]
    (if (>= i update-number)
      strategy
      (recur (inc i)
             (assoc
               strategy (nth agents-to-update i)

                        (nth strategy

                             (first (apply max-key second

                                           (map-indexed vector
                                                        (mapv #(nth resource-vector %)
                                                              (conj (nth g (nth agents-to-update i)) (nth agents-to-update i))))))))
             agents-to-update))))





; ------------------------------- Functions that return statistics for a single generation -------------------------------
; Run simulation across a set number of generations and returns the strategy matrix at the end.


; Returns the average payoff of all Classical Defectors at the end of a generation
(defn average-payoff-CD
  [rec strat cd]
  (double
    (/ (reduce
         + (mapv (fn [c] (nth rec c))
                 (mapv (fn [b] (first b))
                       (filter (fn [a]
                                 (and (= 0 (first (second a)))
                                      (= 0 (second (second a)))))
                               (map-indexed vector strat))))) cd)))

; Returns the average payoff of all Classical Cooperators at the end of a generation
(defn average-payoff-CC
  [rec strat cc]
  (double
    (/ (reduce
         + (mapv (fn [c] (nth rec c))
                 (mapv (fn [b] (first b))
                       (filter (fn [a]
                                 (and (= 0 (first (second a)))
                                      (= 1 (second (second a)))))
                               (map-indexed vector strat))))) cc)))

; Returns the average payoff of all Generous Cooperators at the end of a generation
(defn average-payoff-GC
  [rec strat gc]
  (double
    (/ (reduce
         + (mapv (fn [c] (nth rec c))
                 (mapv (fn [b] (first b))
                       (filter (fn [a]
                                 (and (= 0.8 (first (second a)))
                                      (= 1 (second (second a)))))
                               (map-indexed vector strat))))) gc)))

; Returns the average payoff of all Passers-On at the end of a generation
(defn average-payoff-PO
  [rec strat po]
  (double
    (/ (reduce
         + (mapv (fn [c] (nth rec c))
                 (mapv (fn [b] (first b))
                       (filter (fn [a]
                                 (and (= 0.8 (first (second a)))
                                      (= 0 (second (second a)))))
                               (map-indexed vector strat))))) po)))



;Returns the proportion of classical defectors in the population at the end of a generation
(defn proportion-cd
  [g strat]
  (double (/ (count (filter (fn [a] a)
                            (mapv (fn [b]
                                    (and (= 0 (first b))
                                         (= 0 (second b)))) strat))) (count g))))


;Returns the proportion of classical cooperators in the population at the end of a generation
(defn proportion-cc
  [g strat]
  (double (/ (count (filter (fn [a] a)
                            (mapv (fn [b]
                                    (and (= 0 (first b))
                                         (= 1 (second b)))) strat))) (count g))))
;Returns the proportion of passer-ons in the population at the end of a generation
(defn proportion-po
  [g strat]
  (double (/ (count (filter (fn [a] a)
                            (mapv (fn [b]
                                    (and (= 0.8 (first b))
                                         (= 0 (second b)))) strat))) (count g))))
;Returns the proportion of generous cooperators in the population at the end of a generation
(defn proportion-gc
  [g strat]
  (double (/ (count (filter (fn [a] a)
                            (mapv (fn [b]
                                    (and (= 0.8 (first b))
                                         (= 1 (second b)))) strat))) (count g))))




;Returns the average degree of all classical defectors in the population at the end of a generation
(defn degree-cd
  [g strat list]
  (loop [cd-degree-avg 0
         i 0]
    (if (>= i (count g))
      (if (= (count (filter true? list)) 0)
        0
        (double (/ cd-degree-avg (count (filter true? list)))))
      (recur
             (if (= true (nth list i))
               (+ cd-degree-avg (count (nth g i))) cd-degree-avg)
             (inc i)))))






; ------------------------------- Functions that return statistics at the end of a number of generations -------------------------------
; Run simulation across a set number of generations and returns the strategy matrix at the end.



(defn run-simulation-strategy
  [g rec strat generation-number update-number b c]
  (loop [i 0
         strat strat
         rec rec
         cumulative-strat []]
    (if (>= i generation-number)
      cumulative-strat
      (recur (inc i)
             (strategy-update g (resource-update g rec strat b c) strat update-number)
             (resource-initiate g 0)
             (conj cumulative-strat strat)))))




(defn run-simulation-proportion-cd
  [g rec strat generation-number update-number b c]
  (loop [i 0
         strat strat
         rec rec
         cd-prop []]
    (if (>= i generation-number)
      cd-prop
      (recur (inc i)
             (strategy-update g (resource-update g rec strat b c) strat  update-number)
             (resource-initiate g 0)
             (conj cd-prop (proportion-cd g strat))))))




(defn run-simulation-strategy
  [g rec strat generation-number update-number b c]
  (loop [i 0
         strat strat
         cumulative-strat []]
    (if (>= i generation-number)
      cumulative-strat
      (recur (inc i)
             (strategy-update g (resource-update g rec strat b c) strat  update-number)
             (conj cumulative-strat strat)))))


(defn run-simulation-strategy
  [g rec strat generation-number update-number b c]
  (loop [i 0
         strat strat
         cumulative-strat []]
    (if (>= i generation-number)
      cumulative-strat
      (recur (inc i)
             (strategy-update g (resource-update g rec strat b c) strat  update-number)
             (conj cumulative-strat strat)))))

; Run simulation across a set number of generations and returns the resource matrix at the end.
(defn run-simulation-resource
  [g rec strat generation-number b c]
  (loop [i 0
         rec rec]
    (if (>= i generation-number)
      rec
      (recur (inc i)
             (resource-update g rec strat b c)))))


(defn run-simulation-cd-payoff
  [g rec strat generation-number update-number b c cd]
  (loop [i 0
         payoff []]
    (if (>= i generation-number)
      payoff
      (recur (inc i)
             (conj payoff (average-payoff-CD rec strat cd))))))




; ------------------------------- Test-run code  -------------------------------


; Display the final strategy vector for a world of parameters listed below:
; Population size: 10000
; Number of generations: 20000
; Number of help initiation attempts in each generation: 1 per agent
; Number of agent strategy updates: 200
; Benefit received from a donation: 2
; Cost incurred from a donation: 1
(let [cd 50
      cc 0
      po 0
      g (generate-world 4 100)
      rec (resource-initiate g 0)
      strat (strategy-initiate g cd cc po)]
  (loop [i 0
         rec rec
         strat strat
         gc-proportion []
         avg-degree-cd []
         list (mapv (fn [b]
                      (and (= 0 (first b))
                           (= 0 (second b)))) strat)]
    (if (>= i 200)
      avg-degree-cd
      (recur (inc i)
             (resource-update g rec strat 1.5 1)
             (strategy-update g rec strat 20)
             (conj gc-proportion (proportion-gc g strat))
             (conj avg-degree-cd (degree-cd g strat list))
             (mapv (fn [b]
                     (and (= 0 (first b))
                          (= 0 (second b)))) strat)))))




; Display the final resource vector for a world of parameters listed below:
; Population size: 100
; Number of generations: 100
; Number of help initiation attempts in each generation: 10
; Number of agent strategy updates: 20
; Benefit received from a donation: 2
; Cost incurred from a donation: 1
(let [cd 50
      cc 0
      po 0
      g (generate-world 4 100)
      rec (resource-initiate g 0)
      strat (strategy-initiate g cd cc po)]
  (run-simulation-strategy g rec strat 100 20 5 1))


; Display the final strategy vector for a world of parameters listed below:
; Population size: 100
; Number of generations: 100
; Number of help initiation attempts in each generation: 10
; Number of agent strategy updates: 20
; Benefit received from a donation: 2
; Cost incurred from a donation: 1



; The Masuda paper displayed the following summary statistics:
; 1. Payoff of different strategies across generations
; 2. Mean degree of different strategies across generations
; 3. Fraction of each strategy within population across generations
; 4. Different update rules: Full imitation update vs. Fermi update rule
; 5. Different graphs: Scale-free vs. Regular random vs. Square lattice vs. Extended cycle

; Things I'm looking to explore apart from the above summary statistics:
; 1. The distribution of strategies within population weighted by number of neighbors(degree)
; 2. The average strategy within population with partial imitation learning











(use 'clojure.contrib.duck-streams)

(write-lines f
             (map #(str (% :title) ","  (% :year)) e))













