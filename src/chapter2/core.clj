(ns chapter2.core
  (:gen-class))


(use 'clojure.set)
(use 'clojure.math.numeric-tower)
(use '[clojure.algo.generic.functor :only (fmap)])

(def data
  {
   :Angelica {"Blues Traveler" 3.5
              "Broken Bells" 2
              "Norah Jones" 4.5
              "Phoenix" 5
              "Slightly Stoopid" 1.5
              "The Strokes" 2.5,
              "Vampire Weekend" 2}

   :Bill {"Blues Traveler" 2
          "Broken Bells" 3.5
          "Deadmau5" 4
          "Phoenix" 2
          "Slightly Stoopid" 3.5
          "Vampire Weekend" 3}

   :Chan {"Blues Traveler" 5
          "Broken Bells" 1
          "Deadmau5" 1
          "Norah Jones" 3
          "Phoenix" 5
          "Slightly Stoopid" 1}

   :Dan {"Blues Traveler" 3
         "Broken Bells" 4
         "Deadmau5" 4.5
         "Phoenix" 3
         "Slightly Stoopid" 4.5
         "The Strokes" 4
         "Vampire Weekend" 2}

   :Hailey {"Broken Bells" 4
            "Deadmau5" 1
            "Norah Jones" 4
            "The Strokes" 4
            "Vampire Weekend" 1}

   :Sam {"Blues Traveler" 5
         "Broken Bells" 2
         "Norah Jones" 3
         "Phoenix" 5
         "Slightly Stoopid" 4
         "The Strokes" 5}

   :Veronica {"Blues Traveler" 3
              "Norah Jones" 5
              "Phoenix" 4
              "Slightly Stoopid" 2.5
              "The Strokes" 3}

   :Jordyn {"Broken Bells" 4.5
            "Deadmau5" 4
            "Norah Jones" 5
            "Phoenix" 5
            "Slightly Stoopid" 4.5
            "The Strokes" 4
            "Vampire Weekend" 4}
   }
  )


(comment
  (defn common-users-ratings [user-1 user-2]
    (let [user-1-data (user-1 data)
          user-2-data (user-2 data)
          bands-in-common (keys (select-keys user-1-data (keys user-2-data)))
          users-ratings (fn [band] [(user-1-data band) (user-2-data band)])]
      (map users-ratings bands-in-common)))) ; for manhattan-distance

(def select-values (comp vals select-keys))

(defn common-users-ratings [user-1 user-2]
  (let [user-1-data (user-1 data)
        user-2-data (user-2 data)
        bands-in-common (keys (select-keys user-1-data (keys user-2-data)))]
    [(select-values user-1-data bands-in-common) (select-values user-2-data bands-in-common)]))


(defn manhattan-distance [ratings]
  (->> ratings
       (map (fn [[a b]] (abs (- a b))))
       (reduce + 0)))

(defn nearest-neighbor [user]
  (let [other-users (remove #{user} (keys data))]
    (->> other-users
         (map (fn [neighbor]
                [neighbor (apply pearson-correlation (common-users-ratings user neighbor))]))
         ;[neighbor (manhattan-distance (common-users-ratings user neighbor))])) ;for manhattan-distance
         (sort-by last >))))
;(sort-by last)))) for manhattan-distance

(defn exponential-sum-divided [ratings]
  (/ (expt (reduce + ratings) 2)
     (count ratings)))

(defn sum-exponentials [ratings]
  (reduce +
          (map #(expt % 2) ratings)))

(defn denominator-common-expression [ratings]
  (let [ratings-sum-exponentials (sum-exponentials ratings)
        ratings-exponential-sum-divided (exponential-sum-divided ratings)]
    (sqrt (- ratings-sum-exponentials ratings-exponential-sum-divided))))

(defn calc-denominator [ratings-1 ratings-2]
  (let [denominator-first-expression (denominator-common-expression ratings-1)
        denominator-second-expression (denominator-common-expression ratings-2)]
    (* denominator-first-expression denominator-second-expression)))


(defn calc-numerator [ratings-1 ratings-2]
  (let [numerator-first-expression (reduce + (map * ratings-1 ratings-2))
        ratings-1-sum (reduce + ratings-1)
        ratings-2-sum (reduce + ratings-2)
        numerator-second-expression (/ (* ratings-1-sum ratings-2-sum) (count ratings-1))]
    (- numerator-first-expression numerator-second-expression)))

(defn pearson-correlation [ratings-1 ratings-2] ;use let here
  (if (zero? (calc-denominator ratings-1 ratings-2)) ; and pattern matching
    0
    (/ (calc-numerator ratings-1 ratings-2)
       (calc-denominator ratings-1 ratings-2))))


(defn recommend [user k]
  (let [k-nearest-neighbors (take k (nearest-neighbor user)) ;change nearest to a map?
        neighbors-ratings (map #(second %) k-nearest-neighbors)
        distance (reduce + 0 neighbors-ratings)

        neighbor-and-weight (map (fn [[neighbor rating]]
                                   [neighbor (/ rating distance)]) k-nearest-neighbors)

        neighbors-and-not-in-common-bands (map (fn [[neighbor]]
                                                 (let [user-bands (keys (user data))
                                                       neighbor-data (neighbor data)]
                                                   {neighbor (apply dissoc neighbor-data user-bands)}))
                                               neighbor-and-weight)
        neighbors-and-not-in-common-bands-map (reduce merge {} neighbors-and-not-in-common-bands)

        recomendations (map (fn [[neighbor weight]]
                              (fmap (fn [rate]
                                      (* rate weight))
                                    (neighbor neighbors-and-not-in-common-bands-map)))
                            neighbor-and-weight)]

    (sort-by last > (apply merge-with + recomendations))))

(recommend :Hailey 1)
