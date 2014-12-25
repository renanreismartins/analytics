(ns chapter2.core
  (:gen-class))


(use 'clojure.set)
(use 'clojure.math.numeric-tower)


(def data
	{:Hailey {"Broken Bells" 4,
         "Deadmau5" 1,
         "Norah Jones" 4,
         "The Strokes" 4,
         "Vampire Weekend" 1}

	 :Veronica {"Blues Traveler" 3,
            "Norah Jones" 5,
            "Phoenix" 4,
            "Slightly Stoopid" 2.5,
            "The Strokes" 3}
  	 :Jordyn {"Broken Bells" 4.5,
              "Deadmau5" 4,
              "Norah Jones" 5,
              "Phoenix" 5,
              "Slightly Stoopid" 4.5,
              "The Strokes" 4,
              "Vampire Weekend" 4.0}}
  )


(def select-values (comp vals select-keys))

(defn common-users-ratings [user-1 user-2]
  (let [user-1-data (user-1 data)
        user-2-data (user-2 data)
        data-intersection (intersection (set (keys user-1-data))
              (set (keys user-2-data)))]
    [(select-values user-1-data data-intersection)
    (select-values user-2-data data-intersection)]))



(defn manhattan-distance [ratings-a ratings-b]
  (reduce +
          (map (comp abs -) ratings-a ratings-b)))



(defn nearest-neighbor [user]
  (let [other-users (remove (hash-set user) (keys data))]
  (sort-by last (apply merge
                      (map (fn x [neighbor]
         					{neighbor (apply manhattan-distance
                				(common-users-ratings user neighbor))})
                   		other-users)))))


(defn recommend [user]
  (let [nearest (first (first (nearest-neighbor user)))]
	(sort-by last > (apply merge(map (fn x [band]
           {band (get (nearest data) band)})
         (remove (set (keys (user data)))
			(set (keys(nearest data))))))))
  )

(recommend :Hailey)



(defn sum-exponentials-elements [ratings]
  (reduce +(map (fn [rating] ;#(expt % 2)
                  (expt rating 2)) ratings)))

(defn exponential-sum-divided [ratings ratings-sum]
  (/ (expt ratings-sum 2) (count ratings)))

(defn denominator-common-expression [ratings-sum-exponentials-elements ratings-exponential-sum-divided]
  (sqrt (- ratings-sum-exponentials-elements ratings-exponential-sum-divided)))

(defn pearson-correlation [ratings-1 ratings-2]
  (let [numerator-first-expression (reduce + (map * ratings-1 ratings-2))
        ratings-1-sum (reduce + ratings-1)
        ratings-2-sum (reduce + ratings-2)
        numerator-second-expression (/ (* ratings-1-sum ratings-2-sum) (count ratings-1))
        numerator (- numerator-first-expression numerator-second-expression)


        ratings-1-sum-exponentials-elements (sum-exponentials-elements ratings-1)
        ratings-1-exponential-sum-divided (exponential-sum-divided ratings-1 ratings-1-sum)
        denominator-first-expression (denominator-common-expression ratings-1-sum-exponentials-elements ratings-1-exponential-sum-divided)


        ratings-2-sum-exponentials-elements (sum-exponentials-elements ratings-2)
        ratings-2-exponential-sum-divided (exponential-sum-divided ratings-2 ratings-2-sum)
        denominator-second-expression (denominator-common-expression ratings-2-sum-exponentials-elements ratings-2-exponential-sum-divided)

        denominator (* denominator-first-expression denominator-second-expression)]
    (/ numerator denominator)))




(pearson-correlation (list 4.75 4.5 5 4.25 4) (list 4 3 5 2 1))
