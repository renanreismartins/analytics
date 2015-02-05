(use 'clojure.set)

(def data
  {
   :David {"Imagine Dragons" 3.0
           "Daft Punk" 5.0
           "Lorde" 4.0
           "Fall Out Boy" 1.0}

   :Matt {"Imagine Dragons" 3.0
          "Daft Punk" 4.0
          "Lorde" 4.0
          "Fall Out Boy" 1.0}

   :Ben {"Kacey Musgraves" 4.0
         "Imagine Dragons" 3.0
         "Lorde" 3.0
         "Fall Out Boy" 1.0}

   :Chris {"Kacey Musgraves" 4.0
           "Imagine Dragons" 4.0
           "Daft Punk" 4.0
           "Lorde" 3.0
           "Fall Out Boy" 1.0}

   :Tori {"Kacey Musgraves" 5.0
          "Imagine Dragons" 4.0
          "Daft Punk" 5.0
          "Fall Out Boy" 3.0}
   }
  )



(def select-values (comp vals select-keys))


(defn user-ratings-with-common-bands [bands]
  (filter #(every? (second %) bands) data))


(defn to-user-and-average-rating [[user ratings]]
  (let [ratings (vals ratings)]
    {user {:average (/ (apply + ratings) (count ratings))}}))

(defn user-ratings-for-bands [bands [user band-ratings]]
  {user {:ratings (select-values band-ratings bands)}})

(defn bands-to-user-and-average-rating [bands]
  (let [v-user-ratings-with-common-bands (user-ratings-with-common-bands bands)
        users-and-average-rating (reduce conj {} (map to-user-and-average-rating v-user-ratings-with-common-bands))
        users-and-ratings (reduce conj {} (map (partial user-ratings-for-bands bands) v-user-ratings-with-common-bands))]
    (merge-with union users-and-average-rating users-and-ratings)))





(defn calc-normalized-ratings [ratings-and-average]
  (reduce (fn [acc rating]
            (* (- rating (:average ratings-and-average)))) (:ratings ratings-and-average)))

(calc-normalized-ratings {:ratings '(4.0 5.0), :average 4.25})


(user-ratings-with-common-bands ["Kacey Musgraves" "Imagine Dragons"])

(bands-to-user-and-average-rating ["Kacey Musgraves" "Imagine Dragons"])

