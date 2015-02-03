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



(defn common-ratings [band1 band2]
  (let [have-in-common (filter #(and (% band1)
                                     (% band2))
                               (vals data))]

    (map (fn [ratings]
           (vals ratings)) have-in-common)))


(defn user-average-rating [ratings]
  (/ (apply + ratings) (count ratings)))


(defn adjusted [band1 band2]
  (map user-average-rating (common-ratings band1 band2)))



(adjusted "Kacey Musgraves" "Imagine Dragons")

(common-ratings "Kacey Musgraves" "Imagine Dragons")
