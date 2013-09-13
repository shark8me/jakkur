(ns unigramstats)
(use '[clojure.test :as t])
(use '[chatparse :as cp])
(use '[localpath :as lp])

(def u1 (generate-msgs (str lp/ldir "linux-dev-0X.annot")))
        
(with-test
  (defn get-unigrams
    ([chat] (get-unigrams chat #{}))
    ([chat stopwords]  
      (apply merge-with #(+ %1 %2) 
             (map #(assoc {} % 1) 
               (reduce into [] 
                       (for [i chat :when (not (.equalsIgnoreCase "T-1" (i :thread))) ]
                         (for [w (i :words) :when (not  (stopwords w))]
                           w)))))))
  (is (= {"but" 1, "tells" 1, "in" 1, "meta" 1, "is" 1, "it" 1, "rtm" 1, "date" 1, "format" 1, "large" 1, "you" 1}
         (get-unigrams (take 6 u1)))))

(defn get-stopwords
  ([unigrams] (get-stopwords unigrams 50))
  ([unigrams topn]
    (set 
      (map first 
           (take topn
                 (into (sorted-map-by 
                         (fn [key1 key2]
                           (compare [(get unigrams key2) key2]
                                    [(get unigrams key1) key1])))
                       unigrams))))))

(defn get-unigram-probs
  [infile]
  (let [u1 (generate-msgs infile)
        unigrams (get-unigrams u1)
        stopwords (get-stopwords unigrams)
        finagram (get-unigrams u1 stopwords)
        tot (apply + (vals finagram))
        finagram2 (apply merge 
                         (map (fn[[k v]] {k (float (/ v tot))})
                              finagram))]
    {:unigrams finagram2 :stopwords stopwords
     :tot tot}
    ;(println (str "count " (apply + (vals unigrams)))))
  ))

(defn load-unigrams
  [infile]
  (let [f (.split (slurp infile) "\n")
        tot (apply + (for [i f :let [ v (java.lang.Integer/parseInt
                                          (last (.split i ",")))]] 
                       v))
        ug (apply merge 
                  (for [i f :let [[k v] (.split i ",")]] 
                    {k (float (/ (java.lang.Integer/parseInt v) tot))}))]
    (assoc {} :unigrams ug :tot (apply + (vals ug)))))

(try
  (let [ugramfin (get-unigram-probs (str lp/ldir "linux-dev-0X.annot"))] 
        [(count (:unigrams ugramfin)) (:tot ugramfin)]) 
(catch Throwable t (.printStackTrace t)))
 (:tot (load-unigrams (str lp/ldir "unigrams.txt")))


              