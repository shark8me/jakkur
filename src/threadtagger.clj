(ns threadtagger)
(use '[jakkur :as jk])
(use '[localpath :as lp])
(use '[clojure.test :as t])
;takes the output of the classifier and attaches a label to the chat message


(with-test 
  (defn get-tid 
    "gets the thread with the maximum score"
    [tidstart]
    (let [threadid (atom tidstart)] 
      (fn[scoremap]
        (let [top-score (reduce 
                          (fn [{:keys [tid score] :as m} [k v]] 
                            (if (> v score)
                              {:tid k :score v} m))
                          {:tid -1 :score -1} (if-let [smap scoremap] 
                                                smap {}))]
          ;(println (str " scoremap " scoremap))
      (let [tid (:score top-score)] 
        (if (> tid 0) (:tid top-score) (swap! threadid inc)) 
        )))))
  (is (= 1 (get-tid 0 {})))
  (is (= 1 (get-tid 0 {1 0.43413429967509876})))
  (is (= 2 (get-tid 1 {1 -0.5507760112415543})))
  (is (= 2 (get-tid 1 {1 -1.2404095183367132 2 0.8230024181781685})))
  )
      
(defn score-tid-closure
  "massages the input (a vector or [prediction, score, tid] 
  to call get-tid"
  [tidstart]
  (let [get-tidfn (get-tid tidstart)]
    (fn[[{:keys [issys] :as msgmap} lst]]
      (let [ptidinp (apply merge-with + 
                           (map (fn[[sc va tid]]
                                  (do ;(println (str " aa " sc " " va " " tid))
                                    {tid (- va 0.5)})) 
                                lst))
            ]
    ;(println (str (msgmap :message)))
    (if issys "T-1"
      (get-tidfn ptidinp)
      ;lst
      )))))
 
           (comment
 (try 
   (let [inps (map (fn[l] (str (:ntid l)" " (:speaker l)" " (:message l)))
                   (remove #(.equals (:ntid %) "T-1") 
                           (jk/get-mallet-format (str lp/ldir "linux-dev-0X.annot"))))]
     (spit "c:\\temp\\t5.txt" (clojure.string/join "\n"  inps))
     ;(count (take 50 inps))
     )   
   (catch Exception e(.printStackTrace e))))
   