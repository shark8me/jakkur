(ns threadtagger)
(use '[jakkur :as jk])
(use '[localpath :as lp])
(use '[clojure.test :as t])
;takes the output of the classifier and attaches a label to the chat message


(with-test 
  (defn get-tid 
    "gets the thread with the maximum score"
    [curthread scoremap]
    (let [top-score (reduce 
                      (fn [{:keys [tid score] :as m} [k v]] 
                        (if (> v score)
                          {:tid k :score v} m))
                          {:tid -1 :score -1} (if-let [smap scoremap] 
                                                smap {}))]
      (let [tid (:score top-score)] 
        (if (> tid 0) (:tid top-score) (inc curthread)) 
        )))
  (is (= 1 (get-tid 0 {})))
  (is (= 1 (get-tid 0 {1 0.43413429967509876})))
  (is (= 2 (get-tid 1 {1 -0.5507760112415543})))
  (is (= 2 (get-tid 1 {1 -1.2404095183367132 2 0.8230024181781685})))
  )
      
(let [inp (take 6 
              (jk/get-mallet-format (str lp/ldir "linux-dev-0X.annot")))
      inp2 (remove (fn[[{:keys [issys]} score]] issys)
                   inp)
      cleanfn (fn[[m scores]] 
                  [(dissoc m :thread ) 
                   (for [[c s] scores]
                     [(java.lang.Integer/parseInt c) (- 1 s)])])
      ptid (partial get-tid 34)]
  ;(map cleanfn inp2)
  (map
    (fn[lst]
      (ptid 
      (apply merge-with + 
                        (map (fn[[sc va tid]] {tid (- 1 va)}) lst))))
    (map second inp2)))
 
           (
 
 
 )