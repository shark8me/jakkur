(ns gen-mallet-format)
(use '[mallet-iface :as iface])
;generate mallet-format data from svmlight data

(import '(cc.mallet.classify MaxEnt Classifier))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes))


(def ilist1 
(doto 
  (new InstanceList 
        (new SerialPipes 
              (doto 
                 (new java.util.ArrayList) 
                  (.add (new SvmLight2FeatureVectorAndLabel)  
               ))))
 (.addThruPipe (new Instance "dt_0:0 dt_1:0 neither_tech:0 dt_2:0 dt_3:0 curr_q:0 dt_4:0 prev_q:0 dt_5:0 dt_6:1 dt_7:0 prev_long:0 dt_8:0 curr_long:0 dt_9:0 prev_answer:0 curr_answer:0 dt_10:0 repeat_2:0 dt_11:0 one_tech:1 repeat_3:0 repeat_4:0 same_mention:0 both_tech:0 prev_mentions:0 same_spk:0 curr_mentions:1 curr_thx:0 prev_thx:0 curr_mentions_prev:0 curr_greet:0 prev_mentions_curr:1 prev_greet:0" 1 nil nil))  
 (.addThruPipe (new Instance "dt_0:0 dt_1:0 neither_tech:0 dt_2:0 dt_3:0 curr_q:0 dt_4:0 prev_q:0 dt_5:0 dt_6:0 dt_7:0 prev_long:0 dt_8:1 curr_long:0 dt_9:0 prev_answer:0 curr_answer:0 dt_10:0 repeat_2:0 dt_11:0 one_tech:0 repeat_3:0 repeat_4:0 same_mention:0 both_tech:1 prev_mentions:0 same_spk:0 curr_mentions:0 curr_thx:0 prev_thx:0 curr_mentions_prev:0 curr_greet:0 prev_mentions_curr:0 prev_greet:0" 0 nil nil))
 
  ))
 
(defn get-instancelist 
  [svml-input]
  (let [svmlin (.split (slurp svml-input) "\n")
        fnx (fn[lin] (let [all (.split lin " ")]
                           (new Instance (clojure.string/join " " (rest all))
                                (first all) nil nil))) 
        ilistseq (map fnx svmlin)
        ilist 
         (new InstanceList 
                   (new SerialPipes 
                         (doto 
                            (new java.util.ArrayList) 
                             (.add (new SvmLight2FeatureVectorAndLabel)  
                          ))))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist)
  )

(defn get-labels2
  [classifier ilist]
  (do (println (str " accuracy " (.getAccuracy classifier ilist)))
  (last (for [i (.classify classifier ilist) 
              :let [le (.getEntry (.getBestLabel (.getLabeling i)))]]
          le))))

(let [cl (iface/load-classifier "/home/kiran/sw/mallet-2.0.7/scrollback_data/my.classifier.trial9")]
  (get-labels2 cl
                (get-instancelist 
                 "/home/kiran/sw/mallet-2.0.7/scrollback_data/svml.test")))

(comment (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
 
    (println line)))