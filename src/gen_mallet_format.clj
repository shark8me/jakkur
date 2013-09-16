(ns gen-mallet-format)
(use '[mallet-iface :as iface])
(use '[localpath :as lp])
;generate mallet-format data from svmlight data

(import '(cc.mallet.classify MaxEnt Classifier MaxEntTrainer))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes 
                         TokenSequence2FeatureSequence
                         Target2Label
                         Csv2FeatureVector
                         Token2FeatureVector
                         FeatureSequence2AugmentableFeatureVector))

(defn get-instancelist-line 
  "returns the InstanceList object"
  [instances]
  (let [ilist 
        (new InstanceList 
             (new SerialPipes 
                  (doto 
                    (new java.util.ArrayList) 
                    (.add (new Target2Label))
                    (.add (new Csv2FeatureVector))
                    )))]
    (doseq [i (vec instances)] 
      (.addThruPipe ilist i))
    ilist)
  )

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
                    ;        (.add (new SvmLight2FeatureVectorAndLabel))
                            (.add (new Target2Label))
                           (.add (new Csv2FeatureVector))
                    ;(.add (new Token2FeatureVector))
                    ;(.add (new TokenSequence2FeatureSequence))
                    ;(.add (new FeatureSequence2AugmentableFeatureVector))                            
                                  )))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist))



(comment
  (for [i (.classify classifier ilist) 
              :let [le(.getLabeling i)]]
    (let [k (.getEntry (.getBestLabel le))]      
    [k
    (if (.equals k "1")
      (.valueOfCorrectLabel i)
      (- 1 (.valueOfCorrectLabel i)))
                    ]
    ))
  (for [i (take 5 (get-instancelist "/home/kiran/sw/ccomp/mallet/tf2.txt"))]
  (first (.getAlphabets i)))
(clojure.string/join " "
                     (.size (.getAlphabet 
                            (first (get-instancelist 
                                     ;"/home/kiran/sw/ccomp/mallet/tf2.txt"
                                     (str lp/ldir "svml.train")
                                     )))))

(spit "/tmp/res3.txt"
      (clojure.string/join "\n"
(let [ilist(get-instancelist "/home/kiran/sw/ccomp/mallet/tf2.txt")
      trainer (doto (new MaxEntTrainer)
                (.train ilist 100))
      classifier (.getClassifier trainer)
      ]
  (println (str " done ?" (.isFinishedTraining trainer) 
                " precision " (.getAccuracy classifier ilist)))
  (println (str " classifier feats " 
                (vec (for [i (.getPerClassFeatureSelection classifier)]
                       (.toString i)))))
                (get-labels2 classifier ilist)
                )))

(let [ilist(get-instancelist "/home/kiran/sw/ccomp/mallet/tf2.txt")
      trainer (doto (new MaxEntTrainer) (.train ilist 100))
      classifier (.getClassifier trainer)
      ;classifier (iface/load-classifier "/home/kiran/sw/ccomp/mallet/mallet.classifier.trial9")
      ]
  (println (str " classifier feats " (.size (.getAlphabet classifier)) " "
                (.size (.getAlphabet (first ilist)))
                  ;(vec (for [i (.getAlphabets classifier)] i))
                  )
           )))

  (defn get-labels2
    "given a classifier and an InstanceList, returns the predicted labels"
    [classifier ilist]
    (let [res (for [i (.classify classifier ilist) 
                    :let [le (.getLabelVector i)
                          lab (.getEntry (.getBestLabel le))
                          v (.getBestValue le)]]      
                [lab (if (.equals lab "1") v (do ;(println " lab " lab) 
                                               (- 1 v)))])]
      res))

  (defn classify-closure
    [classifier-parameters]
    (let [;cl (iface/load-classifier classifier-parameters)
          ilist(get-instancelist "/home/kiran/sw/ccomp/mallet/tf2.txt")
          trainer (doto (new MaxEntTrainer) (.train ilist 100))
          classifier (.getClassifier trainer)]
      (fn [ilist] (get-labels2 classifier ilist))))

(comment
  (spit "/tmp/res3.txt"
        (clojure.string/join "\n"
                             (let [ilist(get-instancelist "/home/kiran/sw/ccomp/mallet/tf2.txt")
                                   trainer (doto (new MaxEntTrainer) (.train ilist 100))
                                   classifier (.getClassifier trainer)
                                   cl (iface/load-classifier 
                                        ;(str lp/ldir "my.classifier.trial9")
                                        "/home/kiran/sw/ccomp/mallet/mallet.classifier.trial9"
                                        )
                                   ]
                               (println (str " accuracy " (.getAccuracy classifier ilist)))
                               (get-labels2 classifier ilist))))



 (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
 
      (println line)))