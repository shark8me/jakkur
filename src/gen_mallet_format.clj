(ns gen-mallet-format)
(use '[mallet-iface :as iface])
(use '[localpath :as lp])
(use '[features :as frs])
;generate mallet-format data from svmlight data

(import '(cc.mallet.classify MaxEnt Classifier MaxEntTrainer))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes 
                         TokenSequence2FeatureSequence
                         Target2Label
                         Csv2FeatureVector
                         Token2FeatureVector
                         ;SvmLight2Vectors
                         FeatureSequence2AugmentableFeatureVector))

(defn get-instancelist-line 
  "returns the InstanceList object"
  [instances]
  (let [ilist 
        (new InstanceList 
             (new SerialPipes 
                  (doto 
                    (new java.util.ArrayList) 
                    ;(.add (new Target2Label))
                    (.add (new SvmLight2FeatureVectorAndLabel))
                    )))]
    (doseq [i (vec instances)] 
      (.addThruPipe ilist i))
    ilist)
  )

(defn get-instancelist 
  [svml-input]
  (let [svmlin (.split (slurp svml-input) "\n")
        fnx (fn[lin] (let [all (.split lin " ")]
                           (new Instance (str (first all) (clojure.string/join " " (rest all)))
                                nil nil nil))) 
        ilistseq (map fnx svmlin)
        ilist 
         (new InstanceList 
                   (new SerialPipes 
                         (doto 
                            (new java.util.ArrayList) 
                            ;(.add (new Target2Label))
                            (.add (new SvmLight2FeatureVectorAndLabel)))))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist))

(defn get-instancelist2 
  [svml-input]
  (let [svmlin (.split (slurp svml-input) "\n")
        fnx (fn[lin] (let [all (.split lin " ")
                           iset (set (rest all))
                           istr (for [i frs/fkeys ]
                                  (str i ":" (if (iset i) 1 0)))] 
                       ;(println (str " all " all " f " (first all)))
                          (new Instance (str (first all) " " 
                                             (clojure.string/join " " istr))
                               ;(first all) 
                               nil nil nil))) 
        ilistseq (map fnx svmlin)
        ilist 
         (new InstanceList 
                   (new SerialPipes 
                         (doto 
                            (new java.util.ArrayList) 
                            ;(.add (new Target2Label))
                            (.add (new SvmLight2FeatureVectorAndLabel)))))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist))

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
  [train-instance-list]
  (let [ilist (get-instancelist2 train-instance-list)
        ;"/home/kiran/sw/ccomp/mallet/tf2.txt"
        trainer (doto (new MaxEntTrainer) (.train ilist 100))
        classifier (.getClassifier trainer)]
    (fn [il] (get-labels2 classifier il))))

(comment
  (spit "/tmp/res5.txt"
        (clojure.string/join "\n"
                             (let [ilist(get-instancelist2 "/home/kiran/sw/ccomp/mallet/tf2.txt")
                                   trainer (doto (new MaxEntTrainer) (.train ilist 100))
                                   classifier (.getClassifier trainer)]
                               (println (str " accuracy " (.getAccuracy classifier ilist) 
                                             ;(.getTarget (first ilist)) 
                                             " "(.getData (first ilist))))
                               (get-labels2 classifier ilist))))
  
  (spit "/tmp/res4.txt"
        (clojure.string/join "\n"
                             (let [flist "/home/kiran/sw/ccomp/mallet/tf2.txt"
                                   ilist(get-instancelist2 flist)
                                   xfn (classify-closure flist)]
                               ;(println (str " accuracy " (.getAccuracy classifier ilist)))
                               (xfn ilist))))



 (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
 
      (println line)))