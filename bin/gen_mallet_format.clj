(ns gen-mallet-format)
(use '[mallet-iface :as iface])
(use '[localpath :as lp])
;generate mallet-format data from svmlight data

(import '(cc.mallet.classify MaxEnt Classifier))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes))

(defn get-instancelist-line 
  "returns the InstanceList object"
  [instances]
  (let [ilist 
        (new InstanceList 
             (new SerialPipes 
                  (doto 
                    (new java.util.ArrayList) 
                    (.add (new SvmLight2FeatureVectorAndLabel)  
                      ))))]
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
                             (.add (new SvmLight2FeatureVectorAndLabel)  
                          ))))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist)
  )

(defn get-labels2
  "given a classifier and an InstanceList, returns the predicted labels"
  [classifier ilist]
  (do ;(println (str " accuracy " (.getAccuracy classifier ilist)))
    (for [i (.classify classifier ilist) 
                :let [le (.getEntry (.getBestLabel (.getLabeling i)))]]
            le)))

(defn classify-closure
  [classifier-parameters]
  (let [cl (iface/load-classifier classifier-parameters)]
    (fn [ilist] (get-labels2 cl ilist))))

(let [cl (iface/load-classifier (str lp/ldir "my.classifier.trial9"))]
  (get-labels2 cl
                (get-instancelist 
                 (str lp/ldir "svml.test"))))

(comment (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
 
    (println line)))