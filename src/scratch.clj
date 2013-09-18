(ns scratch)
(use '[mallet-iface :as iface])
(use '[localpath :as lp])
(use '[chatparse :as cp])
(use '[features :as frs])
(use '[gen-mallet-format :as gmf])
;generate mallet-format data from svmlight data

(import '(cc.mallet.classify MaxEnt Classifier MaxEntTrainer))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes 
                         TokenSequence2FeatureSequence
                         Target2Label
                         Csv2FeatureVector
                         Token2FeatureVector
                         FeatureSequence2AugmentableFeatureVector))
;scratchpad.

(defn list-format2
  "converts a map of features into Mallet Instances, only a list 
   of feature names are used."
  [inp]
  (let [allfeats (dissoc inp :same)
        feats (clojure.string/join " " (map str 
                                            (map (fn[[x y]] x) 
                                                 (filter (fn[[x y]] (= 1 y))
                                                         allfeats))))]
    (println (str " feats " feats " " ))
    (new Instance  feats (str (inp :same)) nil nil)))

(defn get-mallet-format-line-closure2
  "a closure that operates on a chat msg line"
  []
  (let [acu (cp/generate-msgs-perline cp/parseline)  
        featfn (frs/gen-features-closure)
        ;clfn (gmf/classify-closure "/home/kiran/sw/ccomp/mallet/tf2.txt")
        ]
    (fn [inline]
      (let [ik (featfn (acu inline))]        
        (gmf/get-instancelist-line (map list-format2 ik))))))

(comment
(let [flist "/home/kiran/sw/ccomp/mallet/tf2.txt"
      ilist(gmf/get-instancelist2 flist)]
  (for [i (take 2 ilist)]
    (println (str " data " (.getData i) " target " (.getTarget i) ))))
 )
(try 
(let [flist (str lp/ldir "linux-dev-0X.annot")
      c1 (slurp flist)
      iseq (.split c1 "\n")
      xfn (get-mallet-format-line-closure2)]
  (for [i (take 7 iseq) :let [j (xfn i)] :when (not-empty j)]
    (for [k j]
      (println (str " data " (.getData k) " target " (.getTarget k )))
      ;j
      )))
(catch Exception e (.printStackTrace e)))
    
