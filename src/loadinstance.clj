(ns loadinstance
  (:import (cc.mallet.types InstanceList)
           (cc.mallet.classify MaxEntTrainer))
            )


(let [ilist (InstanceList/load (java.io.File. "/tmp/m"))
      maxent (doto (new MaxEntTrainer)
               (.setNumIterations 10)
               (.train ilist))]
  (.print maxent))
  

                       