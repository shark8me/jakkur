(ns mallet-iface)

(import '(cc.mallet.classify MaxEnt Classifier))
(import '(java.io ObjectInputStream BufferedInputStream FileInputStream File))
(import '(cc.mallet.types InstanceList))

(defn load-classifier
  "load the pre-trained classifier parameters"
  [infile]
  (.readObject (new ObjectInputStream 
                    (new BufferedInputStream 
                         (new FileInputStream 
                           infile)))))

(defn get-labels
  [classifier testfile]
  (for [i (.classify classifier (InstanceList/load  
                                  (new File testfile))) 
        :let [le (.getEntry (.getBestLabel (.getLabeling i)))]]
        le))

(comment
(let [cl (load-classifier "/home/kiran/sw/mallet-2.0.7/scrollback_data/my.classifier.trial9")]
  (get-labels cl "/home/kiran/sw/mallet-2.0.7/scrollback_data/test.mallet"))
        )                  