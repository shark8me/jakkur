(ns default.core
  (:import (cc.mallet.classify MaxEnt)))
;take the features file that is input to the megam classifier and
;transform it to Mallet's format
(defn get-feats [instr]
  (let [s (.split instr " ")
        klass (if (.equalsIgnoreCase (first s) "1") 1 0)
        part (partition 2 (rest s))]
   (assoc (zipmap (map first part) (map (fn[x] 1) part)) :klass klass)))

(defn get-all-feat-names [fname]
  "write a comma separated file, first is the sequence number or name,
   second is class (0 or 1), and then a list of comma separated features"
  (let [f1 (slurp fname)
        maps (map get-feats (.split f1 "\n"))
        fkeys (disj (reduce into #{} (map #(set (keys %)) maps)) :klass)
        rmunderscore (fn[x] (clojure.string/join (.split x "_")))]
    (clojure.string/join "\n" 
                         (map 
                           #(str %2 "\t" (%1 :klass) "\t"  
                                 (clojure.string/join  " " 
                                     (for [i fkeys] 
                                       (get % i 0))))  
                           maps (range (count maps)))
                         )))

(defn get-svmlight [fname]
  "write a comma separated file, class (0 or 1), and then a key:values"
  (let [f1 (slurp fname)
        maps (map get-feats (.split f1 "\n"))
        fkeys (disj (reduce into #{} (map #(set (keys %)) maps)) :klass)
        rmunderscore (fn[x] (clojure.string/join (.split x "_")))]
    (clojure.string/join "\n" 
                         (map 
                           #(str (%1 :klass) " "  
                                 (clojure.string/join  " " 
                                     (for [i fkeys] 
                                       (str i ":" (get % i 0)))))  
                           maps ))))
;write the file to 
(comment 
(spit "/home/kiran/sw/mallet-2.0.7/scrollback_data/svml.test"
      (get-svmlight
        "/home/kiran/sw/mallet-2.0.7/scrollback_data/test.feat"))

(spit "/home/kiran/sw/mallet-2.0.7/scrollback_data/svml.train"
      (get-svmlight
        "/home/kiran/sw/mallet-2.0.7/scrollback_data/train.feat"))
(spit "/tmp/cl.out"
      (get-all-feat-names
        "/home/kiran/Dropbox/scrollback/cloimpl/resources/feats"))
)


