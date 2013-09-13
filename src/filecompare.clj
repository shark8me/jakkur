(ns filecompare)

(def file1 (slurp "/tmp/clgen.feat"))
(def file2 (slurp "/home/kiran/sw/chat_dis/chat-distr/scratch/129/feats"))

(def stream2 
  (for [i (.split file2 "\n") :let [k (set (map first 
                                                (partition 2 (rest (.split i " ")))))]]
    k))

(def stream1 
  (for [i (.split file1 "\n") :let [k (set (map first 
                                                (partition 2 (.split i " "))))]]
    k))

;getting 1 error: line 1393
 (map (fn[[x y z]] z)
      (filter (fn[[x y z]](not= (count x) (count y))) 
              (map vector stream1 stream2 (iterate inc 1))))

