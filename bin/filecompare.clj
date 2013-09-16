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

;compare the accuracy from different predictions, the first column is the gold standard.
(let [iseq (.split (slurp "/home/kiran/sw/ccomp/mallet/tog4.txt") "\n")
      fnx (fn[x] (vec (.split x ",")))
      gs (map #((fnx %) 0) iseq)
      megam (map #((fnx %) 1) iseq)
      mallet (map #((fnx %) 2) iseq)
      code (map #((fnx %) 3) iseq)
      totcount (count gs)
      fny (fn[inseq ] 
            (float (/ 
                     (count (filter (fn [[x y]] (= x y)) (map vector gs inseq)))
                     totcount)))] 
  (str "megam " (fny megam) " mallet " (fny mallet) "code " (fny code)))

(spit "/home/kiran/sw/ccomp/mallet/pred2.output"
      (clojure.string/join "\n"
                           (let [iseq (.split (slurp 
                                                "/home/kiran/sw/ccomp/mallet/predicted.output") 
                                        "\n")
                                 fnx (fn[x]
                                       (let [iseq2 (vec(.split x "\t"))
                                             oneval (new BigDecimal (iseq2 2))
                                             zeroval (new BigDecimal (iseq2 4))]
                                         (if (> oneval zeroval) 
                                           (str "1 " oneval)
                                           (str "0 " oneval))))]
                             (map fnx iseq
                             ))))