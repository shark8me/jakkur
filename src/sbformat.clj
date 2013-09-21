(ns sbformat)

(use '[jakkur :as jk])
(use '[clojure.test :as t])
(use '[localpath :as lp])
(use '[chatparse :as cp])
(use '[features :as frs])
(use '[default.core :as dc])
(use '[gen-mallet-format :as gmf])
(use '[threadtagger :as tagger])

(import '(cc.mallet.classify MaxEnt Classifier MaxEntTrainer))
(import '(cc.mallet.types InstanceList Instance))
(import '(cc.mallet.pipe SvmLight2FeatureVectorAndLabel SerialPipes))
;read in files in the sb format, ditto for output.


(t/with-test
  (defn sbformat-parseline
    "parse line in the scrollback input format example:
     0b4f4d10136a1cb5d0e52d384ac04949 15412 linux-dev Chauncey personal sales ?
      "
    [line]
    (let [l (vec (.split line " "))
          mrestcol (subvec l 4)
          mrest (clojure.string/join " " mrestcol)
          start 0]
      ;(println (str "parseline " line))
      (assoc {} :msgid (l start) 
             :timestamp (Long/parseLong (l (+ 1 start)))
             :room (l (+ 2 start))
             :speaker (l (+ 3 start))
             :message mrest
             :issys false
             :hasq (not= -1 (.indexOf mrest "?"))
             )))
  (let [pres (parseline "0b4f4d10136a1cb5d0e52d384ac04949 15412 linux-dev Chauncey personal sales ?")]
    (is (.equals "Chauncey" (pres :speaker)))
    (is (.equals "0b4f4d10136a1cb5d0e52d384ac04949" (pres :msgid)))
    (is (.equals "linux-dev" (pres :room)))
    (is (true? (pres :hasq)))
    (is (= 15412 (pres :timestamp)))
    (is (.equals "personal sales ?" (pres :message))))
  )

(with-test
  (defn chatmsg-holder
    "an accumulator that has multiple operations"
    []
    (let [x (ref [])
          maxsize 100]
      {:append (fn [y]
                 (dosync (alter x (fn[j]
                                    (conj 
                                      (if (>= (count j) maxsize )
                                        (subvec j (- (count j) maxsize)) j)                           
                                      y)))))
       :changelast (fn[k v]
                     (dosync (alter x 
                                    (fn[j]
                                      (conj (pop j)
                                            (assoc (peek j) k v))))))
       :curstate (fn[] @x)
       }))
  (is (= 10 (:v1 (peek  (let [ac (chatmsg-holder)
      imap (mapv #(hash-map :k %1 :v %2) (range 10) (range 10 20)) ]
                    (doseq [i imap]
                      ((ac :append) i))
                    ((ac :changelast) :v1 10)
                    ((ac :curstate))))))))

(defn read-train-instancelist
  "read in an instancelist used for training the classifier"
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
                            (.add (new SvmLight2FeatureVectorAndLabel)))))]
         (doseq [i (vec ilistseq)] 
           (.addThruPipe ilist i))
         ilist))

(defn get-label
  "given a classifier and an InstanceList, returns the predicted labels"
  [classifier ilist]
  (let [res (for [i (.classify classifier ilist) 
                  :let [le (.getLabelVector i)
                        lab (.getEntry (.getBestLabel le))
                        v (.getBestValue le)]]
         (do     ;(println (str " val " v))
              [lab (if (.equals lab "1") v (- 1 v))]))]
    res))

(defn classify-closure-sbformat
  "returns a closure that classifies instances for each chat msg"
  [train-instance-list]
  (let [ilist (get-instancelist2 train-instance-list)
        trainer (doto (new MaxEntTrainer) (.train ilist 100))
        classifier (.getClassifier trainer)]
    (fn [il] (get-label classifier il))))

(def commoncl (classify-closure-sbformat (str lp/ldir "tf2.feat")))
(defn single-room-closure
  "a closure that operates on a chat msg line"
  [parsefn]
  (let [accuobj (chatmsg-holder)
        acu (cp/generate-msgs-perline parsefn)  
        featfn (frs/gen-features-closure-sbformat (accuobj :append))
        ;clfn (classify-closure-sbformat (str lp/ldir "tf2.feat"))
        tidfn (tagger/score-tid-closure 0)]
    (fn [inline]
      (let [parsedmsg (acu inline)
            ik (featfn parsedmsg)
            rval (if (empty? ik) []
                   (let [svmli (map jk/svml-format ik)
                         instances (gmf/get-instancelist-line svmli)
                         cl2 (commoncl instances)]
         ;            (println (str " instances " parsedmsg))
                     (map #(conj %1 %2) cl2 (map :tid ik))))
            ntid (tidfn [parsedmsg rval])]
        ;(println (str " curstate " (vec rval)))
        ((accuobj :changelast) :ntid ntid)))))

(defn multi-room-closure
  "a closure that operates on a chat msg line"
  []
  (let [accuobj (chatmsg-holder) 
        featfn (frs/gen-features-closure-sbformat (accuobj :append))
        tidfn (tagger/score-tid-closure 0)]
    (fn [parsedmsg]
      (let [ik (featfn parsedmsg)
            rval (if (empty? ik) []
                   (let [svmli (map jk/svml-format ik)
                         instances (gmf/get-instancelist-line svmli)
                         cl2 (commoncl instances)]
                     ;(println (str " instances " parsedmsg))
                     (map #(conj %1 %2) cl2 (map :tid ik))))
            ntid (tidfn [parsedmsg rval])]
        ;(println (str " curstate " (vec rval)))
        ((accuobj :changelast) :ntid ntid)))))

(defn get-room-closure
  ""
  []
  (let [rooms (ref {})
        acu (cp/generate-msgs-perline sbformat-parseline)
        ;clfn (classify-closure-sbformat (str lp/ldir "tf2.feat"))
        ]
    (fn [inline]
      (let [pmsg (acu inline)
            roomname (:room pmsg)]
        (if-let [rfn (rooms roomname)] (rfn pmsg)
          (let [newcl (multi-room-closure )]
            (dosync (alter rooms (fn[x] 
                                   (assoc x roomname newcl))))
            (newcl pmsg))))))
        )

(defn get-sb-format-multi-room
  [infile]
  (let [c1 (slurp infile)
        iseq (.split c1 "\n")  
        featfn (get-room-closure)]
      (map featfn iseq)))

(defn get-sb-format-single-room
  [infile]
  (let [c1 (slurp infile)
        iseq (.split c1 "\n")  
        featfn (single-room-closure sbformat-parseline)]
      (map featfn iseq)))

(defn get-sb-outputformat
  "generate output in sb format"
  [formatfn outfile infile]
  (spit outfile
        (clojure.string/join "\n"
        (mapv #(clojure.string/join " " (mapv (fn[x] ((last %) x)) [:msgid :ntid]))  
             (formatfn infile)))))

(defn interleave-file
  [file1 file2 opfile]
  (let [fread (fn[x] (.split (slurp x) "\n"))
        [f1 f2] (map fread [file1 file2])
        [c1 c2] (map count [f1 f2])]
    (spit opfile
          (clojure.string/join "\n"
                               (vec 
                                 (concat (interleave f1 f2) 
                                         (if (> c1 c2) (nthrest f1 c2) (nthrest f2 c1))))))))

(defn verify-interleaved-results
  []
  (let [mmap (fn [ifile]
               (apply merge
                      (for [i (.split (slurp ifile) "\n")
                            :let [[k1 k2]  (.split i " ")]] 
                        {k1 k2})))
        allmaps (apply merge (map mmap ["C:\\temp\\sbo_dev.txt"
                                        "C:\\temp\\sbo_test.txt"]))
        combmap (mmap "C:\\temp\\sbo_inter_output.txt")]
    (println (str " a " (count allmaps) " s " (count combmap)))
    (and (= (count allmaps) (count combmap))
         (every? combmap (keys allmaps))
         (every? #(= (combmap %) (allmaps %)) (keys allmaps)))
  ))

(verify-interleaved-results)
(comment
(interleave-file (str lp/ldir "linux-dev-sbform.txt")
                 (str lp/ldir "linux-test-sbform.txt")
                 "C:\\temp\\sbo_interleaved.txt")
    
(get-sb-outputformat get-sb-format-single-room 
                     "C:\\temp\\sbo_dev.txt"
                     (str lp/ldir "linux-dev-sbform.txt"))
(get-sb-outputformat get-sb-format-single-room 
                     "C:\\temp\\sbo_test.txt"
                     (str lp/ldir "linux-test-sbform.txt"))

(get-sb-outputformat get-sb-format-multi-room 
                     "C:\\temp\\sbomultrm.txt"
                     (str lp/ldir "linux-devtest-sbform.txt"))
)
;interleaved
(try
(get-sb-outputformat get-sb-format-multi-room 
                     "C:\\temp\\sbo_inter_output.txt"
                     "C:\\temp\\sbo_interleaved.txt")
(catch Exception e(.printStackTrace e)))
