(ns jakkur)
(use '[clojure.test :as t])
(use '[localpath :as lp])
(use '[chatparse :as cp])
(use '[features :as frs])
(use '[default.core :as dc])
(use '[gen-mallet-format :as gmf])

(import '(cc.mallet.types Instance))

;kind of the main class
(defn svml-format
  "converts a map of features into Mallet Instances in svml format"
  [inp]
  (let [allfeats (dissoc inp :same)] 
    (new Instance 
         (clojure.string/join " "                              
                              (for [i frs/fkeys]
                                (str i ":" (if-let [k (allfeats i)] k 0))))
              (inp :same) nil nil)))

(defn get-mallet-format-line-closure
  "a closure that operates on a chat msg line"
  []
  (let [acu (cp/generate-msgs-perline)  
        featfn (frs/gen-features-closure)
        clfn (gmf/classify-closure (str lp/ldir "my.classifier.trial9"))]
    (fn [inline]
      (let [ik (featfn (acu inline))
            rval (if (empty? ik) []
                   (let [instances (map svml-format ik)]
                     (clfn (gmf/get-instancelist-line instances))))]        
    rval))))

(defn get-mallet-format
  [infile]
  (let [c1 (slurp (str lp/ldir "linux-dev-0X.annot"))
        iseq (.split c1 "\n")  
        featfn (get-mallet-format-line-closure)]
    (reduce into [] (map featfn  iseq))))

(comment
  (def g5 (first(get-mallet-format (str lp/ldir "linux-dev-0X.annot"))))
(try
  (count (get-mallet-format (str lp/ldir "linux-dev-0X.annot")))
  (catch Exception e (.printStackTrace e))))