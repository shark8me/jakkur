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
  (let [allfeats (dissoc inp :same)
        inpstr (str (inp :same) " " 
                    (clojure.string/join " "                              
                                         (for [i frs/fkeys]
                                           (str i ":" (if-let [k (allfeats i)] k 0)))))]
    ;(println (str " svml-format " inpstr))
    ;(println (str " inp-map  " inp))
    (new Instance 
         inpstr
         ;(inp :same)
         nil nil nil)))

(defn list-format
  "converts a map of features into Mallet Instances, only a list 
                 of feature names are used."
  [inp]
  (let [allfeats (dissoc inp :same)
        feats (clojure.string/join " " (map str 
                                            (map (fn[[x y]] x) 
                                                 (filter (fn[[x y]] (= 1 y))
                                                         allfeats))))]
    ;(println (str " feats " feats " " ))
    (new Instance feats (str (inp :same)) nil nil)))
;(fn[lin] (let [all (.split lin " ")]
;                           (new Instance (clojure.string/join " " (rest all))
;                                (first all) nil nil)))

(defn get-mallet-format-line-closure
  "a closure that operates on a chat msg line"
  [parsefn]
  (let [acu (cp/generate-msgs-perline parsefn)  
        featfn (frs/gen-features-closure)
        clfn (gmf/classify-closure "/home/kiran/sw/ccomp/mallet/tf2.txt")]
    (fn [inline]
      (let [ik (featfn (acu inline))
            rval (if (empty? ik) []
                   (let [instances (gmf/get-instancelist-line
                                     (map svml-format ik))]
                     ;(doseq [ins instances]
                       ;(println (str " instance " (.getData ins)
                       ;              (.getTarget ins))))
                     (clfn  instances)))]        
    [inline rval]))))

(defn get-mallet-format
  [infile]
  (let [c1 (slurp infile)
        iseq (.split c1 "\n")  
        featfn (get-mallet-format-line-closure cp/parseline)]
    ;(println (str "ff " (first iseq)))
    (map featfn   iseq)))

(try
  (spit "/tmp/t2.txt" 
        (clojure.string/join "\n"
        (reduce into [] (mapv second 
                              (filter (fn[[x y]] (not-empty y))
                                       (get-mallet-format 
                                         (str lp/ldir "linux-dev-0X.annot")
                                         ))))))
 
       (catch Exception e (.printStackTrace e)))