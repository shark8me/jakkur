(ns perline)
(use '[chatparse :as cp])
(use '[localpath :as lp])
;operate on a per-chat message basis

(defn iter1 [chats blocksize]
  (for [curr chats]
    (for [prev chats  
          :while (not= curr prev)
          :when (and (not (.equals "T-1" (curr :thread)))
                     (not (.equals "T-1" (prev :thread)))
                     (> blocksize (- (curr :timestamp) (prev :timestamp))))]
     [(curr :timestamp) (prev :timestamp)])))

(defn iter2 [chats blocksize prevchats]
  (for [prev (pop prevchats) :let [curr (peek prevchats)]
        :while (not= curr prev)
        :when (and (not (.equals "T-1" (curr :thread)))
                   (not (.equals "T-1" (prev :thread)))
                   (> blocksize (- (curr :timestamp) (prev :timestamp))))]
    [(curr :timestamp) (prev :timestamp)]))


(defn accumulator
  []
  (let [x (ref [])
        maxsize 100]
    (fn [y]
      (dosync (alter x (fn[j]
                         (conj 
                           (if (>= (count j) maxsize )
                             (subvec j (- (count j) maxsize)) j)                           
                           y)))))))
(def my-accumulator (accumulator))
(map my-accumulator (vec (range 10)))


(defn prev-lines-for-curr2
  [infile]
  (let [chats  (cp/generate-msgs infile)
        blocksize 129
        my-accumulator (accumulator)
        i2 (partial iter2 chats blocksize)]
    (for [i chats]
      (i2 (my-accumulator i))
    )))


(defn prev-lines-for-curr
  [infile]
  (let [chats  (cp/generate-msgs infile)
        blocksize 129]
    (iter1 chats blocksize)
    ))

(let [f1 (str lp/ldir "linux-dev-0X.annot")
      r1 (remove empty?
                 (prev-lines-for-curr2 f1))
      r2  (remove empty? 
                  (prev-lines-for-curr f1))]
  (do (println (str "length " (count r1) (count r2)))
    (let [[x y] (second
                  (remove nil? 
                    (map (fn[x y] (if (not= x y) [x y] nil)) 
                         r1 r2)))]
      (clojure.set/difference (set x) (set y))
                    )))

                          