(ns formatconverter)
(use '[clojure.test :as t])
(use '[localpath :as lp])
(use '[chatparse :as cp])
;convert training files from elsner-charniak to sb

(defn convert-to-sb-format
  "converts file from original to sb format"
  [roomname infile outfile]
  (spit outfile
  (clojure.string/join "\n"
  (let [x (.split (slurp infile) "\n")]
    (vec 
      (for [{:keys [timestamp speaker message] :as imap} (map cp/parse-unthreaded-line x)
          :let [uid (.toString (java.util.UUID/randomUUID))]
          :when (not (:issys imap))]
        (clojure.string/join " "
                             [uid timestamp roomname speaker (.trim message)])))))))

(convert-to-sb-format "linux-dev"
                      (str lp/ldir "linux-dev.txt")
                      (str lp/ldir "linux-dev-sbform.txt"))

(convert-to-sb-format "linux-test"
                      (str lp/ldir "linux-test.txt")
                      (str lp/ldir "linux-test-sbform.txt"))