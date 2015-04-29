(ns sb.core
  (:gen-class :main true))

(use '[sbformat :as sb])

(defn -main
  "The application's main function"
  [inpfile & args]
  (let [featfn (sb/get-room-closure)
        pp #(clojure.string/join " " (mapv (fn[x] ((last %) x)) [:msgid :ntid]))]
    (with-open [rdr (clojure.java.io/reader inpfile)]
    (doseq [line (line-seq rdr)]
      (println (pp (featfn line)))))))
