(ns sb.core
  (:gen-class :main true))

(use '[sbformat :as sb])

(defn -main
  "The application's main function"
  [& args]
  (let [featfn (sb/get-room-closure)
        pp #(clojure.string/join " " (mapv (fn[x] ((last %) x)) [:msgid :ntid]))]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
      (println (pp (featfn line))))))
