(ns techterm)
(use '[clojure.test :as t])


(with-test
  (defn hasWebAddr
     [line]
     (some #(not= -1 (.indexOf line %)) ["www." "http://"]))
  (is (hasWebAddr "url is http://abc.com")))

(with-test
  (defn hasBigNumber [words]
    (some #(or (> % 10) (> (- % (java.lang.Math/floor %)) 0))
               (for [i words :when (java.util.regex.Pattern/matches "[0-9]*" i)]
                 (try (java.lang.Float/parseFloat i) (catch Exception e -1)))))
  (is (hasBigNumber ["a" "11"]))
  (is (not (hasBigNumber ["a" "11.2312"]))))

;tofinish: hastechterm and repeatword
                          
(with-test
  (defn stripsplchar [word]
    (let [si (set (.split "([<`\"'.)`']>\":;,!?" ""))
          w (.split word "")]
      (clojure.string/join (for [i w :when (not (si i))] i))))
  (is (= "adfd" (stripsplchar "adfd().?<>"))))
                     
(defn haslinuxword 
  [chatmsg linuxwords]
  (some 
    (fn[x] (let [s (.toLowerCase (stripsplchar x))]
             (linuxwords s)))
    (.split (chatmsg :message) " ")))
  
(defn techTerm 
   [chatmsg linuxwords]
   (or (hasWebAddr (chatmsg :message))
       (hasBigNumber (chatmsg :words))
       (haslinuxword chatmsg linuxwords)))
       