(ns chatparse)
(use '[clojure.test :as t])
(use '[localpath :as lp])

;(def c1 (slurp "/home/kiran/sw/chat_dis/chat-dis/IRC/dev/linux-dev-0X.annot"))

(def hasgreet #{"hey", "hi", "hello"})
(def sys? #{"entered the room" "left the room" "mode (" "is now known as"})

(t/with-test
  (defn parseline [line]
    (let [l (.split line " ")
          mrestcol (nthrest l 4)
          mrest (clojure.string/join " " mrestcol)]
      (assoc {} :thread (first l) 
             :timestamp (java.lang.Integer/parseInt (second l))
             :speaker (.toLowerCase (nth l 2))
             :comment (.equals ":" (nth l 3))
             :message mrest
             ;:hasgreet (some hasgreet mrestcol)
             :issys (some #(not= -1 (.indexOf mrest %)) sys?)
             :hasq (not= -1 (.indexOf mrest "?"))
             ;:islong (> (count mrestcol) 10))
             )))
  (let [pres (parseline "T-1 7118 Felice *  is now known as Felice")]
    (is (true? (pres :issys)))
    (is (.equals "Felice" (pres :speaker)))
    (is (.equals "T-1" (pres :thread))))
  (let [pres (parseline "T3 15418 Chauncey :  person-to-person sales ?")]
    (is (nil? (pres :issys)))
    (is (true? (pres :hasq)))
    (is (.equals "Chauncey" (pres :speaker)))
    (is (.equals "T3" (pres :thread)))))

(defn parse-unthreaded-line
  "parses a chat message which has not been threaded"
  [line]
    (let [l (.split line " ")
          mrestcol (nthrest l 3)
          mrest (clojure.string/join " " mrestcol)]
      (assoc {} :thread 0 
             :timestamp (java.lang.Integer/parseInt (first l))
             :speaker (.toLowerCase (nth l 1))
             :comment (.equals ":" (nth l 2))
             :message mrest
             ;:hasgreet (some hasgreet mrestcol)
             :issys (some #(not= -1 (.indexOf mrest %)) sys?)
             :hasq (not= -1 (.indexOf mrest "?"))
             ;:islong (> (count mrestcol) 10))
             )))

(with-test 
  (defn make-words [iline]
    (let [l (.split iline " ")
          replSpcl (fn[w] (filter #(not (.equals % "")) 
                                  (.split (.trim (.replaceAll w "[^a-zA-Z0-9]" " ")) " ")))]
      (reduce into [] (map replSpcl (for [i l] (.toLowerCase i))))))
  (is (= ["meta" "but" "date" "is" "in" "large" "format"]
         (make-words "Meta, but $(date) is in large format")))
  (is (= ["date" "m" "d" "y"]
         (make-words "date '+%m%d%Y'")))
  (is (= ["rachel" "in" "the" "man" "page" "http" "fetchmail" "berlios" "de" "fetchmail" "man" "html" "it" "doesn" "t" "mention" "any" "global" "fetchmailrc" "file" "that" "is" "what" "was" "confusing" "me"]
         (make-words "Rachel: in the man page http://fetchmail.berlios.de/fetchmail-man.html it doesn't mention any global fetchmailrc file... that is what was confusing me..."))))

(with-test
  (defn decoratemsg
    "add words and mentioned keys to the chatmsg"
    [chatmsg speakers]
      (let [words (make-words (chatmsg :message))
            mentioned (set (filter speakers words))]
        (assoc chatmsg :mentioned mentioned
               :words (remove speakers words)
               :hasgreet (some hasgreet words)
               ;:hasq (not= -1 (.indexOf mrest "?"))
               :islong (> (count words) 10))))
    (let [pl (parseline "T-3 4324 Rachel : Malcolm: give yourself a promotion - You decide! ;-))")         
          res (decoratemsg pl #{"malcolm"})]
      (is (= '("give" "yourself" "a" "promotion" "you" "decide") (res :words)))
      (is (= #{"malcolm"} (res :mentioned)))))

(defn generate-msgs
  "generate chatmsg for all lines in file"
  [infile]
  (let [c1 (slurp infile)
        iseq (.split c1 "\n")
        accu (fn [] (let [x (ref #{})]
                      (fn [y] (dosync (alter x conj y)))))
        spkr-accu (accu)
        fnx (fn[chatmsg ]
              (let [spkrs (spkr-accu (chatmsg :speaker))]
                (decoratemsg chatmsg spkrs)))
        chats (map parseline iseq)]
    (map fnx chats)))

(with-test
  (defn generate-msgs-perline
    "generate fn to process chat msgs. Creates a closure and returns 
   a function that takes a chat line as input"
    [parsefn]
    (let [accu (fn [] (let [x (ref #{})]
                        (fn [y] (dosync (alter x conj y)))))
          spkr-accu (accu)
          fnx (fn[cm]
                (let [chatmsg (parsefn cm)
                      spkrs (spkr-accu (chatmsg :speaker))]
                  (decoratemsg chatmsg spkrs)))]
      fnx))  
  (is (= 7118 (:timestamp (let [c1 (slurp (str lp/ldir "linux-dev-0X.annot"))
                                iseq (first (.split c1 "\n"))
                                acu (generate-msgs-perline)]
                            (acu iseq))))))

;(take 2 (gm2 (str lp/ldir "linux-dev-0X.annot")))
(comment
(take 2 
      (generate-msgs 
        (str lp/ldir "linux-dev-0X.annot"))))
       