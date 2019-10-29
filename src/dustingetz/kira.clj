(ns dustingetz.kira
  (:require
    [clojure.math.combinatorics]
    ))

; Double Booked
; When maintaining a calendar of events, it is important to know if an event overlaps
; with another event. Given a sequence of events, each having a start and end time,
; write a program that will return the sequence of all pairs of overlapping events.


(def xs [{::start #inst "2011-01-01T10:00:00.000Z" ::end #inst "2011-01-01T11:00:00.000Z" ::id "a1"}
         {::start #inst "2011-01-01T09:00:00.000Z" ::end #inst "2011-01-01T09:30:00.000Z" ::id "b"}
         {::start #inst "2011-01-01T09:30:00.000Z" ::end #inst "2011-01-01T10:00:00.000Z" ::id "c"}
         {::start #inst "2011-01-01T10:30:00.000Z" ::end #inst "2011-01-01T11:30:00.000Z" ::id "a2"}
         #_{::start #inst "2011-01-01T11:00:00.000Z" ::id "dangle1"}
         #_{::end #inst "2011-01-01T11:00:00.000Z" ::id "never started"}


         ; triple collision
         {::start #inst "2011-01-01T12:00:00.000Z" ::end #inst "2011-01-01T12:30:00.000Z" ::id "t1"}
         {::start #inst "2011-01-01T12:00:00.000Z" ::end #inst "2011-01-01T12:20:00.000Z" ::id "t2"}
         {::start #inst "2011-01-01T12:00:00.000Z" ::end #inst "2011-01-01T12:30:00.000Z" ::id "t3"}
         ])

(defn to-facts [record]
      (let [e (or (::id record) (hash record))]
           (->> record
                (map (fn [[a v]]
                         [e a v])))))

(comment
  (to-facts (first xs))
  (mapcat to-facts xs)
  )

(defn -e [[e a v]] e)
(defn -a [[e a v]] a)
(defn -v [[e a v]] v)

(def log (->> xs (mapcat to-facts)))
(def index (group-by -e log))
(defn resolve' [e]
      (->> (index e)
           (map (fn [[e a v]] [a v]))
           (into {})))

(comment
  (resolve' "a1")
  (resolve' "t2")
  (resolve' "b")
  )


(->> log
     (filter (fn [[e a v]]
                 (or (= a ::start) (= a ::end))))
     (group-by -v)                                          ; fixed point, handle each tick as a batch
     (sort-by first)
     (reduce (fn [[overlapping open] [_ chunk] #_[[e a v] ...]]
                 (let [{ending ::end starting ::start} (group-by -a chunk)
                       open (apply disj open (map -e ending)) ; Close ending events first
                       open (apply conj open (map -e starting)) ; Open starting events
                       overlapping (if (seq starting)         ; accumulate tuples of overlapping events
                                     (conj overlapping open)  ; track overlaps on leading edge
                                     overlapping)]
                      [overlapping open]))
             [[]                                            ; accumulated tuples
              #{}                                           ; buffer of open events
              ])
     (first)
     (filter #(> (count %) 1))
     (mapcat #(clojure.math.combinatorics/combinations % 2))
     (map #(map resolve' %)))
