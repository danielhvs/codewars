(ns codewars.core
  (:gen-class))

(def sobe "yes, ascending")
(def desce "yes, descending")
(def nao "no")

(defn subindo? [sq] 
  (cond (or (empty? sq) (nil? (second sq))) sobe
        (> (second sq) (first sq)) (subindo? (rest sq))
        :else nao))

(defn descendo? [sq] 
  (cond (or (empty? sq) (nil? (second sq))) desce
        (< (second sq) (first sq)) (descendo? (rest sq))
        :else nao))

(defn sorted-and-how? [sq]
  (cond 
   (> (second sq) (first sq)) (subindo? sq)
   (< (second sq) (first sq)) (descendo? sq)
   :else nao))

;(sorted-and-how? [1 2])
;(sorted-and-how? [15 7 3 -8])
;(sorted-and-how? [4 2 30])

(defn number [bus-stops]
  (reduce + (map #(- (first %) (second %)) bus-stops)))

; =5 (number [[10 0] [3 5] [5 8]])
; =17 (number [[3 0] [9 1] [4 10] [12 2] [6 1] [7 10]])
; =21 (number [[3 0] [9 1] [4 8] [12 2] [6 1] [7 8]])

(defn multiple? [number m]
  (zero? (rem number m)))

(defn solution [number]
  (reduce + 
          (map #(if (or (multiple? % 3) (multiple? % 5)) 
                   % 
                   0)
               (take number (range)))))


(def vowels #{\A \E \I \O \U \Y 
              \a \e \i \o \u \y})
(defn vowel-indices [word]
  (vec
   (filter #(> % 0)
           (map-indexed #(if (contains? vowels %2) (inc %1) 0) word))))

; 2,4
; (vowel-indices "super")

(comment
  (defn last [lst]
    (cond (= 1 (count lst)) (first lst)
          (empty? lst) nil
          :else (last (rest lst)))))

; (last [1 23 3])

