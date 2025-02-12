(require
   hyrule [loop list-n])
(import
   hyrule [recur assoc inc]
   toolz [cons count complement first drop]
   time [sleep])


; (print
;  "hello world"
;  "second form")

; TODO: make naturals inductive
; TODO: make assoc return the updated coll

(defn rest [xs]
  (drop 1 xs))

(defn second [xs]
  (first (rest xs)))

(defn zero? [n]
  (= n 0))

(defn neg? [n]
  (< n 0))

(setv nonneg? (complement neg?))

(defn one? [n]
  (= n 1))

(setv not-zero? (complement zero?))

(setv alive? not-zero?)
(setv dead? zero?)

(defn make-row [n]
  (list-n n 0))

(defn make-board [n m]
  (list-n n (make-row m)))



(setv xs-test [1 2 3 4 5])
(setv xss-test [[1 2 3] [4 5 6] [7 8 9]])
(setv xss-test2 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
(setv xss-test3 [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(setv b (make-board 5 5))
; (assoc2 b 1 1 1)
; (assoc2 b 2 0 1)
; (assoc2 b 2 2 1)
; (assoc2 b 3 1 1)
; (assoc2 b 4 1 1)


(defmacro get2 [xss i j]
  `(get (get ~xss ~i) ~j))


(defmacro assoc2 [xss i j v]
  `(do
     (assoc (get ~xss ~i) ~j ~v)
     ~xss))


(defmacro lmap [f xs]
  `(list (map ~f ~xs)))

(defmacro lfilter [f xs]
  `(list (filter ~f ~xs)))

(defmacro lrange [n]
  `(list (range ~n)))


(defmacro lzip [xs ys]
  `(list (zip ~xs ~ys)))


(defmacro map2 [f xss]
  `(lmap (fn [xs] (lmap ~f xs)) ~xss))


(defn to-cell [x]
  (match x
    0 "O"
    1 "X"
    _ (raise NotImplementedError))) 


(defmacro map-cell [xs]
  `(lmap to-cell ~xs))


(defn list-to-str [xs]
  (str.join " " xs))


(defn lists-to-str [xss]
  (str.join "\n" (lmap list-to-str xss)))


(defn show-list [xs]
  (print (list-to-str xs)))


(defn show-lists [xss]
  (print (lists-to-str xss)))


(defn show-row [xs]
  (show-list (map-cell xs)))


(defn show-board [xss]
  (show-lists (map2 to-cell xss)))


(defn valid? [xss i j]
  (and (nonneg? i) (nonneg? j) (< i (len xss)) (< j (len (first xss)))))

(defn get-adj-idxs [xss i j]
  (let [idxs [[(- i 1) (- j 1)]
            [(- i 1) j]
            [(- i 1) (+ j 1)]
            [i (- j 1)]
            [i (+ j 1)]
            [(+ i 1) (- j 1)]
            [(+ i 1) j]
            [(+ i 1) (+ j 1)]]]
  (lfilter
   (fn [ij] (valid? xss #* ij))
   idxs)))


(defn get-adj [xss i j]
  (lmap (fn [ij] (get2 xss (first ij) (second ij))) (get-adj-idxs xss i j))
  )


(defn get-neighbors [xss i j]
  (lfilter alive? (get-adj xss i j)))


(defn count-neighbors [xss i j]
  (count (get-neighbors xss i j)))


(defn kill [xss i j]
  (assoc2 xss i j 0))

(defn spawn [xss i j]
  (assoc2 xss i j 1))

(defn map-cons [v xs]
  (lmap (fn [x] [v x]) xs))

(defn range2 [n m] 
  (let [js (lrange m)]
    (loop [[n n] [acc []]]
      (if (zero? n)
        acc
        (recur (- n 1) (+ (map-cons (- n 1) js) acc))))))

(defn get-idxs [xss] 
  (range2 (len xss) (len (first xss))))

; TODO: test
(defn undercrowded? [xss i j]
  (match (get2 xss i j)
    1 (let [n (count-neighbors xss i j)]
        (if (or (zero? n) (one? n))
          True
          False))
    0 False))


(defn overcrowded? [xss i j]
  (match (get2 xss i j)
    1 (let [n (count-neighbors xss i j)]
        (if (>= n 4)
          True
          False))
    0 False))


(defn unpopulated? [xss i j]
  (match (get2 xss i j)
    1 False
    0 (let [n (count-neighbors xss i j)]
        (if (= n 3)
          True
          False))))
  

(setv kill-rules [undercrowded? overcrowded?])

(setv spawn-rules [unpopulated?])


(defn empty? [xs]
  (zero? (count xs)))


(defn some [f xs]
  (not (empty? (lfilter f xs))))


(defn kill? [xss i j]
  (and (alive? (get2 xss i j)) (some (fn [f] (f xss i j)) kill-rules)))


(defn spawn? [xss i j]
  (and (dead? (get2 xss i j)) (some (fn [f] (f xss i j)) spawn-rules)))


(defn find-kill [xss]
  (loop [[idxs (get-idxs xss)] [acc []]]
    (match idxs
      [] (list acc)
      [[i j] #* xs] (if (kill? xss i j)
                     (recur xs (cons [i j] acc))
                     (recur xs acc)))))
  

(defn find-spawn [xss]
  (loop [[idxs (get-idxs xss)] [acc []]]
    (match idxs
      [] (list acc)
      [[i j] #* xs] (if (spawn? xss i j)
                     (recur xs (cons [i j] acc))
                     (recur xs acc)))))


(defn step [xss]
  (let [ks (find-kill xss)
        ss (find-spawn xss)]
    (for [[i j] ks] (kill xss i j))
    (for [[i j] ss] (spawn xss i j)))
  xss)


(defn play [xss n]
  (loop [[n n]]
    (if (zero? n)
      xss
      (do
        (show-board xss)
        (print "")
        (sleep 1)
        (step xss)
        (recur (- n 1))))))


(get2 xss-test 1 1) ; 5

(assoc2 xss-test 1 1 99)

(assoc2 xss-test 1 1 100)

(assoc2 b 1 1 99)

(assoc [1 2 3] 0 -1)


(defn factorial [n]
  (loop [[n n] [acc 1]]
    (if n
      (recur (- n 1) (* acc n))
      acc)))

(make-board 1 2)