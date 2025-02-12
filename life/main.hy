(require
   hyrule [defmain loop]
   life.game [lmap])

(import
  hyrule [recur]
  toolz [first]
  life.game [make-board show-board valid? second spawn play])

(defn read-starting-cells [xss] 
  (loop []
    (show-board xss)
    (print "Choose a cell to spawn (enter q to quit):" :end " ")
    (let [ip (input)]
      (match ip
        "q" (print "Starting...")
        _ (let [ij (lmap int (str.split ip " "))]
            (if (valid? xss (first ij) (second ij))
              (spawn xss (first ij) (second ij))
              (print "Invalid cell. Try again."))
            (recur))))))

; (defn main [n m]
;   (print "Welcome to life"))

(defmain [life n m]
  (print "Welcome to the Game of Life!")
  (print "Board size:" n "x" m)
  (setv b (make-board (int n) (int m)))
  (read-starting-cells b)
  (play b 100)
;   (print "Choose a cell to spawn (x y):")
  )