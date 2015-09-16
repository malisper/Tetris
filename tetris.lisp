(defpackage :tetris
  (:use :clamp :experimental :iter)
  (:shadow :down)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod))

(in-package :tetris)
(syntax:use-syntax :clamp)

(defstruct (game (:conc-name nil) (:type vector) (:constructor make-game%))
  piece
  bag
  (board (make-array (list height* width*) :initial-element nil))
  (num-cleared 0))

(defstruct (piece (:conc-name nil) (:type vector))
  square loc)

(defstruct (loc (:conc-name nil) (:constructor make-loc (row col)) (:type list))
  row col)

(def shift (l1 l2)
  "Shifts a point by another point."
  (mapcar #'+ l1 l2))

(defparameter height* 22 "The height of a board.")
(defparameter width*  10 "The width  of the board.")
(defparameter population* 50 "The size of a population.")
(defparameter tournament* 5  "The size of a tournament.")
(defparameter trials* 1 "The number of games each vector should be tested on.")
(defparameter num-given* 500 "The number of pieces per test.")
(defparameter replace* 1/3 "The fraction of vectors in the current generator to replace.")
(defparameter prob-mutation* 1/5 "The probability of mutation.")
(defparameter delta* .2 "The maximum amount to shift by when mutating.")

(def create (piece)
  "Creates a piece from a given piece format."
  (let dim (len piece)
    (make-piece
          :loc (make-loc 0 (- (floor width* 2) (ceiling dim 2)))
          :square (make-array (list dim dim)
                              :displaced-to (mapv #'positive (flat piece))))))


(defparameter pieces*
  (mapcar #'create
       '(((0 0 0 0)
          (1 1 1 1)
          (0 0 0 0)
          (0 0 0 0))
    
         ((1 0 0)
          (1 1 1)
          (0 0 0))

         ((0 0 1)
          (1 1 1)
          (0 0 0))

         ((1 1)
          (1 1))

         ((0 1 1)
          (1 1 0)
          (0 0 0))

         ((0 1 0)
          (1 1 1)
          (0 0 0))

         ((1 1 0)
          (0 1 1)
          (0 0 0))))
  "A list of all of the piece formats.")

(def next-in-bag (game)
  "Get the next piece in the bag."
  (withs (bag   (or (bag game) pieces*)
          piece (rand-elt bag))
    (= (bag game) (rem piece bag))
    (copy-seq piece)))

(def copy (game)
  "Makes a deep copy of a game."
  (ret result (copy-seq game)
    (zap #'copy-seq (piece result))
    (= (board result) (make-array (array-dimensions (board result))
                                  :displaced-to (copy-seq (make-array (array-total-size (board result))
                                                                      :displaced-to (board result)))))))

(def scopy (game)
  "Makes a shallow copy of a game."
  (ret result (copy-seq game)
    (zap #'copy-seq (piece result))))

(def locs (piece)
  "Returns all of the locations the piece is current at."
  (with (loc (loc piece) 
         dim (array-dimension (square piece) 0))
    (accum a
      (up i 0 dim
        (up j 0 dim
          (when (aref (square piece) i j)
            (a (shift (make-loc i j) loc))))))))

(def print-game (game)
  "Print the game."
  (repeat width* (pr "-"))
  (prn)
  (let board (board game)
    (up r 2 height*
      (up c 0 width*
        (prf "~:[ ~;~:*~A~]" board.r.c))
         (prn)))
  (repeat width* (pr "-"))
  (prn))

(def rotate-cw (game)
  "Rotates the piece of the game clockwise."
  (let piece (piece game)
    (= (square piece)
       (ret result (make-array (array-dimensions (square piece)) :initial-element nil)
         (let dim (array-dimension (square piece) 0)
              (up i 0 dim
                (up j 0 dim
                  (= (aref result j (- dim i 1))
                     (aref (square piece) i j))))))))
  game)

(def rotate-ccw (game)
  "Rotates the piece of the game counter-clockwise."
  (calln 3 #'rotate-cw game))

(def left (game)
  "Moves the piece of the game to the left."
  (let piece (piece game)
    (= (loc piece) (shift (loc piece) '(0 -1))))
  game)

(def right (game)
  "Moves the piece of the game to the right."
  (let piece (piece game)
    (= (loc piece) (shift (loc piece) '(0 1))))
  game)

(def down (game ? (lock t))
  "Moves the piece of the game down. LOCK determines if this will lock
   the piece when it cannot be moved any further."
  (if (not lock)
      (zap #'shift (loc (piece game)) '(1 0))
      (let copy (scopy game)
        (if (~valid (down copy nil))
            (lock game)
            (down game nil))))
  game)

(def valid (game)
  "Determines if the game is in a valid state."
  (iter (for (r c) in game!piece!locs)
        (always (and (<= 0 r (dec height*))
                     (<= 0 c (dec width*))
                     (not (aref game!board r c))))))

(def valid-move (game move)
  "Determines if the move is valid."
  (let next (call move (scopy game))
    (values (valid next) next)))

(def lock (game ? (clear t))
  "Locks the current piece of the game in place and grabs a new one
   from the bag."
  (iter (for loc in game!piece!locs)
        (= game!board.loc 'o))
  (when clear
    (clear-lines game))
  (= game!piece (next-in-bag game)))

(def clear-lines (game)
  "Clears all of the full lines."
  (clamp:down r height* 0
    (when (iter (for c from 0 below width*)
                (always (aref game!board r c)))
      (clear-line game r)
      (return-from clear-lines (clear-lines game))))
  game)

(def clear-line (game row)
  "Clears one individual line that is full."
  (++ game!num-cleared)
  (downfrom r row 1
    (up c 0 width*
      (= (aref game!board r c) (aref game!board (dec r) c))))
  (up c 0 width*
    (wipe (aref game!board 0 c)))
  game)

(def aggregate-height (game)
  "Returns the aggregate height."
  (iter (for c from 0 below width*)
        (sum (height game c))))

(def complete-lines (game)
  "Returns the number of complete lines in the game."
  (iter (with board = (board game))
        (for r from (dec height*) downto 0)
        (counting (iter (for c from 0 below width*)
                        (always (aref board r c))))))

(def holes (game)
  "Returns the number of 'holes' in the game."
  (iter (with board = (board game))
        (for c from 0 below width*)
        (sum (iter (with seen = nil)
                   (for r from 0 below height*)
                   (or= seen (aref board r c))
                   (counting (and seen (not (aref board r c))))))))

(def bumpiness (game)
  "Returns the bumpiness of the game."
  (iter (for i from 1 below width*)
        (sum (abs (- (height game (dec i))
                     (height game i))))))

(def height (game c)
  "Returns the height of a column."
  (or (iter (for i downfrom height*)
            (for r from 0 below height*)
            (when (aref (board game) r c)
              (leave i)))
      0))

(def score (game vect)
  "Score the game based on the given vector."
  (assert (is (len vect) 4))
  (iter (for a in-vector vect)
        (for f in '(aggregate-height complete-lines holes bumpiness))
        (sum (* a (call f game)))))

(def best-moves (game vect)
  "Find the best series of moves for this game."
  (with (best-score most-negative-fixnum
         best-moves nil)
    (iter (for rotates from 0 to 4)
      (let game (calln rotates #'rotate-cw (scopy game))
        (while (valid-move game 'left)
          (left game))
        (iter (for rights from 0)
              (while (valid game))
              (let game (copy game)
                (while (valid-move game [down _ nil])
                  (down game))
                (lock game nil)
                (let score (score game vect)
                  (when (> score best-score)
                    (= best-score score
                       best-moves (list rotates rights)))))
              (= game (right game)))))
    best-moves))

(def perform-moves (game (rotates rights))
  "Performs all of the moves."
  (calln rotates #'rotate-cw game)
  (while (valid-move game 'left)
    (left game))
  (calln rights #'right game)
  (while (valid-move game [down _ nil])
    (down game))
  (down game))

(def make-game ()
  (ret result (make-game%)
    (= (piece result) (next-in-bag result))))

(def fitness (vect)
  "Returns the fitness of a vector."
  (ret result 0
    (repeat trials*
      (let game (make-game)
           (iter (repeat num-given*)
                 (while (valid game))
                 (perform-moves game (best-moves game vect)))
           (++ result game!num-cleared)))))

(def rand-vector ()
  "Return a random set of four 'genes'."
  (normalize (apply #'vector (n-of 4 (- (rand 1.0) 0.5)))))

(def generation (vectors)
  "Given a single generation, return a new generation by using
   tournament selection."
  (let scores (mapv #'fitness vectors)
    (let new '()
      (repeat (ceiling population* (/ replace*))
        (with (vects  (make-array tournament* :fill-pointer 0)
               scoresv (make-array tournament* :fill-pointer 0))
          (repeat tournament*
            (let num (rand population*)
              (vector-push (elt vectors num) vects)
              (vector-push (elt scores num) scoresv)))
          (push (tournament vects scores) new)))
      (replace-gen vectors scores new))))

(def replace-gen (vs ss new)
  "Replace the lowest scoring vectors in VS with the vectors in
   new. SS should be the scores of VS."
  (ado (bestn (ceiling population* (/ replace*))
              #'< (range 0 (dec population*)) [elt ss _])
       (iter (for pos in it)
             (for v in new)
             (= vs.pos v)
             (finally (return vs)))))

(def tournament (vects scores)
  "Runs a single tournament. Returns the new vector created."
  (let (p1 p2) (bestn 2 #'> (range 0 (dec tournament*)) [elt scores _])
    (combine (elt vects p1) (elt vects p2) (elt scores p1) (elt scores p2))))

(def combine (v1 v2 s1 s2)
  "Breeds two vectors. S1 and S2 are the fitness scores of the two
   vectors."
  (let result (mapv #'+ (mapv [* _ s1] v1) (mapv [* _ s2] v2))
    (when (< (rand 1.0) prob-mutation*)
      (mutate result))
    (normalize result)))

(def mutate (x)
  "Mutates a vector. Shifts one of the genes by at most delta*."
  (++ (elt x (rand 4))
      (- (rand (* 2 delta*))
         delta*)))

(def normalize (x)
  "Normalizes a vector."
  (let total (sqrt (reduce #'+ x :key [* _ _]))
    (if (cl:= total 0.0)
        (rand-vector)
        (mapv [/ _ total] x))))

(def play (vect)
  "Returns the score of a vector."
  (let game (make-game)
    (iter (print-game game)
          (sleep 0.5)
          (while (valid game))
          (perform-moves game (best-moves game vect)))))

;; The best vector I have been able to find after running for a while.
; #(-0.6534881 0.54680866 -0.46280736 -0.24446443)
