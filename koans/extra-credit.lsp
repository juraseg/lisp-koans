;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.

(defclass dice-set ()
  ((values :reader get-values :initform '())))

(defmethod get-values ((object dice-set))
  (slot-value object 'values))

(defmethod roll (how-many (object dice-set))
  (setf (slot-value object 'values) '())
  (dotimes (i how-many)
    (push (+ (random 6) 1) (slot-value object 'values)))
  (get-values object))

(defun score (dice-set)
  "Counts score in 'greed' dice game"
  
  (defun get-dice-score (count score-for-3 score-for-1)
    "Counts score for given count of dice with define score for 3 and 1.
     Always prefers 3 dice over 1, so not universal"
    (let ((res 0))
      (loop while (>= count 3)
            do (incf res score-for-3)
               (decf count 3))
      (incf res (* count score-for-1))
      res))

  (defun score-helper (dice-number dice-count)
    "Counts how many score for given count of concrete dice number"
    (cond ((= dice-number 1) (get-dice-score dice-count 1000 100))
          ((= dice-number 5) (get-dice-score dice-count (* dice-number 100) 50))
          (t (get-dice-score dice-count (* dice-number 100) 0))))

  (let ((count-hash (make-hash-table))
        (dice-values (get-values dice-set))
        (result 0))
    (loop for i from 1 to 6
          do (incf result (score-helper i (count i dice-values))))
    result))

(defun get-non-scoring (dice-set)
  "Extracts non-scoring dice from dice set"
  ;; TODO
  )
