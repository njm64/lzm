(in-package #:lzm)

(defvar *rand-seed* nil)
(defvar *rand-next* 0)

(defun rand-seed (n)
  (if (zerop n)
      (setf *rand-seed* nil *rand-next* 0)
      (setf *rand-seed* n *rand-next* 0)))

(defun rand-next-predictable ()
  (assert *rand-seed*)
  (prog1 (mod *rand-next* *rand-seed*)
    (incf *rand-next*)))

(defun rand-next-random ()
  (get-internal-real-time))

(defun rand-next (n)
  (let ((raw (if *rand-seed*
                 (rand-next-predictable)
                 (rand-next-random))))
    (1+ (mod raw n))))
