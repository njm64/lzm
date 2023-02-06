(in-package #:lzm)

(defvar *stack*)
(defvar *frame*)
(defvar *pc*)
(defvar *break*)

(defun fetch-u8 ()  
  (prog1 (read-u8 *pc*)
    (incf *pc*)))

(defun fetch-u16 ()
  (prog1 (read-u16 *pc*)
    (incf *pc* 2)))

(defun stack-push (value)
  (vector-push-extend value *stack*))

(defun stack-pop ()
  (vector-pop *stack*))

(defun stack-top ()
  (aref *stack* (1- (length *stack*))))

(defun ret (val)
  (setf (fill-pointer *stack*) *frame*
        *frame* (stack-pop)
        *pc* (stack-pop))
  (write-var (stack-pop) val))

(defun branch (condition)
  (setf condition (not (not condition)))
  (let* ((b (fetch-u8))
         (offset (ldb (byte 6 0) b)))
    (unless (logbitp 6 b)
      (let ((b2 (fetch-u8)))
        (setf offset (logior (ash offset 8) b2))))
    (when (logbitp 13 offset)
      ;; Handle negative branch
      (decf offset #x4000))
    (when (eql condition (logbitp 7 b))
      (case offset
        (0 (ret 0))
        (1 (ret 1))
        (otherwise (incf *pc* (-  offset 2)))))))
