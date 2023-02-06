(in-package #:lzm)

(defun local (i)
  (aref *stack* (+ *frame* i)))

(defun (setf local) (val i)
  (setf (aref *stack* (+ *frame* i)) val))

(defun global (i)
  (read-u16 (+ *global-table-offset* (* i 2))))

(defun (setf global) (val i)
  (write-u16 (+ *global-table-offset* (* i 2)) val))

(defun read-var (n)
  (cond
    ((zerop n) (stack-pop))
    ((< n #x10) (local (1- n))) 
    ((< n #x100) (global (- n #x10)))))

(defun read-signed-var (n)
  (u16->s16 (read-var n)))

(defun write-var (n val)
  (let ((uval (logand #xffff val)))
    (cond
      ((zerop n) (stack-push uval))
      ((< n #x10) (setf (local (1- n)) uval))
      ((< n #x100) (setf (global (- n #x10)) uval)))))
