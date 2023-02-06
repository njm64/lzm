(in-package #:lzm)

(defvar *memory*)

(defun read-u8 (addr)
  (aref *memory* addr))

(defun read-u16 (addr)
  (logior (ash (read-u8 addr) 8) (read-u8 (1+ addr))))

(defun read-word-addr (addr)
  (* 2 (read-u16 addr)))

(defun write-u8 (addr value)
  (assert (< addr *static-memory-offset*))
  (setf (aref *memory* addr) value))

(defun write-u16 (addr value)
  (assert (< (1+ addr) *static-memory-offset*))
  (setf (aref *memory* addr) (ash value -8)
        (aref *memory* (1+ addr)) (logand value #xff)))

(defun u16->s16 (n)
  (if (logbitp 15 n)
      (- n #x10000)
      n))

(defun s16->u16 (n)
  (logand #xffff n))
