(in-package #:lzm)

(defparameter *A0* "abcdefghijklmnopqrstuvwxyz")
(defparameter *A1* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defparameter *A2*
  #(#\Space #\Newline #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\. #\, #\! #\? #\_ #\# #\' #\" #\/ #\\ #\- #\: #\( #\) ))

(defun abbrev-addr (table-index i)
  (read-word-addr (+ *abbrev-table-offset* (* table-index 64) (* i 2))))

(defun collect-chars (addr)
  (loop for p = addr then (+ 2 p)
        for w = (read-u16 p)
        collect (ldb (byte 5 10) w) into cs 
        collect (ldb (byte 5 5) w) into cs
        collect (ldb (byte 5 0) w) into cs
        until (logbitp 15 w)
        finally (return (values cs (+ 2 (- p addr))))))

(defun decode-chars (chars)
  (with-output-to-string (s)
    (loop with charset = *A0*
          with abbrev = nil
          with esc = nil
          with esc-code = 0
          for c in chars do
            (cond
              ((eql esc 0)
               (setf esc-code (ash c 5))
               (incf esc))
              ((eql esc 1)
               (setf esc-code (logior esc-code c))
               (princ (code-char esc-code) s)
               (setf esc nil))
              (abbrev
               (princ (decode-text (abbrev-addr abbrev c)) s)
               (setf abbrev nil))
              ((= c 0) (princ " " s))
              ((= c 1) (setf abbrev 0))
              ((= c 2) (setf abbrev 1))
              ((= c 3) (setf abbrev 2))
              ((= c 4) (setf charset *A1*))
              ((= c 5) (setf charset *A2*))
              ((and (= c 6) (eql charset *A2*))
               (setf esc 0 charset *A0*))
              (t
               (princ (elt charset (- c 6)) s)
               (setf charset *A0*))))))

(defun decode-text (addr)
  (multiple-value-bind (chars byte-len) (collect-chars addr)
    (let ((s (decode-chars chars)))
      (values s byte-len))))
