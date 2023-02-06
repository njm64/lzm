(in-package #:lzm)

(defun read-data (filename)
  (with-open-file (f filename :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length f)
                            :element-type '(unsigned-byte 8))))
      (read-sequence data f)
      data)))

(defun init (filename)
  (setf *memory* (read-data filename))
  (read-header)
  ;; TODO: Validate header
  (init-dictionary)
  (setf *pc* *initial-pc*)
  (setf *frame* 0)
  (setf *break* nil)
  (setf *rand-seed* nil)
  (setf *stack* (make-array 0 :fill-pointer 0
                              :adjustable t
                              :element-type '(unsigned-byte 32))))

(defun run (filename)
  (init filename)
  (loop until *break* do
    ;;    (format t "PC: $~x~%" *pc*)
    (next-instruction)))

(defun run-hitch () (run "/Users/nickm/infocom/hitchhiker-r60-s861002.z3"))
(defun run-zork () (run "/Users/nickm/infocom/zork1-r88-s840726.z3"))
(defun run-czech () (run "/Users/nickm/infocom/czech_0_8/czech.z3"))

(defun log-czech ()
  (with-open-file (*standard-output* "czech.log"
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :overwrite)
    (run-czech)))

