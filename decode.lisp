(in-package #:lzm)

(defconstant +operand-type-word+ 0)
(defconstant +operand-type-byte+ 1)
(defconstant +operand-type-var+  2)
(defconstant +operand-type-none+ 3)

(defun read-operand (operand-type)
  (ecase operand-type
    (#.+operand-type-word+ (fetch-u16))
    (#.+operand-type-byte+ (fetch-u8))
    (#.+operand-type-var+ (read-var (fetch-u8)))))

(defun decode-operand-types (b)
  (loop for pos from 6 downto 0 by 2
        for type = (ldb (byte 2 pos) b)
        until (= type +operand-type-none+)
        collect type))

(defun opcode-type (b)
  (cond
    ((and (= b #xbe) (>= *version* 5)) :opcode-type-ext)
    ((not (logbitp 7 b)) :opcode-type-long)
    ((logbitp 6 b) :opcode-type-var)
    (t :opcode-type-short)))
     
(defun decode-instruction ()
  (let ((b (fetch-u8)))
    (ecase (opcode-type b)
      (:opcode-type-ext nil)
      (:opcode-type-long
       (list :2op
             (logand b #b11111)
             (if (logbitp 6 b) +operand-type-var+ +operand-type-byte+)
             (if (logbitp 5 b) +operand-type-var+ +operand-type-byte+)))
      (:opcode-type-var
       (list* (if (logbitp 5 b) :var :2op)
              (logand b #b11111)
              (decode-operand-types (fetch-u8))))
      (:opcode-type-short
       (let ((opcode (ldb (byte 4 0) b))
             (operand-type (ldb (byte 2 4) b)))
         (if (= operand-type +operand-type-none+)
             (list :0op opcode)
             (list :1op opcode operand-type)))))))

(defun next-instruction ()
  (let ((pc *pc*))
    (destructuring-bind (arity opcode &rest optypes) (decode-instruction)
      (let ((operands (mapcar #'read-operand optypes)))
        (if-let (handler (find-handler arity opcode))
          (progn
        ;;    (format t "PC $~x: ~a ~a ~%" pc handler operands)
            (apply handler operands))
          (progn
            (format t "No handler for ~a $~x at PC $~x" arity opcode pc)
            (setf *break* t)))))))
