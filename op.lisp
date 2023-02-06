(in-package #:lzm)

(defparameter *op-table* (make-array 128 :initial-element nil))

(eval-when (:compile-toplevel :load-toplevel)
  (defun op-table-index (arity opcode)
    (+ opcode (ecase arity (:0op 0) (:1op 32) (:2op 64) (:var 96)))))

(defun find-handler (arity opcode)
  (aref *op-table* (op-table-index arity opcode)))

(defmacro def-op ((arity opcode name &key store) args &body body)
  (let ((i (op-table-index arity opcode))
        (sym (intern (concatenate 'string "OP-" (string-upcase name)))))
    (when store
     (setf body `((write-var (fetch-u8) (progn ,@body)))))
    `(progn
       (defun ,sym ,args ,@body)
       (setf (aref *op-table* ,i) (function ,sym)))))

;;;;------------------------------------------------------------------------
;;;; 2OP opcodes
;;;;------------------------------------------------------------------------

(def-op (:2op #x01 "je") (a &rest args)
  (branch (member a args)))

(def-op (:2op #x02 "jl") (a b)
  (branch (< (u16->s16 a) (u16->s16 b))))

(def-op (:2op #x03 "jg") (a b)
  (branch (> (u16->s16 a) (u16->s16 b))))

(def-op (:2op #x04 "dec-chk") (var limit)
  (let ((val (1- (read-signed-var var))))
    (write-var var val)
    (branch (< val (u16->s16 limit)))))

(def-op (:2op #x05 "inc-chk") (var limit)
  (let ((val (1+ (read-signed-var var))))
    (write-var var val)
    (branch (> val (u16->s16 limit)))))

(def-op (:2op #x06 "jin") (a b)
  (branch (= (obj-parent a) b)))

(def-op (:2op #x07 "test") (bitmap flags)
  (branch (= (logand bitmap flags) flags)))

(def-op (:2op #x08 "or" :store t) (a b)
  (logior a b))

(def-op (:2op #x09 "and" :store t) (a b)
  (logand a b))

(def-op (:2op #x0a "test-attr") (obj attr)
  (branch (obj-attr obj attr)))

(def-op (:2op #x0b "set-attr") (obj attr)
  (setf (obj-attr obj attr) t))

(def-op (:2op #x0c "clear-attr") (obj attr)
  (setf (obj-attr obj attr) nil))

(def-op (:2op #x0d "store") (variable value)
  (when (zerop variable)
    (stack-pop)) ; 6.3.4
  (write-var variable value))

(def-op (:2op #x0e "insert-obj") (obj dst)
  (obj-insert obj dst))

(def-op (:2op #x0f "loadw" :store t) (array word-index)
  (read-u16 (+ array (* 2 word-index))))

(def-op (:2op #x10 "loadb" :store t) (array byte-index)
  (read-u8 (+ array byte-index)))

(def-op (:2op #x11 "get-prop" :store t) (obj prop)
  (obj-prop obj prop))

(def-op (:2op #x12 "get-prop-addr" :store t) (obj prop)
  (or (obj-prop-addr obj prop) 0))

(def-op (:2op #x13 "get-next-prop" :store t) (obj prop)
  (or (if (zerop prop)
          (obj-first-prop obj)
          (obj-next-prop obj prop))
      0))

(def-op (:2op #x14 "add" :store t) (a b)
  (+ a b))

(def-op (:2op #x15 "sub" :store t) (a b)
  (- a b))

(def-op (:2op #x16 "mul" :store t) (a b)
  (* a b))

(def-op (:2op #x17 "div" :store t) (a b)
  (truncate (u16->s16 a) (u16->s16 b)))

(def-op (:2op #x18 "mod" :store t) (a b)
  (rem (u16->s16 a) (u16->s16 b)))

;;;;------------------------------------------------------------------------
;;;; 1OP opcodes
;;;;------------------------------------------------------------------------

(def-op (:1op #x00 "jz") (a)
  (branch (zerop a)))

(def-op (:1op #x01 "get-sibling" :store t) (obj)
  (let ((sibling (obj-sibling obj)))
    (branch (plusp sibling))
    sibling))

(def-op (:1op #x02 "get-child" :store t) (obj)
  (let ((child (obj-child obj)))
    (branch (plusp child))
    child))

(def-op (:1op #x03 "get-parent" :store t) (obj)
  (obj-parent obj))

(def-op (:1op #x04 "get-prop-len" :store t) (prop-addr)
  (obj-prop-len prop-addr))

(def-op (:1op #x05 "inc") (var)
  (write-var var (1+ (read-signed-var var))))

(def-op (:1op #x06 "dec") (var)
  (write-var var (1- (read-signed-var var))))

(def-op (:1op #x07 "print-addr") (addr)
  (princ (decode-text addr)))

(def-op (:1op #x09 "remove-obj") (obj)
  (obj-remove obj))

(def-op (:1op #x0a "print-obj") (obj)
  (princ (obj-name obj)))

(def-op (:1op #x0b "ret") (val)
  (ret val))

(def-op (:1op #x0c "jump") (offset)
  (incf *pc* (- (u16->s16 offset) 2)))

(def-op (:1op #x0d "print-paddr") (addr)
  (princ (decode-text (* addr 2))))

(def-op (:1op #x0e "load" :store t) (var)
  (when (zerop var)
    (stack-push (stack-top))) ; 6.3.4
  (read-var var))

(def-op (:1op #x0f "not" :store t) (val)
  (logxor val #xffff))

;;;;------------------------------------------------------------------------
;;;; 0OP opcodes
;;;;------------------------------------------------------------------------

(def-op (:0op #x00 "true") ()
  (ret 1))

(def-op (:0op #x01 "false") ()
  (ret 0))

(def-op (:0op #x02 "print") ()
  (multiple-value-bind (s byte-len) (decode-text *pc*)
    (princ s)
    (incf *pc* byte-len)))

(def-op (:0op #x03 "print-ret") ()
  (op-print)
  (terpri)
  (ret 1))

(def-op (:0op #x08 "ret-popped") ()
  (ret (stack-pop)))

(def-op (:0op #x09 "pop") ()
  (stack-pop))

(def-op (:0op #x0b "new-line") ()
  (terpri))

(def-op (:0op #x0a "quit") ()
  (setf *break* t))

(def-op (:0op #x0d "verify") ()
  ;; Is there any point implementing this?
  (branch t))

;;;;------------------------------------------------------------------------
;;;; Variable opcodes
;;;;------------------------------------------------------------------------

(def-op (:var #x00 "call") (routine &rest args)
  (let ((ret-var (fetch-u8)))
    (if (zerop routine)
        (write-var ret-var 0)
        (progn
          (stack-push ret-var) ;; Return variable
          (stack-push *pc*) ;; Return address
          (stack-push *frame*) ;; Frame pointer
          (setf *frame* (fill-pointer *stack*)
                *pc* (* routine 2))
          (let ((num-locals (fetch-u8)))
            (dotimes (i num-locals)
              (let* ((default (fetch-u16))
                     (local (or (nth i args) default)))
                (stack-push local))))))))

(def-op (:var #x01 "storew") (array word-index value)
  (write-u16 (+ array (* 2 word-index)) value))

(def-op (:var #x02 "storeb") (array byte-index value)
  (write-u8 (+ array byte-index) value))

(def-op (:var #x03 "put-prop") (obj prop val)
  (setf (obj-prop obj prop) val))

(def-op (:var #x04 "read") (text-buf parse-buf)
  (let* ((max-chars (1- (read-u8 text-buf)))
         (max-tokens (read-u8 parse-buf))
         (text (truncate-seq (string-downcase (read-line)) max-chars)))
    (loop for c across text
          for dst = (1+ text-buf) then (1+ dst)
          do (write-u8 dst (char-code c)))
    (write-u8 (+ text-buf (length text) 1) 0)
    (let ((tokens (truncate-seq (tokenise text) max-tokens)))
      (write-u8 (1+ parse-buf) (length tokens))
      (loop for (addr len offset) in tokens
            for dst = (+ parse-buf 2) then (+ dst 4) do
              (write-u16 dst addr)
              (write-u8 (+ dst 2) len)
              (write-u8 (+ dst 3) (1+ offset))))))

(def-op (:var #x05 "print-char") (n)
  (princ (code-char n)))

(def-op (:var #x06 "print-num") (n)
  (princ (u16->s16 n)))

(def-op (:var #x07 "random" :store t) (n)
  (setf n (u16->s16 n))
  (if (plusp n)
      (rand-next n)
      (rand-seed (- n))))

(def-op (:var #x08 "push") (n)
  (stack-push n))

(def-op (:var #x09 "pull") (dst)
  (let ((val (stack-pop)))
    (when (zerop dst)
      (stack-pop)) ; 6.3.4
    (write-var dst val)))

