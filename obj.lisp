(in-package #:lzm)

(defun obj-property-default (property)
  (read-u16 (+ *object-table-offset* (* (1- property) 2))))

(defun obj-address (obj)
  (assert (plusp obj))
  (+ *object-table-offset* 62 (* (1- obj) 9)))

(defun obj-attr-address (obj attr)
  (multiple-value-bind (byte bit) (floor attr 8)
    (values (+ (obj-address obj) byte)
            (- 7 bit))))

(defun obj-attr (obj attr)
  (multiple-value-bind (addr bit) (obj-attr-address obj attr)
    (logbitp bit (read-u8 addr))))

(defun (setf obj-attr) (val obj attr)
  (multiple-value-bind (addr bit) (obj-attr-address obj attr)
    (write-u8 addr (dpb (if val 1 0)
                        (byte 1 bit)
                        (read-u8 addr)))))

(defun obj-parent (obj)
  (read-u8 (+ (obj-address obj) 4)))

(defun (setf obj-parent) (val obj)
  (write-u8 (+ (obj-address obj) 4) val))

(defun obj-sibling (obj)
  (read-u8 (+ (obj-address obj) 5)))

(defun (setf obj-sibling) (val obj)
  (write-u8 (+ (obj-address obj) 5) val))

(defun obj-child (obj)
  (read-u8 (+ (obj-address obj) 6)))

(defun (setf obj-child) (val obj)
  (write-u8 (+ (obj-address obj) 6) val))

(defun obj-prev-sibling (obj)
  (let ((parent (obj-parent obj)))
    (if (zerop parent)
        0
        (loop for s = (obj-child parent) then (obj-sibling s)
              and prev = 0 then s
              finally (return 0)
              when (= s obj) do (return prev)))))

(defun obj-name-addr (obj)
  (read-u16 (+ (obj-address obj) 7)))

(defun obj-name-byte-len (obj)
  (* 2 (read-u8 (obj-name-addr obj))))

(defun obj-name (obj)
  (decode-text (+ 1 (obj-name-addr obj))))

(defun obj-properties-addr (obj)
  (+ (obj-name-addr obj) (obj-name-byte-len obj) 1))

;; Return a list of (prop, addr, size) tuples
(defun obj-property-list (obj)
  (loop with addr = (obj-properties-addr obj)
        for size-byte = (read-u8 addr)
        for prop = (logand size-byte #b11111)
        for size = (1+ (ash size-byte -5))
        until (zerop prop)
        collect (list prop (1+ addr) size)
        do (incf addr (1+ size))))

(defun obj-first-prop (obj)
  (caar (obj-property-list obj)))

(defun obj-next-prop (obj prop)
  (let ((plist (obj-property-list obj)))
    (when-let (i (position prop plist :key #'first))
      (first (nth (1+ i) plist)))))

(defun obj-prop-addr (obj prop)
  (when-let (p (find prop (obj-property-list obj) :key #'first))
    (second p)))

(defun obj-prop-len (prop-addr)
  (if (zerop prop-addr)
      0
      (let ((b (read-u8 (1- prop-addr))))
        (1+ (ash b -5)))))

(defun obj-prop (obj prop)
  (if-let (p (find prop (obj-property-list obj) :key #'first))
    (destructuring-bind (prop addr size) p
      (declare (ignore prop))
      (ecase size
        (1 (read-u8 addr))
        (2 (read-u16 addr))
        (t (error "invalid property size"))))
    (obj-property-default prop)))

(defun (setf obj-prop) (val obj prop)
  (let ((p (find prop (obj-property-list obj) :key #'first)))
    (unless p (error "put to invalid property"))
    (destructuring-bind (prop addr size) p
      (declare (ignore prop))
      (ecase size
        (1 (write-u8 addr val))
        (2 (write-u16 addr val))
        (t (error "invalid property size"))))))

(defun obj-remove (obj)
  (let ((parent (obj-parent obj))
        (sibling (obj-sibling obj))
        (prev-sibling (obj-prev-sibling obj)))
    (when (plusp parent)
      (if (zerop prev-sibling)
          (setf (obj-child parent) sibling)
          (setf (obj-sibling prev-sibling) sibling))
      (setf (obj-sibling obj) 0)
      (setf (obj-parent obj) 0))))

(defun obj-insert (obj dst)
  (obj-remove obj)
 ;; (format t "Moving ~a to ~a~%" (obj-name obj) (obj-name dst))
  (setf (obj-sibling obj) (obj-child dst))
  (setf (obj-child dst) obj)
  (setf (obj-parent obj) dst))
