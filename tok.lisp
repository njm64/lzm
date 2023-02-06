(in-package #:lzm)

(defvar *dict-separators*)
(defvar *dict-words*)
(defvar *dict-word-length*)

(defun read-separators ()
  (let ((num-separators (fetch-u8)))
    (loop repeat num-separators
          collect (code-char (fetch-u8)))))

(defun read-words ()
  (let ((entry-length (fetch-u8))
        (num-entries (fetch-u16)))
    (loop repeat num-entries
          collect (cons (decode-text *pc*) *pc*)
          do (incf *pc* entry-length))))

(defun init-dictionary ()
  (let ((*pc* *dictionary-offset*))
    (setf *dict-separators* (read-separators))
    (setf *dict-words* (read-words))
    (setf *dict-word-length* 6) ;; This varies by version
    nil))

(defun truncate-seq (s max-length)
  (if (> (length s) max-length)
      (subseq s 0 max-length)
      s))

(defun find-dict-word (word)
  (let ((key (truncate-seq word *dict-word-length*)))
    (if-let (dict-entry (assoc key *dict-words* :test #'equal))
      (cdr dict-entry)
      0)))

(defun is-split-point (a b)
  "Split points are before/after spaces, and before/after separator chars"
  (or (char= a #\Space)
      (char= b #\space)
      (member a *dict-separators*)
      (member b *dict-separators*)))

(defun split-text (text)
  "Split text into a list of words at split points, preserving whitespace"
  (loop for c across text and prev = c
        for i = 0 then (1+ i)
        with prev-split = 0
        with words = nil
        do (when (and prev (is-split-point c prev))
             (push (subseq text prev-split i) words)
             (setf prev-split i))
        finally (return (nreverse (cons (subseq text prev-split) words)))))

(defun tokenise (text)
  "Tokenise text into a list of (dict-address, length, offset) tuples"
  (loop for word in (split-text text)
        and offset = 0 then (+ offset (length word))
        unless (string= word " ")
          collect (list (find-dict-word word) (length word) offset)))
