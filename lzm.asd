(defsystem :lzm
  :author "Nick Maher"
  :description "Lisp Z-Machine"
  :depends-on (:alexandria)
  :components ((:file "packages")
               (:file "mem")
               (:file "header")
               (:file "text")
               (:file "var")
               (:file "obj")
               (:file "cpu")
               (:file "rand")
               (:file "tok")
               (:file "op")
               (:file "decode")
               (:file "main")))
