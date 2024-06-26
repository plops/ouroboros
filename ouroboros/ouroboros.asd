(defsystem :ouroboros
  :description "Bringing SBCL to Python and vice versa."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "cffi"
   "closer-mop"
   "named-readtables"
   "trivial-garbage")

  :components
  ((:static-file "gencore")
   (:file "packages")
   (:file "readtables")
   (:file "foreign")
   (:file "low-level")
   (:file "mirror-into-lisp")
   (:file "mirror-into-python")
   (:file "python")
   (:file "lispify")
   (:file "pythonize")))
