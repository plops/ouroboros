;; This file is responsible for generating the core image of the Ouroboros system.
;; It loads the necessary systems using ASDF and defines an alien callable named posix-argv.
;; The posix-argv callable is used as a callable export for the save-lisp-and-die function.
;; The load-ouroboros function is added as an initialization hook to load the Ouroboros system.
;; Finally, the core image is saved using the save-lisp-and-die function.

(in-package #:cl-user)

(asdf:load-systems
 :alexandria
 :bordeaux-threads
 :cffi
 :closer-mop
 :named-readtables
 :trivial-garbage
 )


;; This is a hack - we define an alien callable named posix-argv only so that
;; we can supply it as callable export to save-lisp-and-die.  We need to supply
;; at least one callable export to prevent SBCL from starting a REPL.
(sb-alien:define-alien-callable ("posix_argv" %posix-argv) sb-alien:void ())


(defun load-ouroboros ()
  (handler-case (asdf:load-system :ouroboros :verbose t :force t)
    (error (e)
      (format t "Caught error ~A." e)
      (finish-output))))

(pushnew 'load-ouroboros sb-ext:*init-hooks*)

(sb-ext:save-lisp-and-die *ouroboros-core* :callable-exports '(("posix_argv" %posix-argv)))
