;;; Utilities for defining user commands

(in-package :shanai.define-user-command)

;;; Issue in that all packages in a lisp system will use the same "help" or
;;; whatever commands.
(defparameter *user-commands* (make-hash-table :test #'equalp)
  "All commands that users are allowed to call are interned here.")

(defun user-command (name)
  (declare (type string name))
  (gethash name *user-commands*
           (lambda (con msg nick args)
             (values con msg nick args))))

(defun (setf user-command) (value name)
  (declare (type string name)
           (type (or function symbol) value))
  (setf (gethash name *user-commands*) value))

(defun make-user-command-key (key)
  "Convert KEY into a format appropriate for *USER-COMMANDS*."
  (string-upcase (princ-to-string key)))

(defmacro define-user-command (name (con target user args) &body body)
  `(setf (user-command (make-user-command-key ',name))
         (lambda (,con ,target ,user ,args)
           (declare (ignorable ,con ,target ,user ,args))
           ,@body)))

;;; lets try something simpler but in the same line of theory.
#+ () (define-user-command help (connection target user arguments)
  "sorry can't help")


