(in-package :shanai.util)

(defun esc (string)
  (declare (type string string))
  (cl-who:escape-string string))


(defgeneric ensure-stream (thing)
  (:documentation "Returns stream representation of THING"))

(defmethod ensure-stream ((stream stream))
  "Given a stream, return it."
  stream)

(defmethod ensure-stream ((sock usocket:stream-usocket))
  "Grab output stream for SOCK."
  (usocket:socket-stream sock))


;;; conditions

(defparameter *restart-recursive-p* nil)
(defun yield-to-parent-restart (name)
  `(lambda (c)
    (declare (ignore c))
    (if *restart-recursive-p*
        nil
      (let ((*restart-recursive-p* t))
        (print (find-restart ,name))
        (not (find-restart ,name))))))
#+ () (defmacro yield-to-parent-restart (name)
  (yield-to-parent-restart-function name))

(defmacro with-yielding-restart-case ((name args (&key test report interactive)
                                            &body forms) &body body)
  `(restart-case (progn ,@body)
     (,name ,args :test (lambda (c)
                          (print c)
                          (and (funcall #',(yield-to-parent-restart `',name) c)
                               ,(if test
                                   `(funcall ,test c)
                                   t)))
            :report ,report :interactive ,interactive
             ,@forms)))

