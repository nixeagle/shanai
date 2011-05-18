(in-package :shanai.util)

(defun esc (string)
  (declare (type string string))
  (cl-who:escape-string string))