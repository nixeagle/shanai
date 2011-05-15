(in-package :shanai.util)

(defun make-keyword (name)
  "Make a keyword given NAME as a string."
  (declare (type string name))
  (alexandria:make-keyword (string-upcase name)))