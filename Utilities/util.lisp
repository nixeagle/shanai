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