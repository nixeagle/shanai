(in-package :shanai.po.client)

(deftype color-spec ()
  "4 Valid values that color specifications may be."
  '(member :rgb :hsv :cmyk :invalid))

(defclass color ()
  ((spec :initarg :spec)
   (alpha :initarg :alpha)
   (red :initarg :red)
   (green :initarg :green)
   (blue :initarg :blue)))


(defun read-color (stream)
  ;; Read {s1,u2,u2,u2,u2}
  )