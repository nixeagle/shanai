(in-package :shanai.team)

(defclass basic-team ()
  ((pokemon :initarg :pokemon :initform (make-array 6 :initial-element nil
                                                    :adjustable nil)
            :accessor team-pokemon)))