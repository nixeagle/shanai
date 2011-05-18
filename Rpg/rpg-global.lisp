(in-package :shanai.rpg.global)

(defvar *trainers*
  (list)
  "All available trainers in the rpg.")

(defclass rpg-player ()
  ((name :initarg :name :type 'string
         :reader name)))

(defun add-rpg-player (name)
  (declare (type string name))
  (let ((player (make-instance 'rpg-player :name name)))
    (pushnew player *trainers*
             :key (lambda (p)
                    (when p (name p))) :test #'string-equal)
    player))

