(in-package :pokemon)


(defclass ability ()
  ((name :name :name :initarg :name :type 'string)
   (number :name :number :initarg :number :type 'fixnum :readers (number))
   (description :name :description :initarg :description :type 'string)
   (generation :name :generation :initarg :generation :type 'fixnum)
   (single :name :single :initarg :single :type 'fixnum)
   (dual :name :dual :initarg :dual :type 'fixnum)
   (hidden :name :hidden :initarg :hidden :type 'fixnum)))