;;; Objects for manipulating numeric intervals.
(defpackage #:shanai.util.interval
  (:use :cl)
  (:export :interval #:left-bound #:right-bound
           :closed-interval))

(in-package :shanai.util.interval)


(defclass interval ()
  ((left-bound :initarg :left-bound :accessor left-bound)
   (right-bound :initarg :right-bound :accessor right-bound))
  (:documentation "Mathematical interval.

This is the base class of intervals, no information is known as to if this
interval is open, closed or any other property."))

(defclass closed-interval (interval)
  ()
  (:documentation "Represents a closed interval [a, b]."))

(defun closed-interval (left right)
  "Create a closed interval [LEFT, RIGHT]."
  (make-instance 'closed-interval :left-bound left :right-bound right))

(defmethod print-object ((obj closed-interval) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "[~A, ~A]" (left-bound obj) (right-bound obj))))


(defun intersect (a b)
  (declare (type interval a b))
  (let ((la (left-bound a))
        (ra (right-bound a))
        (lb (left-bound b))
        (rb (right-bound b)))
    (if (< la lb ra)
        (closed-interval lb (cons ra rb))
        (closed-interval la (cons ra rb)))))