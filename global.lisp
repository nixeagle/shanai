;;;; Global state variables that we use for in memory lookup and other
;;;; related functions.

(in-package :shanai.global)

(defparameter *po-output-stream* nil
  "Output stream used by each `connection' object.

Anything that needs to output a packet involving the PO client or server
should output to this stream.")

(defvar *active-po-bots* nil
  "Contains a list of all active bots.")

(defun active-po-bots ()
  "List of all active bots."
  *active-po-bots*)


(defparameter *current-connection* nil
  "Dynamic variable containing the current connection.")

(defun current-connection ()
  *current-connection*)