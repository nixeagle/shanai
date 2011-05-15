;;; temporary holding place for the connection class used in the po client
;;; and possibly used for a server later...

(in-package :pokemon.po.client)

(defvar *po-socket* nil
  "Global dynamic variable for holding pokemon online sockets.

Local to each thread that is created.")

(defvar *po-socket-recv-log* nil
  "Continuing log of all recieved messages from all PO servers.")
(defvar @po-socket@ nil
  "Contains a pointer to the last created pokemon online socket.")

(defvar *pingcount* 0)
(defvar *temp* nil)
(defparameter +PO-shanai-id+ 6)
(defparameter +PO-shanaindigo-id+ 7)


(defclass connection (usocket:stream-usocket)
  ((nickname :initarg :nickname :reader nickname)
   (channels :initarg :channels :accessor channels
             :initform (make-hash-table :test #'equalp))
   (battles :initarg :battles :accessor battles)
   (trainers :initarg :trainers :accessor trainers
             :initform (make-hash-table :test #'equalp))))


(defmethod generic:name ((con connection))
  (slot-value con 'nickname))
;;; should be defined elsehwere but works fine here
(defun get-stream (thing)
  (typecase thing
    (stream thing)
    (usocket:stream-usocket (usocket:socket-stream thing))))

(defmacro with-input-from-octet-vector ((var list) &rest body)
  (alexandria:once-only ((l list))
    `(with-input-from-vector (,var (make-array (length ,l)
                                               :element-type '(unsigned-byte 8)
                                               :initial-contents
                                               (typecase ,l
                                                 (array (loop for i across ,l
                                                             collect i))
                                                 (otherwise ,l))))
       ,@body)))