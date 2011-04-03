(in-package :lisp-user)
(defpackage #:shanai.binary-data
  (:use :cl :com.gigamonkeys.binary-data)
  (:export :unsigned-integer :u1 :u2 :u3 :u4 :s1 :s2 :s3
           :s4 :signed-integer :generic-qt-string
           :utf16-qt-string :define-binary-type
           :define-binary-class :read-value :write-value))

(in-package :shanai.binary-data)

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(defun twos-complement (num size)
  "Change an unsigned NUM to a signed number representation."
  ;; This exists because we read in unsigned octets of information, but we
  ;; wish to treat 255 as -1 and so forth.
  ;; TODO: 128 should be changed to -128... or something! Kinda loopy due to
  ;; backpain, so check this later!
  (if (<= (/ size 2) num)
      (- (mod size num))
      num))

(define-binary-type signed-integer (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return (twos-complement value  (expt 2 (* bits-per-byte bytes))))))
  (:writer (out value)
    (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-type s1 () (signed-integer :bytes 1 :bits-per-byte 8))
(define-binary-type s2 () (signed-integer :bytes 2 :bits-per-byte 8))
(define-binary-type s3 () (signed-integer :bytes 3 :bits-per-byte 8))
(define-binary-type s4 () (signed-integer :bytes 4 :bits-per-byte 8))

(define-binary-type generic-qt-string (external-format)
  (:reader (in)
            (flexi-streams:octets-to-string
             (loop for i from 1 to (read-value 'u4 in)
                collect (read-byte in))
             :external-format external-format))
  (:writer (out string)
           (write-value 'u4 out (flexi-streams:octet-length string :external-format external-format))
           (loop for o across (flexi-streams:string-to-octets string :external-format external-format)
               do (write-byte o out))))

(define-binary-type utf16-qt-string ()
  (generic-qt-string :external-format :utf-16))

