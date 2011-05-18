(in-package :cl-user)
(defpackage #:shanai.binary-data
  (:use :cl)
  (:export :unsigned-integer :u1 :u2 :u3 :u4 :s1 :s2 :s3
           :s4 :signed-integer :generic-qt-string
           #:write-qtstring
           #:read-qtstring
           #:qtstring-length
           #:write-u1
           #:write-u2
           #:write-u4
           #:write-s1
           #:read-u1
           #:read-u2
           #:read-u4
           #:read-s1
           #:write-s1))

(in-package :shanai.binary-data)

(deftype u1 ()
  "Shorthand notation for saying `unsigned-byte'."
  '(unsigned-byte 8))

(deftype u2 ()
  "Shorthand notation for 2 `unsigned-byte'."
  '(unsigned-byte 16))

(deftype u4 ()
  "Shorthand notation for 4 `unsigned-byte'."
  '(unsigned-byte 32))

(defun twos-complement (num size)
  "Change an unsigned NUM to a signed number representation."
  ;; This exists because we read in unsigned octets of information, but we
  ;; wish to treat 255 as -1 and so forth.
  ;; TODO: 128 should be changed to -128... or something! Kinda loopy due to
  ;; backpain, so check this later!
  (if (<= (/ size 2) num)
      (- (mod size num))
      num))

(defun read-u1 (in)
  "Read a single byte from IN stream."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           )
  (the u1 (read-byte in)))

(defun write-u1 (value out)
  (declare 
           (type u1 value))
  (write-byte value out))

(defun read-u2 (in)
  "Read a 2 byte unsigned integer."

  (let ((result 0))
    (declare (type u2 result))
    (setf (ldb (byte 8 8) result) (read-u1 in)
          (ldb (byte 8 0) result) (read-u1 in))
    result))

(defun write-u2 (value out)
  (declare 
           (type u2 value))
  (write-u1 (ldb (byte 8 8) value) out)
  (write-u1 (ldb (byte 8 0) value) out))

(defun read-u4 (in)
 
  (let ((result 0))
    (declare (type u4 result))
    (setf (ldb (byte 16 16) result) (read-u2 in)
          (ldb (byte 16 0)  result) (read-u2 in))
    result))

(defun write-u4 (value  out)
  (declare 
           (type u4 value))
  (write-u1 (ldb (byte 8 24) value) out)
  (write-u1 (ldb (byte 8 16) value) out)
  (write-u1 (ldb (byte 8  8) value) out)
  (write-u1 (ldb (byte 8  0) value) out))

(defun read-s1 (in)
  "Read a signed one bit integer from IN stream."
  (twos-complement (read-u1 in) 256))

(defun read-qtstring (in)
  (let ((len (read-u4 in)))
    (when (not (= #xffffffff len))
      (let ((s (make-string (/ len 2))))
        (setf (stream-external-format in)
              (ccl:make-external-format :character-encoding :utf-16be))
        (read-sequence s in)
        (values s (+ len 4))))))

(defun write-qtstring (value out)
  (let ((size (and value (ccl:string-size-in-octets value :external-format :utf-16be))))
    (write-u4 (or size 0)  out))
  (setf (stream-external-format out)
        (ccl:make-external-format :character-encoding :utf-16be))
  (write-string value out))

(defun qtstring-length (string)
  (+ 4 (* 2 (length string))))

#+ ()
(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

#+ ()
(define-binary-type signed-integer (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return (twos-complement value  (expt 2 (* bits-per-byte bytes))))))
  (:writer (out value)
    (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))
#+ ()
(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
#+ ()
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
#+ ()(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
#+ ()(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

#+ ()(define-binary-type s1 () (signed-integer :bytes 1 :bits-per-byte 8))
#+ ()(define-binary-type s2 () (signed-integer :bytes 2 :bits-per-byte 8))
#+ ()(define-binary-type s3 () (signed-integer :bytes 3 :bits-per-byte 8))
#+ ()(define-binary-type s4 () (signed-integer :bytes 4 :bits-per-byte 8))

#+ ()(define-binary-type generic-qt-string (external-format)
  (:reader (in)
            (flexi-streams:octets-to-string
             (loop for i from 1 to (read-value 'u4 in)
                collect (read-byte in))
             :external-format external-format))
  (:writer (out string)
           (write-value 'u4 out (flexi-streams:octet-length string :external-format external-format))
           (loop for o across (flexi-streams:string-to-octets string :external-format external-format)
               do (write-byte o out))))

#+ ()(define-binary-type utf16-qt-string ()
  (generic-qt-string :external-format :utf-16))

#+ ()(define-binary-type qtstring ()
  (utf16-qt-string))

#+ () (defun read-u1 (in)
  (declare (type stream in)
           (optimize (speed 3)))
  (read-byte in))