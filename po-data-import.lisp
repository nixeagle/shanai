(in-package :pokemon)



(defun make-po-data-filepath (file)
  (declare (type string file))
  "Append the relative path to FILE to PO installation database path."
  (concatenate 'string *po-directory* "bin/db/" file))

(defparameter *pokedex* (make-hash-table :test #'eq))
(defun import-poke-stats (filename)
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil)
         while line do
         (let ((pline (parse-po-db-line line)))
           (setf (gethash (+ (car pline) (* 2048 (second pline)))
                          *pokedex*)
                 (make-instance 'pokemon :number (car pline)
                                :base-stats (apply #'battle-stats (nthcdr 2 pline))))))))

(defun with-po-data-file-function (filename hash thunk)
  (with-open-file (s (make-po-data-filepath filename) :direction :input)
    (loop for line = (read-line s nil)
         while line do
         (funcall thunk hash (parse-po-db-line line)))))

(defmacro with-po-data-file ((filename hash line hashname) &body body)
  `(with-po-data-file-function ,filename ,hash
                              (lambda (,hashname ,line)
                                ,@body)))

(defun parse-integer-or-string (input)
  "Try to parse an integer from input, on failure, just return a string."

  (when input (or (parse-integer (remove #\. input) :junk-allowed t) input)))

(defun parse-po-db-line (line)
  (declare (type string line))
  (let ((colonpos (position #\: line)))
    (cons (parse-integer (subseq line 0 colonpos))
          (mapcar #'parse-integer-or-string (split-sequence #\ (subseq line (1+ colonpos)))))))


(defun compute-pokemon-id (number forme)
  "Generate a unique id among Generation V pokemon given DexID and forme."
  (declare (type non-negative-fixnum number forme))
  (+ number (* 2048 forme)))

(defun pokemon-id-from-parsed-line (pline)
  "Generate a unique id given a parsed PO database line."
  (declare (type (cons non-negative-fixnum (cons non-negative-fixnum)) pline))
  (compute-pokemon-id (car pline) (second pline)))


(defparameter *typedex*
  (with-open-file (s (make-po-data-filepath "types/types.txt") :direction :input)
    (loop for line = (read-line s nil)
       while line collect (intern (string-upcase line) :pokemon)))
  "List of the symbols standing for the 17 types in pokemon. Order is important!")

(defparameter *abilitydex*
  (with-open-file (s (make-po-data-filepath "types/types.txt") :direction :input)
    (loop for line = (read-line s nil)
         for i from 0
       while line collect (make-instance 'ability :name line :number i)))
  "List of the symbols standing for the 17 types in pokemon. Order is important!")

(defparameter *naturedex*
  (with-open-file (s (make-po-data-filepath "natures/nature.txt") :direction :input)
    (loop for line = (read-line s nil)
       while line collect (intern (string-upcase line) :keyword)))
  "List of the symbols standing for the 17 types in pokemon. Order is important!")