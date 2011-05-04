;;; temporary ccl initialization

(declaim (optimize (debug 3) (safety 3) (compilation-speed 0)))

(setq ccl:*fasl-save-definitions* t
      ccl:*load-xref-info* t
      ccl:*save-definitions* t
      ccl:*record-xref-info* t
      ccl:*short-site-name* "lappy"
      ccl:*SAVE-DOC-STRINGS* t
      ccl:*long-site-name* ccl:*short-site-name*)


(in-package :common-lisp-user)
(defun loaded-modules-list ()
  "List of all currently loaded modules in the lisp image."
  #+:ccl ccl::*modules*)


(unless (find "c:/cygwin/home/Tim/hackage/lisp/www-shanai/"
              asdf:*central-registry* :test #'equalp)
  (setq asdf:*central-registry*
        (cons "c:/cygwin/home/Tim/hackage/lisp/www-shanai/"
              asdf:*central-registry*)))
;; copy-instance awesome!

;; decode-string-from-octets encode-string-to-octets

;; DOTTED-TO-IPADDR

;; enclose

;; 