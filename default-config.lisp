;;; some sort of global config... should this be in it's own package?

(in-package :shanai.config)

(defvar *po-root-directory*
  "/home/eagle/pogeymon-online/"
  "Full path to the Pokemon Online installation files.

Ensure that this line ends with a trailing slash!")

(defun po-root-directory ()
  *po-root-directory*)

(defvar *google-translate-api-key* ""
  "Key for using google's translation api. Sign up for one on google if you
  do not have one.")

(defun google-translate-api-key ()
  *google-translate-api-key*)

(in-package :pokemon)

;;; A temporary hack until all references to *po-directory* are converted
;;; to conf:po-root-directory
(defvar *po-directory* (conf:po-root-directory)
  "Full path to where Pokemon Online source/installation is.

Make sure your file path ends with a trailing slash!")