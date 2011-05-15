
;;; Small wrapper over the binary-data package written by gigamonkeys.

(asdf:defsystem :shanai.binary-data
  :depends-on (:com.gigamonkeys.binary-data :flexi-streams)
  :components
  ((:file "binary-data")))