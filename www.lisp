;;; file containing all of our web stuff

(defvar *acceptor888* (make-instance 'hunchentoot:acceptor :port 8888))

(hunchentoot:start *acceptor888*)

(defun pprint-to-string (object)
  (with-output-to-string (s)
    (pprint object s)))
(hunchentoot:define-easy-handler (root :uri "/") ()
  "Nothing to see here move along!")

(hunchentoot:define-easy-handler (show-raw-packet-log
                                  :uri "/shanai/alpha/packets") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (pprint-to-string (loop for i from 1 to 100
                       for p in  pokemon.po.client::*po-socket-recv-log*
                       collect p)))


(hunchentoot:define-easy-handler (show-raw-packet-log
                                  :uri "/shanai/POServer/log") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (pprint-to-string (loop for i from 1 to 200
                       for p in pokemon.po.client::*sock-rcv-log*
                       collect p)))


(hunchentoot:define-easy-handler (show-raw-packet-log
                                  :uri "/shanai/POServer/battlelog") ()
  (setf (hunchentoot:content-type*) "text/html")
  (cl-who:with-html-output-to-string (*standard-output*)
    (:list
     (loop for p in pokemon.po.client::*sock-rcv-log*
        when (or (eql :battle-message (car p)) (eql :spectating-battle-message (car p)))
        do (cl-who:htm (:li (:b (princ (nth 9 (cdr p))))
                            (princ ": ")
                            (princ (nth 1 (cdr p)))
                            (princ " -- ")
                            (:b (princ (nth 3 (cdr p))))
                            (princ ":")
                            (princ (nth 5 (cdr p)))
                            (princ " ")
                            (:ul
                             (:ul
                              (:font :color :grey (princ (nth 7 (cdr p)))))
                             (:ul
                              (:font :color (if (eql :battle-message (car p))
                                                :blue
                                                :green) (print (nthcdr 11 p))))))))))
#+ ()  (pprint-to-string (loop 
                       for p in pokemon.po.client::*sock-rcv-log*
                       ;for i from 1 to 200
                       when (eql :spectating-battle-message (car p))
                       collect p)))




