;;; file containing all of our web stuff
(in-package :cl-user)
(defvar *acceptor888* (make-instance 'hunchentoot:acceptor :port 8888))

(hunchentoot:start *acceptor888*)

(defun pprint-to-string (object)
  (with-output-to-string (s)
    (pprint object s)))
(hunchentoot:define-easy-handler (root :uri "/") ()
  "Nothing to see here move along!")


(setq tbnl:*dispatch-table*
      (list 'tbnl:dispatch-easy-handlers
            (tbnl:create-folder-dispatcher-and-handler
             "/" "c:/cygwin/home/Tim/hackage/lisp/shanai/pub/")))

(hunchentoot:define-easy-handler (show-raw-packet-log
                                  :uri "/shanai/POServer/log") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (pprint-to-string (loop for i from 1 to 200
                       for p in pokemon.po.client::*sock-rcv-log*
                       collect p)))
(defvar *battle-logs*)
(defun filter-sock-recv-log ()
  (setf *battle-logs*
        (loop for p in pokemon.po.client::*sock-rcv-log*
           when (or (eql :battle-message (car p)) (eql :spectating-battle-message (car p)))
           collect p)))
(hunchentoot:define-easy-handler (show-raw-packet-battlelog
                                  :uri "/shanai/POServer/battlelog") ()
;  (setf (hunchentoot:content-type*) "text/html")
  (filter-sock-recv-log)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:h1 "Battle log")
    (:list
     (loop for p in *battle-logs*
          for i from 0 to 1000
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


(setq tbnl:*show-lisp-errors-p* t)
(defmacro with-toplevel-html (&body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t
                                                         :indent t)
     ,@body))

(defmacro with-hout (&body body)
  `(cl-who:with-html-output (*standard-output* nil :indent t)
     ,@body))

(setq cl-who:*prologue*
      "<!DOCTYPE html>")

(defmacro with-html-head ((&key title base-url (charset :utf-8)) &body body)
  `(with-hout
    (:head
     (:meta :charset ,charset)
     (:base :href ,base-url)
     (:title ,title)
     ,@body)))
(defun visitor-nixeagle-p ()
  (or (string= "24.209.52.11"
               (tbnl:real-remote-addr))
      (string= "99.73.33.129"
               (tbnl:real-remote-addr))
      (string= "108.85.126.36"
               (tbnl:real-remote-addr))))

(tbnl:define-easy-handler (api-battles-current
                           :uri "/api/battles/current")
    ()
  (setf (hunchentoot:content-type*) "text/json")
  (json:encode-json-to-string shanai.po.client::*current-battle*))
(defvar *last-request* nil)
(hunchentoot:define-easy-handler (shanai-root
                                  :uri "/shanai") (msg)
  (tbnl:no-cache)
  (setq *last-request* tbnl:*request*)
  (when msg
      (send-channel-message))
  (let ((tbnl:*default-connection-timeout* 100))
    
    (with-toplevel-html
      (:html :lang :en
             (with-html-head (:title "Shanai's Home"
                                     :base-url "http://i.nixeagle.net:7777/")
               (:link :rel "stylesheet" :type "text/css"
                      :href "master.css")
               (:script :src "jquery-1.6.js")
             #+ ()  (:script :type "text/javascript"
                         :src "master.js")
               (:script :src "demo-channel-convo.js")
               (:meta :generator "cl-who"))           
             (:body (:header #+ () (:p "hi how are you?"))
                    
                    (:form
                     (:input :type "text" :id "txt"
                             :name "msg"
                             :onkeypress "return runScript(event);"))
                  #+ ()  (:a "a link")
                  (:div
                   (:a :href "html-tests.html"
                       "Testing layout for battle views.")
                 )
                  (:team
                   (:name "Bulbasaur")
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon (:name "Bulbasaur"))
                   (:pokemon :id "foo" (:name "Bulbasaur")))
                   (:code (cl-who:str (visitor-nixeagle-p))))))))

(tbnl:define-easy-handler (send-channel-message :uri "/api/beta/send/channel-message/") (msg)
  (when msg
    (when (visitor-nixeagle-p)
      (po-proto:write-channel-message msg
                                      (pokemon.po.client::get-stream pokemon.po.client::@po-alpha-socket@)
                                      :channel-id (generic:object-id (shanai.po.client:get-channel "Hackage" pokemon.po.client::@po-alpha-socket@)))
       (force-output (pokemon.po.client::get-stream pokemon.po.client::@po-alpha-socket@)))))
(tbnl:define-easy-handler (get-beta-channel-log :uri "/api/beta/channel/log")
    (id)
  (let ((id (or (and (not (eq nil id)) (parse-integer id :junk-allowed t)) 0)))
    (cl-who:with-html-output-to-string (*standard-output*)
      (mapcar (lambda (msg)
                (cl-who:htm (:p (cl-who:str msg))))
              (reverse
               (loop for i from 0 to 10000
                  for msg in pokemon.po.client::*shanai-channel-messages*
                  when (<= id i)
                  collect msg))))))
(tbnl:define-easy-handler (master-script :uri "/master.js") ()
  (setf (hunchentoot:content-type*) "text/plain")
  #+ () (parenscript:ps
    (let (time timer-on-p (id 0))
      (defun do-timer ()
        (unless timer-on-p
          (setq timer-on-p t)
          (timed-count)))
      (defun timed-count (&optional (count 0))
        (setf (parenscript:chain document (get-element-by-id :txt) value)
              count
              time (set-timeout (lambda ()
                                  (timed-count (1+ count))
                                 #+ () (parenscript:chain ($ (get "/api/beta/channel/log" nil (lambda (data)
                                                                                                    (parenscript:chain ($ "#foo") (after data))
                                                                                                    (setq id 0)))))
                                  (parenscript:chain ($ "#foo") (after "<p>hi</p>"))) 1000))))))