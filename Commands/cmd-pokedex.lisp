(in-package :shanai.po.bot)

(define-bot-command pokedex (con target user args)
  (let ((int (parse-integer args :junk-allowed t)))
        (if int
            (let ((poke (gethash int pokemon::*pokedex*)))
              (reply (if poke
                         (format nil "<b>#~A</b> <i>~A</i> ~A" (shanai.pokemon:pokemon-id poke) (cl-who:escape-string (name poke)) (shanai.pokemon:pokemon-base-stats poke))
                                 "Could not find it! Did you typo?")))
            (let ((poke (pokemon.po.client::find-pokemon-by-name args)))
              (reply (if poke (format nil "<b>#~A</b> <i>~A</i> ~A" (shanai.pokemon:pokemon-id poke) (cl-who:escape-string (name poke)) (shanai.pokemon:pokemon-base-stats poke))
                                 "Could not find it! Did you typo?"))))))


(define-bot-command multiply (con target user args)
  (cl-ppcre:register-groups-bind ((#'parse-integer a b)) ("(\\d+)\\s+(\\d+)" args)
    (reply (format nil "~A :: <b>STAB:</b> ~A" (* a b) (floor (/ (* a b 3) 2))))))