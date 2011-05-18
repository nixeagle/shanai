(in-package :shanai.rpg.database)


(defun insert-rpg-player (player)
  (query (sql (:insert-into 'rpg.players
                            (:values
                             (name player))))))

(defun select-rpg-player-by-name (name)
  (declare (type string name))
  (query (sql (:select '* :from 'rpg.players :where (:= 'name name)))
         :plist))

(defun select-rpg-player-by-id (id)
  (declare (type alexandria:non-negative-fixnum id))
  (query (sql  (:select '* :from 'rpg.players
                       :where (:= 'id id))) :plist))


(insert-rpg-player (make-instance 'rpg-global::rpg-player
                                  :name "nixeagle"))



(query "ALTER TABLE players OWNER TO eagle;")
(sql (:create-table name
                    (("name" :type (varchar 20)))))