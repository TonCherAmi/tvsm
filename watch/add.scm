(define-module  (watch add)
  #:export      (add-show-db)
  #:use-module  (watch show-utils)
  #:use-module  (ice-9 rdelim))

;; ---------------------------------------------------------------------- ;;
;; Add show to the show database.                                         ;;
;; ---------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show that   ;;
;;          is being added.                                               ;;
;;          Serves as a unique identifier, there cannot  be two shows     ;;
;;          with the same name in the db.                                 ;;
;;                                                                        ;; 
;; #:param: show-path - a string that is a path to the directory that     ;; 
;;          contains the show                                             ;;
;;                                                                        ;;
;; #:param: starting-episode - an integer that is the index of the        ;;
;;          episode from which the show will begin to play                ;; 
;; ---------------------------------------------------------------------- ;;
(define* (add-show-db show-name show-path #:optional (starting-episode 0))
  (let ((new-show (make-show show-name show-path starting-episode)))
    (if (show:current-episode-out-of-bounds? new-show)
        ;; Throw an exception if starting episode index is out of bounds.
        (throw 'episode-out-of-bounds-exception 
               (format #f "cannot add '~a': Starting episode index is out of bounds" show-name))
        (let ((new-show-list 
                (let ((show-list-db (read-show-list-db)))
                  (cond
                    ((not (find-show show-name show-list-db))
                     (cons new-show show-list-db))
                    ;; If show with such a name already exists we ask user
                    ;; whether they would like to overwrite it, if the answer 
                    ;; is positive we overwrite it, otherwise we throw an
                    ;; exception.
                    ((ask-user-overwrite show-name)
                     (cons new-show (remove-show show-name show-list-db)))
                    (else
                      (throw 'show-already-exists-exception
                             (format #f "cannot add '~a': Show already exists" show-name)))))))
               (write-show-list-db new-show-list)))))
