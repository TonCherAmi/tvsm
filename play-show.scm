(define-module  (watch play-show)
  #:export      (play-show-db)
  #:use-module  (ice-9 ftw)
  #:use-module  (watch show-utils)
  #:use-module ((watch config)
                  #:prefix config:))

;; ------------------------------------------------------------------------ ;;
;; Play an episode of show called show-name.                                ;;
;; ------------------------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of the show that is  ;;
;;          played                                                          ;;
;; #:param: increment? - a boolean value that determines whether show's     ;;
;;          current-episode index will be incremented after current episode ;;
;;          is played. #t by default                                        ;;
;; #:param: custom-episode-index - an integer that will be used instead of  ;;
;;          shows's current-epsidoe index. #f by default                    ;; 
;; ------------------------------------------------------------------------ ;;
(define* (play-show-db show-name #:key (increment? #t) (custom-episode-index #f))
  (let* ((show-list (read-show-list-db))
         (show (let ((show-db (find-show show-name show-list)))
                 (cond 
                   ;; If show is not found in the db throw an exception.
                   ((not show-db)
                    (throw 'show-not-found-exception
                           (format #f "A show called '~a' is not found." show-name)))
                   ;; If custom-episode-index was passed then make a show that has it as its 
                   ;; current-episode index.
                   (custom-episode-index 
                    (make-show (show:name show-db) (show:path show-db) custom-episode-index))
                   (else show-db)))))
    (cond 
      ((show-over? show) 
       (throw 'show-is-over-exception "The show is over."))
      ((show:current-episode-out-of-bounds? show) 
       (throw 'episode-out-of-bounds-exception "Episode index is out of bounds."))
      (else 
        (let* ((episode-list (show:episode-list show))
               (current-episode-path 
                 (format #f "~a/~a" (show:path show) (list-ref episode-list (show:current-episode show)))))
          (cond
            ((not (zero? (play-episode current-episode-path)))
             (throw 'external-command-fail-exception "Media player command failed."))
            (increment?
             (let* ((updated-show (show:current-episode-inc show))
                    (updated-show-list (cons updated-show (remove-show show-name show-list))))
              (write-show-list-db updated-show-list)))))))))

;; ------------------------------------------------------------ ;;
;; Play an episode using user-defined media player command.     ;;
;; ------------------------------------------------------------ ;;
;; #:param: episode-path - a string representing full path to a ;;
;;          media file representing an episode                  ;;
;;                                                              ;;
;; #:return: zero is returned on successful command execution   ;;
;;           non-zero error code is returned otherwise          ;;
;; ------------------------------------------------------------ ;;
(define (play-episode episode-path)
  ;; This doesn't work for paths with double quotes in them
  ;; but who would put a double quote in a filename anyway?
  (system (format #f "~a \"~a\"" config:media-player-command episode-path)))
