(define-module  (watch play-show)
  #:export      (play-show-db)
  #:use-module  (ice-9 ftw)
  #:use-module  (watch show-utils)
  #:use-module ((watch config)
                  #:prefix config:))

;; ----------------------------------------------------------- ;;
;; Play current-episode of show called show-name and increment ;;
;; its current-episode-index.                                  ;;
;; ----------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the  ;;
;;                      show that is played                    ;;
;; ----------------------------------------------------------- ;;
(define (play-show-db show-name)
  (let* ((show-list (read-show-list-db))
         (show (find-show show-name show-list)))
    (if (not show) 
      ;; If show is not found in the db throw an exception.
      (throw 'show-not-found-exception
             (format #f "A show called '~a' is not found." show-name))
      (cond 
        ((show-over? show) 
         (throw 'show-is-over-exception "The show is over."))
        ((show-current-episode-out-of-bounds? show) 
         (throw 'episode-out-of-bounds-exception "Current episode number is out of bounds."))
        (else 
           (let* ((episode-list (show-episode-list show))
                  (current-episode-path 
                    (format #f "~a/~a" (show-path show) (list-ref episode-list (show-current-episode show)))))
             (if (not (zero? (play-episode current-episode-path)))
               (throw 'external-command-fail-exception "Media player command failed.")
               (let* ((updated-show (show-current-episode-inc show))
                      (updated-show-list (cons updated-show (remove-show show-name show-list))))
                 (write-show-list-db updated-show-list)))))))))

;; ------------------------------------------------------------ ;;
;; Play an episode using user-defined media player command.     ;;
;; ------------------------------------------------------------ ;;
;; #:param: episode-path - a string representing full path to a ;;
;;                         media file representing an episode   ;;
;;                                                              ;;
;; #:return: zero is returned on successful command execution   ;;
;;           non-zero error code is returned otherwise          ;;
;; ------------------------------------------------------------ ;;
(define (play-episode episode-path)
  ;; This doesn't work for paths with double quotes in them
  ;; but who would put a double quote in a filename anyway?
  (system (format #f "~a \"~a\"" config:media-player-command episode-path)))
