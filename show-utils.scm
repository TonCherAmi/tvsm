(define-module  (watch show-utils)
  #:export      (read-show-list-db
                 write-show-list-db
                 remove-show
                 create-show
                 find-show
                 print-show
                 get-show-name
                 get-show-path
                 get-show-current-episode)
  #:use-module ((watch config)
                  #:prefix config:))

;; ------------------------------------------------------ ;;
;; Read show-list database.                               ;;
;; ------------------------------------------------------ ;;
;; #:return: show list (list is empty if the database is  ;;
;;           empty)                                       ;;
;; ------------------------------------------------------ ;;
(define (read-show-list-db)
  (if (access? config:show-database-path R_OK)
    (with-input-from-file 
      config:show-database-path
      read)
    (list)))

;; ------------------------------------------------------ ;;
;; Write show-list database.                              ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - a show list                       ;; 
;; ------------------------------------------------------ ;;
(define (write-show-list-db show-list)
  (if (access? config:resources-directory W_OK)
    (with-output-to-file
      config:show-database-path
      (lambda ()
        (write show-list)))
    (throw 'insufficient-permissions-exception)))

;; ------------------------------------------------------ ;;
;; Remove show named show-name from show-list. Original   ;;
;; list remains unmodified.                               ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;                      the show that is being removed    ;;
;; #:param: show-list - a show-list                       ;;
;;                                                        ;; 
;; #:return: show-list without the show named show-name   ;;
;; ------------------------------------------------------ ;;
(define (remove-show show-name show-list)
  (assoc-remove! show-list show-name))

;; ------------------------------------------------------ ;;
;; Create a show named show-name, located at show-path    ;;
;; that will start playing from starting-episode.         ;;
;; A show is a list of three elements:                    ;; 
;;  - First one is this show's name.                      ;;
;;  - Second one is this show's path.                     ;;
;;  - Third one is this show's current episode.           ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;                      the new show                      ;;
;; #:param: show-path - a string representing the path to ;;
;;                      the new show                      ;;
;; #:param: starting-episode - an integer representing    ;; 
;;                             the number of the episode  ;;
;;                             from which the new show    ;;
;;                             will begin to play         ;;
;;                                                        ;;
;; #:return: a newly created show                         ;;
;; ------------------------------------------------------ ;;
(define (create-show show-name show-path starting-episode)
  (list show-name show-path starting-episode))

;; ------------------------------------------------------ ;;
;; Find show named show-name in show-list.                ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;                      the show that we're looking for   ;;
;; #:param: show-list - a show list to search             ;;
;;                                                        ;;
;; #:return: in case the show was found - the show itself ;;
;;           otherwise - #f                               ;;
;; ------------------------------------------------------ ;;
(define (find-show show-name show-list)
  (assoc show-name show-list))

;; ------------------------------------------------------ ;;
;; Print show contents to (current-output-port)           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show to print                        ;;
;; #:param: verbose - make output more detailed           ;;
;; ------------------------------------------------------ ;;
(define* (print-show show #:optional (verbose #f))
  (call-with-values 
    (lambda () 
      (let ((format-string (string-append "name: ~a, current-episode: ~a" 
                                          (if verbose ", path: ~a~%" "~%"))))
        (if verbose
          (values 
             #t
             format-string
            (get-show-name show)
            (1+ (get-show-current-episode show))
            (get-show-path show))
          (values
             #t
             format-string
            (get-show-name show)
            (1+ (get-show-current-episode show))))))
     format))

;; ------------------------------------------------------ ;;
;; Get show name of show.                                 ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the name of the show   ;;
;; ------------------------------------------------------ ;;
(define (get-show-name show)
  (car show))

;; ------------------------------------------------------ ;;
;; Get show path of show.                                 ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the path to the show   ;;
;; ------------------------------------------------------ ;;
(define (get-show-path show)
  (cadr show))

;; ------------------------------------------------------ ;;
;; Get current episode of show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: an integer representing current episode of   ;;
;;           the show                                     ;;
;; ------------------------------------------------------ ;;
(define (get-show-current-episode show)
  (caddr show))
