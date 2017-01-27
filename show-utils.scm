(define-module  (watch show-utils)
  #:export      (read-show-list-db
                 write-show-list-db
                 remove-show
                 make-show
                 find-show
                 print-show
                 show-name
                 show-path
                 show-current-episode
                 show-episode-list
                 show-current-episode-out-of-bounds?)
  #:use-module  (ice-9 ftw)
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
    (throw 'insufficient-permissions-exception
           "Insufficient permissions. Can't write to database.")))

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
(define (make-show show-name show-path starting-episode)
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
      (let ((format-string (string-append "name: ~a, show-current-episode: ~a" 
                                          (if verbose ", path: ~a~%" "~%"))))
        (if verbose
          (values 
             #t
             format-string
            (show-name show)
            (1+ (show-current-episode show))
            (show-path show))
          (values
             #t
             format-string
            (show-name show)
            (1+ (show-current-episode show))))))
     format))

;; ------------------------------------------------------ ;;
;; Get name of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the name of the show   ;;
;; ------------------------------------------------------ ;;
(define (show-name show)
  (car show))

;; ------------------------------------------------------ ;;
;; Get path of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the path to the show   ;;
;; ------------------------------------------------------ ;;
(define (show-path show)
  (cadr show))
;; ------------------------------------------------------ ;; ;; Get current episode of show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: an integer representing current episode of   ;;
;;           the show                                     ;;
;; ------------------------------------------------------ ;;
(define (show-current-episode show)
  (caddr show))

;; ------------------------------------------------------ ;;
;; Get a list of episodes of show.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;; 
;;                                                        ;;
;; #:return: a list of strings representing filepaths to  ;;
;;           episodes of show.                            ;;
;;           (essentially contents of directory located   ;;
;;           at (show-path show) filtered by their        ;; 
;;           extension)                                   ;;
;; ------------------------------------------------------ ;;
(define (show-episode-list show)
  (let ((episode-list 
          (scandir (if (eq? 'symlink (stat:type (lstat (show-path show)))) 
                     (readlink (show-path show))
                     (show-path show))
                   (lambda (filepath)
                     (let loop ((format-list config:episode-format-list))
                       (cond 
                         ((null? format-list) #f)
                         ((string-suffix-ci? (car format-list) filepath))
                         (else (loop (cdr format-list)))))))))
    (if (not episode-list)
      (throw 'directory-not-readable-exception
             "Can't read show directory contents.")
      episode-list)))

;; ------------------------------------------------------ ;;
;; Check whether current episode index of show is out     ;;
;; of bounds.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t - if current episode index < 0 or         ;;
;;                index >= length of episode list         ;;
;;           #f - otherwise                               ;;
;; ------------------------------------------------------ ;;
(define (show-current-episode-out-of-bounds? show)
  (let ((episode-list (show-episode-list show)))
    (or (<= (length episode-list)) (show-current-episode show)
        (> 0 (show-current-episode show)))))
