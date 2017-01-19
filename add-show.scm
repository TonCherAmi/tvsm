(define-module  (watch add-show)
  #:export      (add-show)
  #:use-module  (ice-9 rdelim)
  #:use-module  (watch show-utils)
  #:use-module ((watch config)
                  #:prefix config:))

;; -------------------------------------------------------------------- ;;
;; Add show to the show database.                                       ;;
;; -------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show that ;;
;;                      is being added,                                 ;;
;;                      serves as a unique identifier, there cannot     ;;
;;                      be two shows with the same show-name in the     ;;
;;                      database                                        ;;
;;                                                                      ;; 
;; #:param: show-path - a string that is a path to the directory        ;; 
;;                      that contains the show                          ;;
;;                                                                      ;;
;; #:param: starting-episode - an integer that is the number of the     ;;
;;                             episode from which the show will         ;; 
;;                             begin to play, cannot be less than 1     ;;
;; -------------------------------------------------------------------- ;;
;; TODO: Add invalid starting episode number handling.                  ;;
;; -------------------------------------------------------------------- ;;
(define* (add-show show-name show-path #:optional (starting-episode 1))
  (let* ((new-show  (create-show show-name show-path starting-episode))
         (show-list (read-show-list))
         (show-list 
           (cond
             ;; If show with such a name already exists we ask user
             ;; whether he wishes to overwrite it, if the answer is
             ;; positive we overwrite it, otherwise we throw an
             ;; exception.
             ((and (find-show show-name show-list)
                   (ask-user-overwrite show-name))
              (cons new-show (remove-show show-name show-list)))
             ((not (find-show show-name show-list))
              (cons new-show show-list))
             (else (throw 'show-already-exists-exception)))))
    (write-show-list show-list)))

;; -------------------------------------------------------------------- ;;
;; Ask user whether they'd like to overwrite already existing show      ;;
;; -------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show      ;;
;; -------------------------------------------------------------------- ;;
(define (ask-user-overwrite show-name)
  (let loop ((ask-message (format #f 
                                  "A show called '~a' already exists, overwrite? (y/n): " 
                                  show-name)))
    (display ask-message)
    (let ((answer (read-line)))
      (cond
        ((eof-object? answer) #f)
        ((or (string-ci=? answer "y") (string-ci=? answer "yes")) #t)
        ((or (string-ci=? answer "n") (string-ci=? answer "no"))  #f)
        ;; If the answer is neither 'yes' or 'y' nor 'no' or 'n'
        ;; loop until the requested answer is received.
        (else (loop "Please answer (y/n): "))))))
