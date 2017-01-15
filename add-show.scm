(define-module  (watch add-show)
  #:export      (add-show)
  #:use-module  (ice-9 rdelim)
  #:use-module  (watch util)
  #:use-module ((watch config)
                  #:prefix config:))

(define* (add-show show-name show-path #:optional (starting-episode 1))
  (let* ((new-show  (create-show show-name show-path starting-episode))
         (show-list (read-show-list))
         (show-list 
           (cond
             ((and (find-show show-name show-list)
                    config:ask-on-existing-show-overwrite?
                   (ask-whether-to-overwrite-show show-name))
              (cons new-show (remove-show show-name show-list)))
; ----------------------------------------------------------------------------------
             ((and (find-show show-name show-list)
                   (not config:ask-on-existing-show-overwrite?))
              (cons new-show (remove-show show-name show-list)))
; ----------------------------------------------------------------------------------
             ((not (find-show show-name show-list))
              (cons new-show show-list))
; ----------------------------------------------------------------------------------
             (else (throw 'show-already-exists-exception)))))
    (write-show-list show-list)))

(define (ask-whether-to-overwrite-show show-name)
  (let loop ((ask-message (format #f 
                                  "A show with name '~a' already exists, overwrite? (y/n): " 
                                  show-name)))
    (display ask-message)
    (let ((answer (read-line)))
      (cond
        ((or (string-ci=? answer "y") (string-ci=? answer "yes")) #t)
        ((or (string-ci=? answer "n") (string-ci=? answer "no"))  #f)
        (else (loop "Please answer (y/n): "))))))
