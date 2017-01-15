(define-module (watch add-show)
  #:export     (add-show)
  #:use-module (ice-9 rdelim)
  #:use-module ((watch config)
                #:prefix config:))

(define* (add-show show-name show-path #:optional (starting-episode 1))
  (let* ((new-show  (list show-name show-path starting-episode))
         (show-list (read-show-list))
         (show-list 
           (cond
             ((and (assoc show-name show-list)
                    config:ask-on-existing-show-overwrite?
                   (ask-whether-to-overwrite-show show-name))
              (cons new-show (assoc-remove! show-list show-name)))
; ----------------------------------------------------------------------------------
             ((and (assoc show-name show-list)
                   (not config:ask-on-existing-show-overwrite?))
              (cons new-show (assoc-remove! show-list show-name)))
; ----------------------------------------------------------------------------------
             ((not (assoc show-name show-list))
              (cons new-show show-list))
; ----------------------------------------------------------------------------------
             (else (throw 'show-already-exists-exception)))))
    (write-show-list show-list)))

(define (read-show-list)
  (with-input-from-file
    config:show-database-path
    read))

(define (write-show-list show-list)
  (with-output-to-file
    config:show-database-path
    (lambda ()
      (write show-list))))

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
