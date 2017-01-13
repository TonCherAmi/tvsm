(use-modules (watch config)
             (ice-9 optargs)
             (ice-9 rdelim)
             (srfi srfi-1))

(define* (add-show show-name show-path #:optional (starting-episode 1))
  (let* ((new-show  (list show-name show-path starting-episode))
         (show-list (call-with-input-file config:show-database-path read-show-list))
         (show-list 
           (cond
             ((and (contains-show? show-name show-list) 
                    config:ask-on-existing-show-overwrite?
                   (ask-whether-to-overwrite show-name))
              (cons new-show (delete-show show-name show-list)))
; ----------------------------------------------------------------------------------
             ((and (contains-show? show-name show-list)
                   (not config:ask-on-existing-show-overwrite?))
              (cons new-show (delete-show show-name show-list)))
; ----------------------------------------------------------------------------------
             ((not (contains-show? show-name show-list))
              (cons new-show (show-list)))
; ----------------------------------------------------------------------------------
             (else (throw 'show-already-exists-exception)))))
    (call-with-output-file config:show-database-path write-show-list)))

(define (read-show-list port)
  (let loop ((show-list '()))
    (let ((show (read port)))
      (if (eof-object? show)
        show-list
        (loop (cons show show-list))))))

; TODO (define (write-show-list port)

(define (contains-show? show-name show-list)
  (null? (filter (lambda (x)
                   (equal? show-name (car x)))
                 show-list)))

(define (delete-show show-name show-list)
  (delete show-name show-list (lambda (x y)
                                (equal? x (car y)))))

(define (ask-whether-to-overwrite-show show-name)
  (let loop ((ask-message (format #f "A show with name ~a already exists,
                                      would you like to overwrite it? (y/n): " show-name)))
    (display ask-message)
    (let ((answer (read-line)))
      (cond
        ((string-ci=? answer "y" "yes") #t)
        ((string-ci=? answer "n" "no")  #f)
        (else (loop "Please answer (y/n): "))))))



