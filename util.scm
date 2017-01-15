(define-module  (watch util)
  #:export      (read-show-list
                 write-show-list
                 remove-show
                 create-show
                 find-show)
  #:use-module ((watch config)
                  #:prefix config:))

(define (read-show-list)
  (if (access? config:show-database-path R_OK)
    (with-input-from-file 
      config:show-database-path
      read)
    (list)))

(define (write-show-list show-list)
  (if (access? config:resources-directory W_OK)
    (with-output-to-file
      config:show-database-path
      (lambda ()
        (write show-list)))
    (throw 'insufficient-permissions-exception)))

(define (remove-show show-name show-list)
  (assoc-remove! show-list show-name))

(define (create-show show-name show-path starting-episode)
  (list show-name show-path starting-episode))

(define (find-show show-name show-list)
  (assoc show-name show-list))
