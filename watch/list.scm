(define-module (watch list)
  #:export     (list-shows-db)
  #:use-module (watch show-utils))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: verbose - make output more detailed                ;;
;; ----------------------------------------------------------- ;;
(define* (list-shows-db #:optional (verbose #f))
  (let ((show-list (read-show-list-db)))
    (if verbose (format #t "total: ~a~%" (length show-list)))
    (for-each 
      (lambda (s) (print-show s verbose))
       show-list)))

;; ------------------------------------------------------ ;;
;; Print show contents to (current-output-port)           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show to print                        ;;
;; #:param: verbose - make output more detailed           ;;
;; ------------------------------------------------------ ;;
(define* (print-show show #:optional (verbose #f))
  (call-with-values 
    (lambda () 
      (let ((format-string (string-append "~a\t" 
                                          (if verbose "~a\t~a~%" ""))))
        (if verbose
          (values 
             #t
             format-string
            (show:name show)
            (if (number? (show:current-episode show))
                  (1+ (show:current-episode show))
                  (show:current-episode show))
            (show:path show))
          (values
             #t
             format-string
            (show:name show)))))
     format))
