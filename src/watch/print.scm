(define-module (watch print)
  #:export     (print-show-list-db)
  #:use-module (watch show-utils))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: verbose - make output more detailed                ;;
;; ----------------------------------------------------------- ;;
(define* (print-show-list-db #:optional (verbose #f))
  (let ((show-list (read-show-list-db)))
    (if verbose (format #t "total: ~a~%" (length show-list)))
    (for-each 
      (lambda (s) (print-show s verbose))
       show-list)))
