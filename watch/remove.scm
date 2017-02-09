(define-module (watch remove)
  #:export     (remove-show-db
                remove-over-db)
  #:use-module (watch show-utils))

;; ---------------------------------------------------- ;;
;; Remove show called show-name from the show database. ;;
;; ---------------------------------------------------- ;;
;; #:param: show-name - name of the show to remove      ;;
;; ---------------------------------------------------- ;;
(define (remove-show-db show-name)
  (let ((new-show-list 
          (let ((show-list-db (read-show-list-db)))
            (if (not (find-show show-name show-list-db))
              (throw 'show-not-found-exception
                     (format #f "cannot remove '~a': No such show" show-name))
              (remove-show show-name show-list-db)))))
    (write-show-list-db new-show-list)))

;; ---------------------------------------------------- ;;
;; Remove shows that are over from the database.        ;;
;; ---------------------------------------------------- ;;
(define (remove-over-db)
  (let ((new-show-list
          (let ((show-list-db (read-show-list-db)))
            (filter 
              (lambda (show) 
                (not (show-over? show)))
              show-list-db))))
    (write-show-list-db new-show-list)))
