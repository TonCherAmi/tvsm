(define-module (watch set)
  #:export     (set-current-episode-index-db
                jump-to-next-episode-db
                jump-to-previous-episode-db)
  #:use-module (watch show-utils))

;; ------------------------------------------------------ ;;
;; Set current episode index of show called show-name     ;;
;; in the db.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show whose index is being modified        ;;
;; #:param: new-index - an integer representing the new   ;;
;;          index that will be set                        ;;
;; ------------------------------------------------------ ;;
(define (set-current-episode-index-db show-name new-index)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show 
                   (let ((show-db (find-show show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot set current episode index for '~a': No such show" show-name))
                       (make-show (show:name show-db) (show:path show-db) new-index)))))
            (if (show:current-episode-out-of-bounds? new-show)
              (throw 'episode-out-of-bounds-exception
                     (format #f "cannot set current episode index for '~a': index out of bounds" show-name))
              (cons new-show (remove-show show-name show-list-db))))))
    (write-show-list-db new-show-list)))

;; ------------------------------------------------------ ;;
;; Jump to next episode of show called show-name.         ;;
;;                                                        ;;
;; Essentially what this does is it increments current-   ;;
;; episode index of the specified show and writes         ;;
;; the result to the show db.                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show whose index is being incremented     ;;
;; ------------------------------------------------------ ;;
(define (jump-to-next-episode-db show-name)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show
                   (let ((show-db (find-show show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot jump to next episode for '~a': No Such show" show-name))
                       (show:current-episode-inc show-db)))))
            (cons new-show (remove-show show-name show-list-db)))))
    (write-show-list-db new-show-list)))

;; ------------------------------------------------------ ;;
;; Jump to previous episode of show called show-name.     ;;
;;                                                        ;;
;; Essentially what this does is it decrements current-   ;;
;; episode index of the specified show and writes         ;;
;; the result to the show db.                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show whose index is being decremented     ;;
;; ------------------------------------------------------ ;;
(define (jump-to-previous-episode-db show-name)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show
                   (let ((show-db (find-show show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot jump to previous episode for '~a': No Such show" show-name))
                       (show:current-episode-dec show-db)))))
            (cons new-show (remove-show show-name show-list-db)))))
    (write-show-list-db new-show-list)))
