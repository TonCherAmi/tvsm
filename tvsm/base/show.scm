;; tvsm - a tv show manager.
;; Copyright © 2017 Vasili Karaev
;;
;; This file is part of tvsm.
;;
;; tvsm is free software: you can redistribute  it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; tvsm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHENTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with tvsm. If not, see <http://www.gnu.org/licenses/>.

(define-module (tvsm base show)
  #:export     (call-with-show-list
                make-show
                remake-show
                show:name
                show:path
                show:date
                show:airing?
                show:ep/index
                show:ep/offset
                show:ep/current
                show:ep/watched
                show:ep/index-inc
                show:ep/index-dec
                show:ep/list
                show:watchable?
                show:finished?
                show:ep/index-out-of-bounds?
                remove-show
                find-show)
  #:use-module (ice-9 ftw)
  #:use-module (tvsm syntax call-if)
  #:use-module (tvsm base db)
  #:use-module (tvsm base config)
  #:use-module (tvsm util path))

;; ------------------------------------------------------- ;;
;; Read the show database and call '(proc show-list)'      ;;
;; with the read show-list.                                ;;
;; ------------------------------------------------------- ;;
;; #:param: proc :: [show] -> [show] - must return a show  ;;
;;          list only if 'overwrite' is #t                 ;;
;; #:param: overwrite :: bool - if #t show-list database   ;;
;;          will be overwritten with the result of 'proc'  ;;
;; ------------------------------------------------------- ;;
(define* (call-with-show-list #:key proc overwrite)
  (let* ((show-list-db  (read-show-list-db))
         (new-show-list (proc show-list-db)))
    (when (and overwrite new-show-list)
      (write-show-list-db new-show-list))))

;; ------------------------------------------------------ ;;
;; Create a show.                                         ;;
;; ------------------------------------------------------ ;;
;; #:param: name :: string - show name                    ;;
;;                                                        ;;
;; #:param: path :: string - path to the show directory   ;;
;;                                                        ;;
;; #:param: date :: int - show creation date (Unix time)  ;;
;;                                                        ;;
;; #:param: airing? :: bool - if #t show is not           ;;
;;          considered finished when all of its episodes  ;;
;;          have been watched                             ;;
;;                                                        ;;
;; #:param: ep/index :: int - episode index, points at    ;;
;;          the current episode inside an episode list    ;;
;;                                                        ;;
;; #:param: ep/offset :: int - episode offset, generally  ;;
;;          only useful for shows whose first episode is  ;;
;;          numbered differently than 'E01'               ;;
;;                                                        ;;
;; #:param: ep/current :: int - current episode number    ;;
;;          as specified by the user, can be specified    ;;
;;          instead of 'ep/index'                         ;;
;;                                                        ;;
;; #:return: x :: show - show                             ;;
;; ------------------------------------------------------ ;;
(define* (make-show #:key name
                          path
                          date
                          airing?
                          ep/index
                          ep/offset
                          ep/current)
  `((name      . ,name)
    (path      . ,path)
    (date      . ,date)
    (airing?   . ,airing?)
    (ep/index  . ,(if ep/current
                    (- ep/current ep/offset 1)
                    ep/index))
    (ep/offset . ,ep/offset)))

;; ------------------------------------------------------ ;;
;; Remake a show using either newly specified values or   ;;
;; old ones from the 'show' argument.                     ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;; ------------------------------------------------------ ;;
;; For information on other parameters take a look at     ;;
;; the specification of 'make-show'.                      ;;
;; ------------------------------------------------------ ;;
(define* (remake-show show #:key (name (show:name show))
                                 (path (show:path show))
                                 (date (show:date show))
                                 (airing? (show:airing? show))
                                 (ep/index (show:ep/index show))
                                 (ep/offset (show:ep/offset show))
                                 (ep/current #f))
  (make-show #:name name
             #:path path
             #:date date
             #:airing? airing?
             #:ep/index ep/index
             #:ep/offset ep/offset
             #:ep/current ep/current))

;; ------------------------------------------------------ ;;
;; Get name of a show.                                    ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: string - show name                      ;;
;; ------------------------------------------------------ ;;
(define (show:name show)
  (assq-ref show 'name))

;; ------------------------------------------------------ ;;
;; Get path of a show.                                    ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: string - show path                      ;;
;; ------------------------------------------------------ ;;
(define (show:path show)
  (assq-ref show 'path))

;; ------------------------------------------------------ ;;
;; Get creation date of a show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: int - show creation date (Unix time)    ;;
;; ------------------------------------------------------ ;;
(define (show:date show)
  (assq-ref show 'date))

;; ------------------------------------------------------ ;;
;; Check whether show is airing.                          ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;; 
;; #:return: x :: bool - #t if show is airing,            ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show:airing? show)
  (assq-ref show 'airing?))

;; ------------------------------------------------------ ;;
;; Get episode index of a show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: int - episode index                     ;;
;; ------------------------------------------------------ ;;
(define (show:ep/index show)
  (assq-ref show 'ep/index))

;; ------------------------------------------------------ ;;
;; Get episode offset of a show.                          ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: int - episode offset                    ;;
;; ------------------------------------------------------ ;;
(define (show:ep/offset show)
  (assq-ref show 'ep/offset))

;; ------------------------------------------------------ ;;
;; Get current episode number of a show.                  ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: int - current episode number            ;;
;; ------------------------------------------------------ ;;
(define (show:ep/current show)
  (+ 1 (show:ep/index show) (show:ep/offset show)))

;; ------------------------------------------------------ ;;
;; Get number of episodes that the user has already seen. ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: int - number of episodes watched        ;;
;; ------------------------------------------------------ ;;
(define (show:ep/watched show)
  (+ (show:ep/index show) (show:ep/offset show)))

;; ------------------------------------------------------ ;;
;; Increment episode index of a show.                     ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: show - the same show with incremented   ;;
;;           episode index                                ;;
;; ------------------------------------------------------ ;;
(define (show:ep/index-inc show)
  (remake-show show
               #:ep/index
                 (1+ (show:ep/index show))))

;; ------------------------------------------------------ ;;
;; Decrement episode index of a show.                     ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: show - the same show with decremented   ;;
;;           episode index if episode index is > 0,       ;;
;;           otherwise the same show with no changes      ;;
;; ------------------------------------------------------ ;;
(define (show:ep/index-dec show)
  (if (>= 0 (show:ep/index show))
    show
    (remake-show show
                 #:ep/index
                   (1- (show:ep/index show)))))

;; ------------------------------------------------------ ;;
;; Get episode filelist of a show.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: [string] - episode filelist of a show   ;;
;;           (essentially contents of a directory located ;;
;;           at (show:path show) filtered by their        ;; 
;;           extension)                                   ;;
;; ------------------------------------------------------ ;;
(define (show:ep/list show)
  (let ((path (show:path show)))
    (cond
      ((not (file-exists? path))
       (throw 'invalid-path-exception
              (format #f "invalid path '~a': ~a" path (strerror ENOENT))))
      ((not (directory? path))
       (throw 'invalid-path-exception
              (format #f "invalid path '~a' ~a:" path (strerror ENOTDIR))))
      (else
       (scandir path
                (lambda (path)
                  (let loop ((format-list (config 'media-format-list)))
                    (cond
                      ((null? format-list)
                       #f)
                      ((string-suffix-ci? (car format-list) path)
                       #t)
                      (else
                       (loop (cdr format-list)))))))))))

;; ------------------------------------------------------ ;;
;; Check whether show is watchable.                       ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: bool - #t if show is watchable,         ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show:watchable? show)
  (and (<= 0 (show:ep/index show))
       (< (show:ep/index show) 
          (length (show:ep/list show)))))

;; ------------------------------------------------------ ;;
;; Check whether show is fisnished.                       ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: bool - #t if show is finished,          ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show:finished? show)
  (and (not (show:airing? show))
       (not (show:watchable? show))))

;; ------------------------------------------------------ ;;
;; Check whether episode index of a show is out of bounds ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: bool - #t if episode index is out of    ;;
;;           bounds, #f otherwise                         ;;
;; ------------------------------------------------------ ;;
(define (show:ep/index-out-of-bounds? show)
  (let ((ep/list-len (length (show:ep/list show)))
        (ep/index (show:ep/index show)))
        ;; Out of bounds if current episode < 0
    (or (> 0 ep/index)
        (call-if (show:airing? show)
          (> | >=) ep/index ep/list-len))))

;; ------------------------------------------------------ ;;
;; Remove a show from a show-list. Original list remains  ;;
;; unmodified.                                            ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name :: string - show name               ;;
;;                                                        ;;
;; #:param: show-list :: [show] - show list               ;;
;;                                                        ;; 
;; #:return: x :: [show] - show-list with requested show  ;;
;;           removed                                      ;;
;; ------------------------------------------------------ ;;
(define (remove-show show-name show-list)
  (cond 
    ((null? show-list)
     '())
    ((string=? show-name (show:name (car show-list)))
     (cdr show-list))
    (else
     (cons (car show-list)
           (remove-show show-name (cdr show-list))))))

;; ------------------------------------------------------ ;;
;; Find a show in a show-list.                            ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name :: string - show name               ;;
;;                                                        ;;
;; #:param: show-list :: [show] - show list               ;;
;;                                                        ;;
;; #:return: x :: show/bool - if such a show was found    ;;
;;           it is returned, otherwise #f is returned     ;;
;; ------------------------------------------------------ ;;
(define (find-show show-name show-list)
  (cond
    ((null? show-list)
     #f)
    ((string=? show-name (show:name (car show-list)))
     (car show-list))
    (else
     (find-show show-name (cdr show-list)))))
