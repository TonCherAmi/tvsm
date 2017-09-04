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

(define-module (tvsm show)
  #:export     (call-with-show-list
                make-show
                remake-show
                show:name
                show:path
                show:current-episode
                show:episode-offset
                show:date
                show:airing?
                show:current-episode-inc
                show:current-episode-dec
                show:episode-list
                show-playable?
                show-finished?
                show:current-episode-out-of-bounds?
                remove-show
                find-show)
  #:use-module (ice-9 ftw)
  #:use-module (tvsm db)
  #:use-module (tvsm config))

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
;; #:param: current-episode :: int - current episode      ;;
;;          number                                        ;;
;;                                                        ;;
;; #:param: date :: string - show creation date in form   ;;
;;          'MMM DD YYYY' e.g. 'Jan 01 1970'              ;;
;;                                                        ;;
;; #:param: airing? :: bool - if #t show is not marked as ;;
;;          finished when all of its episodes have been   ;;
;;          watched                                       ;;
;;                                                        ;;
;; #:param: episode-offset :: int - episode number offset ;;
;;          ---                                           ;;
;;          most shows should have episode-offset equal   ;;
;;          to 1 since their first episode is numbered    ;;
;;          as 'E01',                                     ;;
;;          but for example in case of Star Trek TOS S01  ;;
;;          which has a pilot episode numbered 'E00'      ;;
;;          episode-offset should be equal to 0           ;;
;;          ---                                           ;;
;;                                                        ;;
;; #:param: subtract-offset? :: bool - if #t              ;;
;;          'episode-offset is subtracted from            ;;
;;          'current-episode'                             ;;
;;                                                        ;;
;; #:return: x :: show - show                             ;;
;; ------------------------------------------------------ ;;
(define* (make-show #:key name 
                          path 
                          date
                          airing?
                          current-episode 
                          episode-offset
                          subtract-offset?)
  (list (cons 'name name) 
        (cons 'path path) 
        (cons 'date date)
        (cons 'airing? airing?)
        (cons 'current-episode (if subtract-offset? 
                                 (- current-episode episode-offset)
                                 current-episode))
        (cons 'episode-offset episode-offset)))

;; ------------------------------------------------------ ;;
;; Remake a show using either newly specified values or   ;;
;; old ones from the 'show' parameter.                    ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;; ------------------------------------------------------ ;;
;; For information on other parameters take a look at     ;;
;; the specification of 'make-show'.                      ;;
;; ------------------------------------------------------ ;;
(define* (remake-show show #:key (name    (show:name show))
                                 (path    (show:path show))
                                 (date    (show:date show))
                                 (airing? (show:airing? show))
                                 (current-episode (show:current-episode show))
                                 (episode-offset  (show:episode-offset show))
                                 subtract-offset?)
  (make-show #:name name 
             #:path path
             #:date date
             #:airing? airing?
             #:current-episode current-episode
             #:episode-offset episode-offset
             #:subtract-offset? subtract-offset?))

;; ------------------------------------------------------ ;;
;; Get name of a show.                                    ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: string - show name                      ;;
;; ------------------------------------------------------ ;;
(define (show:name show)
  (cdr (assoc 'name show)))

;; ------------------------------------------------------ ;;
;; Get path of a show.                                    ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: string - show path                      ;;
;; ------------------------------------------------------ ;;
(define (show:path show)
  (cdr (assoc 'path show)))

;; ------------------------------------------------------ ;;
;; Get creation date of a show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: string - show creation date             ;;
;; ------------------------------------------------------ ;;
(define (show:date show)
  (cdr (assoc 'date show)))

;; ------------------------------------------------------ ;;
;; Check whether show is airing.                          ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;; 
;; #:return: x :: bool - #t if show is airing,            ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show:airing? show)
  (cdr (assoc 'airing? show)))

;; ------------------------------------------------------ ;;
;; Get current episode number of a show.                  ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:param: with-offset :: bool - if #t episode offset    ;;
;;          is added to current episode number            ;;
;;                                                        ;;
;; #:return: x :: bool - current episode number           ;;
;; ------------------------------------------------------ ;;
(define* (show:current-episode show #:key with-offset)
  (+ (cdr (assoc 'current-episode show))
     (if with-offset 
       (show:episode-offset show)
       0)))

;; ------------------------------------------------------ ;;
;; Get episode offset of a show.                          ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;; 
;; #:return: x :: int - show episode offset               ;;
;; ------------------------------------------------------ ;;
(define (show:episode-offset show)
  (cdr (assoc 'episode-offset show)))

;; ------------------------------------------------------ ;;
;; Increment current episode number of a show.            ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: show - the same show with incremented   ;;
;;           current episode number                       ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-inc show)
  (remake-show show
               #:current-episode
                 (1+ (show:current-episode show))))

;; ------------------------------------------------------ ;;
;; Decrement current episode number of a show.            ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: show - the same show with decremented   ;;
;;           current episode number if current episode    ;;
;;           is > 0, otherwise the same show with         ;;
;;           no changes                                   ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-dec show)
  (if (>= 0 (show:current-episode show))
    show
    (remake-show show
                 #:current-episode
                   (1- (show:current-episode show)))))

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
(define (show:episode-list show)
  (let ((path (show:path show)))
    (cond
      ((not (file-exists? path))
       (throw 'invalid-path-exception
              (format #f "invalid path '~a': No such file or directory" path)))
      ((not (eq? 'directory (stat:type (stat path))))
       (throw 'invalid-path-exception
              (format #f "invalid path '~a': Not a directory" path)))
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
;; Check whether show is playable.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: bool - #t if show is playable,          ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show-playable? show)
  (and (<= 0 (show:current-episode show))
       (< (show:current-episode show) 
          (length (show:episode-list show)))))

;; ------------------------------------------------------ ;;
;; Check whether show is fisnished.                       ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: x :: bool - #t if show is finished,          ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show-finished? show)
  (and (not (show:airing? show))
       (not (show-playable? show))))

;; ------------------------------------------------------ ;;
;; Check whether current episode index of show is out     ;;
;; of bounds.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show :: show - show                           ;;
;;                                                        ;;
;; #:return: #t if current episode is out of bounds,      ;;
;;           #f otherwise                                 ;;
;; #:return: x :: bool - #t if current episode number is  ;;
;;           out of bounds, #f otherwise                  ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-out-of-bounds? show)
  (let ((ep-lst-len (length (show:episode-list show)))
        (current-ep (show:current-episode show)))
        ;; Out of bounds if current episode < 0
    (or (> 0 current-ep)
        ((if (show:airing? show) > >=) current-ep ep-lst-len))))

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
