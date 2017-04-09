;; watch - a tv show manager.
;; Copyright © 2017 Vasili Karaev
;;
;; This file is part of watch.
;;
;; watch is free software: you can redistribute  it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; watch is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHENTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with watch. If not, see <http://www.gnu.org/licenses/>.

(define-module (watch show)
  #:export     (call-with-show-list
                make-show
                remake-show
                show:name
                show:path
                show:current-episode
                show:episode-offset
                show:date
                show:current-episode-inc
                show:current-episode-dec
                show:episode-list
                show-over?
                show:current-episode-out-of-bounds?
                remove-show
                find-show
                ask-user-overwrite)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (watch db)
  #:use-module (watch config))

;; ------------------------------------------------------- ;;
;; Read show-list database and call '(proc show-list)'     ;;
;; with the read show-list.
;; ------------------------------------------------------- ;;
;; #:param: proc - a procedure that takes a show-list as   ;;
;;          its only argument. Must return a show-list if  ;;
;;          'overwrite' is #t                              ;;
;; #:param: overwrite - if #t show-list database will be   ;;
;;          overwritten with the result of 'proc'          ;;
;; ------------------------------------------------------- ;;
(define* (call-with-show-list #:key proc overwrite)
  (let* ((show-list-db  (read-show-list-db))
         (new-show-list (proc show-list-db)))
    (when (and overwrite new-show-list)
      (write-show-list-db new-show-list))))

;; ------------------------------------------------------ ;;
;; Create a show object.                                  ;;
;; ------------------------------------------------------ ;;
;; #:param: name - a string representing the name of      ;;
;;          a show                                        ;;
;;                                                        ;;
;; #:param: path - a string representing the path to      ;;
;;          a show                                        ;;
;;                                                        ;;
;; #:param: current-episode - an integer representing     ;; 
;;          the number of the current episode a show      ;;
;;                                                        ;;
;; #:param: episode-offset - an integer representing      ;;
;;          episode number offset                         ;;
;;          ---                                           ;;
;;          most shows would have episode-offset equal    ;;
;;          to 1 since their first episode is numbered    ;;
;;          as 'E01'                                      ;;
;;          but for example in case of Star Trek TOS S01  ;;
;;          which has a pilot episode numbered 'E00'      ;;
;;          episode-offset should be equal to 0           ;;
;;          ---                                           ;;
;; #:param: date - a string representing the date of      ;;
;;          show's creation                               ;;
;;                                                        ;;
;; #:return: a newly created show                         ;;
;; ------------------------------------------------------ ;;
(define* (make-show #:key name 
                          path 
                          date
                          current-episode 
                          episode-offset)
  (list (cons 'name name) 
        (cons 'path path) 
        (cons 'date date)
        (cons 'current-episode current-episode)
        (cons 'episode-offset episode-offset)))

;; ------------------------------------------------------ ;;
;; Remaake show using either new values if specified or   ;;
;; old ones from the show object otherwise.               ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show.                                ;;
;; ------------------------------------------------------ ;;
;; For information on other parameters take a look at     ;;
;; the specification of 'make-show'.                      ;;
;; ------------------------------------------------------ ;;
(define* (remake-show show #:key (name (show:name show))
                                 (path (show:path show))
                                 (date (show:date show))
                                 (current-episode (show:current-episode show))
                                 (episode-offset  (show:episode-offset show)))
  (list (cons 'name name) 
        (cons 'path path) 
        (cons 'date date)
        (cons 'current-episode current-episode)
        (cons 'episode-offset episode-offset)))

;; ------------------------------------------------------ ;;
;; Get name of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the name of the show   ;;
;; ------------------------------------------------------ ;;
(define (show:name show)
  (cdr (assoc 'name show)))

;; ------------------------------------------------------ ;;
;; Get path of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the path to the show   ;;
;; ------------------------------------------------------ ;;
(define (show:path show)
  (cdr (assoc 'path show)))

;; ------------------------------------------------------ ;;
;; Get date of show's creation.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing date of show's         ;;
;;           creation                                     ;;
;; ------------------------------------------------------ ;;
(define (show:date show)
  (cdr (assoc 'date show)))

;; ------------------------------------------------------ ;;
;; Get current episode of show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: an integer representing current episode of   ;;
;;           the show                                     ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode show)
  (cdr (assoc 'current-episode show)))

;; ------------------------------------------------------ ;;
;; Get episode offset of show.                            ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;; 
;; #:return: an integer representing episode number       ;;
;;           offset                                       ;;
;; ------------------------------------------------------ ;;
(define (show:episode-offset show)
  (cdr (assoc 'episode-offset show)))

;; ------------------------------------------------------ ;;
;; Return show with incremented current-episode index.    ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: the same show but with its current-episode   ;;
;;           index:                                       ;;
;;           IF show is already over then index remains   ;;
;;              'over                                     ;;
;;           IF show has already reached the final        ;;
;;              episode index becomes 'over               ;;
;;           OTHERWISE it is just incremented             ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-inc show)
  (remake-show show
               #:current-episode
                 (let ((current-episode (show:current-episode show)))
                   (cond 
                     ((show-over? show) 
                      'over)
                     ((= (1+ current-episode) (length (show:episode-list show)))
                      'over)
                     (else 
                      (1+ current-episode))))))

;; ------------------------------------------------------ ;;
;; Return show with decremented current-episode index.    ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: the same show but with its current-episode   ;;
;;           index:                                       ;;
;;           IF show is already over set it to index of   ;;
;;              the last episode of show                  ;;
;;           IF it is zero then it remains zero           ;;
;;           OTHERWISE it is just decremented             ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-dec show)
  (remake-show show
               #:current-episode
                 (let ((current-episode (show:current-episode show)))
                   (cond 
                     ((show-over? show) 
                      (1- (length (show:episode-list show)))) 
                     ((zero? current-episode) 
                      0)
                     (else 
                      (1- current-episode))))))

;; ------------------------------------------------------ ;;
;; Get a list of filenames of episodes of show.           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;; 
;;                                                        ;;
;; #:return: a list of strings representing filenames of  ;;
;;           episodes of show.                            ;;
;;           (essentially contents of directory located   ;;
;;           at (show:path show) filtered by their        ;; 
;;           extension)                                   ;;
;; ------------------------------------------------------ ;;
(define (show:episode-list show)
  (let ((episode-list 
          ;; If show-path is a symlink read it and pass to result to scandir
          ;; it is necessary because scandir does not work with symlinks.
          (scandir (if (eq? 'symlink (stat:type (lstat (show:path show)))) 
                     (readlink (show:path show))
                     (show:path show))
                   ;; This predicate checks whether filepath's suffix is
                   ;; present in the 'media-format-list'.
                   (lambda (filepath)
                     (let loop ((format-list (config 'media-format-list)))
                       (cond 
                         ((null? format-list) 
                          #f)
                         ((string-suffix-ci? (car format-list) filepath)
                          #t)
                         (else 
                          (loop (cdr format-list)))))))))
    (if (not episode-list)
      (throw 'directory-not-readable-exception
             (format #f "cannot stat '~a': No such directory" (show:path show)))
      episode-list)))

;; ------------------------------------------------------ ;;
;; Check whether show is over (final episode had been     ;;
;; played)                                                ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t if show is over                           ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show-over? show)
  (eq? 'over (show:current-episode show)))

;; ------------------------------------------------------ ;;
;; Check whether current episode index of show is out     ;;
;; of bounds.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t - IF current episode index < 0            ;;
;;                OR index >= length of episode list      ;;
;;           #f - otherwise                               ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-out-of-bounds? show)
  (and (not (show-over? show))
       (let ((episode-list (show:episode-list show)))
         (or (<= (length episode-list) (show:current-episode show))
             (> 0 (show:current-episode show))))))

;; ------------------------------------------------------ ;;
;; Remove show named show-name from show-list. Original   ;;
;; list remains unmodified.                               ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show that is being removed                ;;
;;                                                        ;;
;; #:param: show-list - a show-list                       ;;
;;                                                        ;; 
;; #:return: show-list without the show named show-name   ;;
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
;; Find show named show-name in show-list.                ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show that we're looking for               ;;
;;                                                        ;;
;; #:param: show-list - a show list to search             ;;
;;                                                        ;;
;; #:return: in case the show was found - the show itself ;;
;;           otherwise - #f                               ;;
;; ------------------------------------------------------ ;;
(define (find-show show-name show-list)
  (cond
    ((null? show-list)
     #f)
    ((string=? show-name (show:name (car show-list)))
     (car show-list))
    (else
     (find-show show-name (cdr show-list)))))

;; -------------------------------------------------------------------- ;;
;; Ask the user whether they'd like to overwrite already existing show. ;;
;; -------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show      ;;
;; -------------------------------------------------------------------- ;;
(define (ask-user-overwrite show-name)
  (let loop ((ask-message (format #f 
                                  "A show called '~a' already exists, overwrite? (y/n): " 
                                  show-name)))
    (display ask-message)
    (let ((answer (read-line)))
      (cond
        ((eof-object? answer) 
         #f)
        ((or (string-ci=? answer "y") (string-ci=? answer "yes"))
         #t)
        ((or (string-ci=? answer "n") (string-ci=? answer "no")) 
         #f)
        ;; If the answer is neither 'yes' or 'y' nor 'no' or 'n'
        ;; loop until the requested answer is received.
        (else 
         (loop "Please answer (y/n): "))))))
