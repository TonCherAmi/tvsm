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
                find-show
                ask-user-overwrite)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (tvsm db)
  #:use-module (tvsm config))

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
                          airing?
                          current-episode 
                          episode-offset)
  (list (cons 'name name) 
        (cons 'path path) 
        (cons 'date date)
        (cons 'airing? airing?)
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
(define* (remake-show show #:key (name    (show:name show))
                                 (path    (show:path show))
                                 (date    (show:date show))
                                 (airing? (show:airing? show))
                                 (current-episode (show:current-episode show))
                                 (episode-offset  (show:episode-offset show)))
  (list (cons 'name name) 
        (cons 'path path) 
        (cons 'date date)
        (cons 'airing? airing?)
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
;; Check whether show is airing.                          ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;; 
;; #:return: #t if show is airing, #f otherwise           ;;
;; ------------------------------------------------------ ;;
(define (show:airing? show)
  (cdr (assoc 'airing? show)))

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
;; Get show with incremented current episode.             ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: show with incremented current episode        ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-inc show)
  (remake-show show
               #:current-episode
                 (1+ (show:current-episode show))))

;; ------------------------------------------------------ ;;
;; Get show with decremented current episode.             ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: show with decremented current epsidoe        ;;
;;           if current episode > 0, otherwise unmodified ;;
;;           show                                         ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-dec show)
  (if (>= 0 (show:current-episode show))
    show
    (remake-show show
                 #:current-episode
                   (1- (show:current-episode show)))))

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
;; Check whether show is playable.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t if show is playable, #f otherwise         ;;
;; ------------------------------------------------------ ;;
(define (show-playable? show)
  (and (<= 0 (show:current-episode show))
       (< (show:current-episode show) 
          (length (show:episode-list show)))))

;; ------------------------------------------------------ ;;
;; Check whether show is fisnished.                       ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t if show is finished, #f otherwise         ;;
;; ------------------------------------------------------ ;;
(define (show-finished? show)
  (and (not (show:airing? show))
       (not (show-playable? show))))

;; ------------------------------------------------------ ;;
;; Check whether current episode index of show is out     ;;
;; of bounds.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: #t if current episode is out of bounds,      ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode-out-of-bounds? show)
  (let ((ep-lst-len (length (show:episode-list show)))
        (current-ep (show:current-episode show)))
        ;; Out of bounds if current episode < 0
    (or (> 0 current-ep)
        ((if (show:airing? show) > >=) current-ep ep-lst-len))))

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
