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
;; You should have received a copy of the GNU General Public License
;; along with watch. If not, see <http://www.gnu.org/licenses/>.

(define-module  (watch show-utils)
  #:export      (read-show-list-db
                 write-show-list-db
                 remove-show
                 make-show
                 find-show
                 show:name
                 show:path
                 show:current-episode
                 show:current-episode-inc
                 show:current-episode-dec
                 show:episode-list
                 show-over?
                 show:current-episode-out-of-bounds?
                 ask-user-overwrite)
  #:use-module  (ice-9 ftw)
  #:use-module ((watch config)
                  #:prefix config:))

;; ------------------------------------------------------ ;;
;; Read show-list database.                               ;;
;; ------------------------------------------------------ ;;
;; #:return: show list (list is empty if the database is  ;;
;;           empty)                                       ;;
;; ------------------------------------------------------ ;;
(define (read-show-list-db)
  (if (access? config:show-database-path R_OK)
    (with-input-from-file 
      config:show-database-path
      read)
    (list)))

;; ------------------------------------------------------ ;;
;; Write show-list database.                              ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - a show list                       ;; 
;; ------------------------------------------------------ ;;
(define (write-show-list-db show-list)
  (if (access? config:resources-directory W_OK)
    (with-output-to-file
      config:show-database-path
      (lambda ()
        (write show-list)))
    (throw 'insufficient-permissions-exception
           "Insufficient permissions. Can't write to database.")))

;; ------------------------------------------------------ ;;
;; Remove show named show-name from show-list. Original   ;;
;; list remains unmodified.                               ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show that is being removed                ;;
;; #:param: show-list - a show-list                       ;;
;;                                                        ;; 
;; #:return: show-list without the show named show-name   ;;
;; ------------------------------------------------------ ;;
(define (remove-show show-name show-list)
  (assoc-remove! show-list show-name))

;; ------------------------------------------------------ ;;
;; Create a show named show-name, located at show-path    ;;
;; that will start playing from starting-episode.         ;;
;; A show is a list of three elements:                    ;; 
;;  - First one is this show's name.                      ;;
;;  - Second one is this show's path.                     ;;
;;  - Third one is this show's current episode.           ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the new show                                  ;;
;; #:param: show-path - a string representing the path to ;;
;;          the new show                                  ;;
;; #:param: starting-episode - an integer representing    ;; 
;;          the number of the episode from which the new  ;;
;;          will begin to play                            ;;
;;                                                        ;;
;; #:return: a newly created show                         ;;
;; ------------------------------------------------------ ;;
(define (make-show show-name show-path starting-episode)
  (list show-name show-path starting-episode))

;; ------------------------------------------------------ ;;
;; Find show named show-name in show-list.                ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show that we're looking for               ;;
;; #:param: show-list - a show list to search             ;;
;;                                                        ;;
;; #:return: in case the show was found - the show itself ;;
;;           otherwise - #f                               ;;
;; ------------------------------------------------------ ;;
(define (find-show show-name show-list)
  (assoc show-name show-list))


;; ------------------------------------------------------ ;;
;; Get name of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the name of the show   ;;
;; ------------------------------------------------------ ;;
(define (show:name show)
  (car show))

;; ------------------------------------------------------ ;;
;; Get path of show.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: a string representing the path to the show   ;;
;; ------------------------------------------------------ ;;
(define (show:path show)
  (cadr show))

;; ------------------------------------------------------ ;;
;; Get current episode of show.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show                                 ;;
;;                                                        ;;
;; #:return: an integer representing current episode of   ;;
;;           the show                                     ;;
;; ------------------------------------------------------ ;;
(define (show:current-episode show)
  (caddr show))

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
  (make-show (show:name show)
             (show:path show) 
             (let ((current-episode (show:current-episode show)))
               (cond 
                 ((show-over? show) 'over)
                 ((= (1+ current-episode) (length (show:episode-list show))) 'over)
                 (else (1+ current-episode))))))

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
  (make-show (show:name show)
             (show:path show)
             (let ((current-episode (show:current-episode show)))
               (cond ((show-over? show) (1- (length (show:episode-list show)))) ((zero? current-episode) 0)
                 (else (1- current-episode))))))

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
                   (lambda (filepath)
                     (let loop ((format-list config:episode-format-list))
                       (cond 
                         ((null? format-list) #f)
                         ((string-suffix-ci? (car format-list) filepath))
                         (else (loop (cdr format-list)))))))))
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
  (let ((episode-list (show:episode-list show)))
    (or (<= (length episode-list) (show:current-episode show))
        (> 0 (show:current-episode show)))))

;; -------------------------------------------------------------------- ;;
;; Ask the user whether they'd like to overwrite already existing show. ;;
;; -------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show  ;;
;; -------------------------------------------------------------------- ;;
(define (ask-user-overwrite show-name)
  (let loop ((ask-message (format #f 
                                  "A show called '~a' already exists, overwrite? (y/n): " 
                                  show-name)))
    (display ask-message)
    (let ((answer (read-line)))
      (cond
        ((eof-object? answer) #f)
        ((or (string-ci=? answer "y") (string-ci=? answer "yes")) #t)
        ((or (string-ci=? answer "n") (string-ci=? answer "no"))  #f)
        ;; If the answer is neither 'yes' or 'y' nor 'no' or 'n'
        ;; loop until the requested answer is received.
        (else (loop "Please answer (y/n): "))))))
