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

(define-module (tvsm add)
  #:export     (add-show-db)
  #:use-module (srfi srfi-19)
  #:use-module (tvsm util)
  #:use-module (tvsm show))

;; ------------------------------------------------------------------ ;;
;; Add a show to the database.                                        ;;
;; ------------------------------------------------------------------ ;;
;; #:param: name :: string - show name                                ;;
;;                                                                    ;;
;; #:param: path :: string - path to the show directory               ;;
;;                                                                    ;;
;; #:param: airing? :: bool - if #t show is not marked as finished    ;;
;;          when all of its episodes have been watched                ;;
;;                                                                    ;;
;; #:param: starting-episode :: int - number of the episode to be     ;;
;;          played first                                              ;;
;;    NOTE: it is assumed that this value is passed without           ;;
;;          episode-offset subtracted i.e. as it was specified by     ;;
;;          the user                                                  ;;
;;                                                                    ;;
;; #:param: episode-offset :: int - episode number offset. generally  ;;
;;          only useful for shows whose first episode is numbered     ;;
;;          differently than 'E01'                                    ;;
;; ------------------------------------------------------------------ ;;
(define* (add-show-db #:key name path airing? starting-episode episode-offset)
  (let ((new-show (make-show #:name name 
                             #:path path
                             #:date (date->string (current-date) "~b ~e ~Y")
                             #:airing? airing?
                             #:current-episode starting-episode
                             #:episode-offset episode-offset
                             #:subtract-offset? #t)))
    (if (show:current-episode-out-of-bounds? new-show)
        (throw 'episode-out-of-bounds-exception 
               (format #f "cannot add '~a': Starting episode index is out of bounds" name))
        (call-with-show-list
          #:overwrite
            #t
          #:proc
            (lambda (show-list)
              (cond
                ((not (find-show name show-list))
                 (cons new-show show-list))
                ((ask-user-y/n (format #f "Show '~a' already exists. Overwrite? " name))
                 (cons new-show (remove-show name show-list)))
                (else
                 (throw 'show-already-exists-exception
                        (format #f "not overwriting '~a': Exiting" name)))))))))
