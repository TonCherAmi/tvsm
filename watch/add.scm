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

(define-module  (watch add)
  #:export      (add-show-db)
  #:use-module  (srfi srfi-19)
  #:use-module  (watch show))

;; ------------------------------------------------------------------ ;;
;; Add show to the show database.                                     ;;
;; ------------------------------------------------------------------ ;;
;; #:param: name - a string representing the name of the show that    ;;
;;          is being added.                                           ;;
;;                                                                    ;;
;; #:param: path - a string representing a path to the show directory ;; 
;;                                                                    ;;
;; #:param: starting-episode - an integer that is the index of the    ;;
;;          episode from which the show will begin to play            ;; 
;;    NOTE: it is assumed that the value is passed without            ;;
;;          episode-offset subtracted i.e. as it was specified by the ;;
;;          user                                                      ;;
;;                                                                    ;;
;; #:param: episode-offset - an integer representing episode number   ;;
;;          offset. useful for shows whose first episode is numbered  ;;
;;          differently than 'E01'                                    ;;
;; ------------------------------------------------------------------ ;;
(define* (add-show-db #:key name path airing? starting-episode episode-offset)
  (let ((new-show (make-show #:name name 
                             #:path path
                             #:date (date->string (current-date) "~b ~e ~Y")
                             #:airing? airing?
                             #:current-episode (- starting-episode episode-offset)
                             #:episode-offset  episode-offset)))
    (if (show:current-episode-out-of-bounds? new-show)
        ;; Throw an exception if starting episode index is out of bounds.
        (throw 'episode-out-of-bounds-exception 
               (format #f "cannot add '~a': Starting episode index is out of bounds" 
                       name))
        (call-with-show-list
          #:overwrite
            #t
          #:proc
            (lambda (show-list)
              (cond
                ((not (find-show name show-list))
                 (cons new-show show-list))
                ;; If show with such a name already exists we ask user
                ;; whether they would like to overwrite it, if the answer 
                ;; is positive we overwrite it, otherwise we throw an
                ;; exception.
                ((ask-user-overwrite name)
                 (cons new-show (remove-show name show-list)))
                (else
                 (throw 'show-already-exists-exception
                        (format #f "cannot add '~a': Show already exists" name)))))))))
