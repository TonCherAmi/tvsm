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

(define-module (tvsm cmd add)
  #:export     (add-show-db)
  #:use-module (srfi srfi-19)
  #:use-module (tvsm base show)
  #:use-module (tvsm util ask-user))

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
;; #:param: ep/current :: int - current episode number                ;;
;;    NOTE: it is assumed that this value is passed without           ;;
;;          ep/offset subtracted i.e. as it was specified by the user ;;
;;                                                                    ;;
;; #:param: ep/offset :: int - episode number offset. generally       ;;
;;          only useful for shows whose first episode is numbered     ;;
;;          differently than 'E01'                                    ;;
;; ------------------------------------------------------------------ ;;
(define* (add-show-db #:key name path airing? ep/current ep/offset)
  (let ((new-show (make-show #:name name 
                             #:path path
                             #:date (date->string (current-date) "~b ~e ~Y")
                             #:airing? airing?
                             #:ep/current ep/current
                             #:ep/offset ep/offset)))
    (if (show:ep/index-out-of-bounds? new-show)
        (throw 'episode-out-of-bounds-exception 
               (format #f "cannot add '~a': Current episode is out of bounds" name))
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
