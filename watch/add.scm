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

(define-module  (watch add)
  #:export      (add-show-db)
  #:use-module  (watch show-utils)
  #:use-module  (ice-9 rdelim))

;; ---------------------------------------------------------------------- ;;
;; Add show to the show database.                                         ;;
;; ---------------------------------------------------------------------- ;;
;; #:param: show-name - a string representing the name of the show that   ;;
;;          is being added.                                               ;;
;;          Serves as a unique identifier, there cannot  be two shows     ;;
;;          with the same name in the db.                                 ;;
;;                                                                        ;; 
;; #:param: show-path - a string that is a path to the directory that     ;; 
;;          contains the show                                             ;;
;;                                                                        ;;
;; #:param: starting-episode - an integer that is the index of the        ;;
;;          episode from which the show will begin to play                ;; 
;; ---------------------------------------------------------------------- ;;
(define* (add-show-db show-name show-path #:optional (starting-episode 0))
  (let ((new-show (make-show show-name show-path starting-episode)))
    (if (show:current-episode-out-of-bounds? new-show)
        ;; Throw an exception if starting episode index is out of bounds.
        (throw 'episode-out-of-bounds-exception 
               (format #f "cannot add '~a': Starting episode index is out of bounds" show-name))
        (call-with-show-list
          #:overwrite
            #t
          #:proc
            (lambda (show-list)
              (cond
                ((not (find-show show-name show-list))
                 (cons new-show show-list))
                ;; If show with such a name already exists we ask user
                ;; whether they would like to overwrite it, if the answer 
                ;; is positive we overwrite it, otherwise we throw an
                ;; exception.
                ((ask-user-overwrite show-name)
                 (cons new-show (remove-show show-name show-list)))
                (else
                  (throw 'show-already-exists-exception
                         (format #f "cannot add '~a': Show already exists" show-name)))))))))
