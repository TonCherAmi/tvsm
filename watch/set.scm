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

(define-module (watch set)
  #:export     (set-show-name-db
                set-show-path-db
                set-show-current-episode-db
                jump-to-next-episode-db
                jump-to-previous-episode-db)
  #:use-module (watch show-utils))


;; ------------------------------------------------------ ;;
;; Rename a show in the db.                               ;;
;; ------------------------------------------------------ ;;
;; #:param: old-show-name - a string representing         ;; 
;;          the name of the show that is being renamed    ;;
;; #:param: new-show-name - a string representing the new ;;
;;          name that the show will bear                  ;;
;; ------------------------------------------------------ ;;
(define (set-show-name-db old-show-name new-show-name)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show
                   (let ((show-db (find-show old-show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot rename '~a': No such show" old-show-name))
                       (make-show new-show-name 
                                  (show:path show-db)
                                  (show:current-episode show-db))))))
            ;; If show called 'new-show-name' already exists in the db
            (if (find-show new-show-name show-list-db)
              ;; We ask the user whether they'd like to overwrite already existing show
              (if (ask-user-overwrite new-show-name)
                ;; If the answer is positive we obviously overwrite it
                (cons new-show (remove-show new-show-name (remove-show old-show-name show-list-db)))
                ;; If the answer is negative we cannot proceed and therefore an exception is thrown
                (throw 'show-already-exists-exception
                       (format #f "cannot rename '~a' to '~a': Show already exists"
                               old-show-name new-show-name)))
              (cons new-show (remove-show old-show-name show-list-db))))))
    (write-show-list-db new-show-list)))

;; ------------------------------------------------------ ;;
;; Set a new path for a show.                             ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show whose path is being set              ;;
;; #:param: new-show-path - a string representing the new ;;
;;          path to the show directory                    ;;
;; ------------------------------------------------------ ;;
(define (set-show-path-db show-name new-show-path)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show
                   (let ((show-db (find-show show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot set path for '~a': No such show" show-name))
                       (make-show (show:name show-db)
                                  new-show-path
                                  (show:current-episode show-db))))))
            (if (show:current-episode-out-of-bounds? new-show)
              (throw 'episode-out-of-bounds-exception
                     (format #f "cannot set path for '~a': Episode out of bounds" show-name))
              (cons new-show (remove-show show-name show-list-db))))))
    (write-show-list-db new-show-list)))

;; ------------------------------------------------------ ;;
;; Set current episode of show called show-name in the db ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of ;;
;;          the show whose index is being modified        ;;
;; #:param: new-index - an integer representing the new   ;;
;;          index that will be set                        ;;
;; ------------------------------------------------------ ;;
(define (set-show-current-episode-db show-name new-index)
  (let ((new-show-list
          (let* ((show-list-db (read-show-list-db))
                 (new-show 
                   (let ((show-db (find-show show-name show-list-db)))
                     (if (not show-db)
                       (throw 'show-not-found-exception
                              (format #f "cannot set current episode for '~a': No such show" show-name))
                       (make-show (show:name show-db)
                                  (show:path show-db)
                                  new-index)))))
            (if (show:current-episode-out-of-bounds? new-show)
              (throw 'episode-out-of-bounds-exception
                     (format #f "cannot set current episode for '~a': Episode out of bounds" show-name))
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
                              (format #f "cannot jump to next episode of '~a': No Such show" show-name))
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
                              (format #f "cannot jump to previous episode of '~a': No Such show" show-name))
                       (show:current-episode-dec show-db)))))
            (cons new-show (remove-show show-name show-list-db)))))
    (write-show-list-db new-show-list)))
