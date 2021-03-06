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

(define-module (tvsm cmd set)
  #:export     (set-show-name-db
                set-show-path-db
                set-show-airing-db
                set-show-ep/current-db)
  #:use-module (tvsm base show)
  #:use-module (tvsm util ask-user))

;; ------------------------------------------------------ ;;
;; Rename a show in the database.                         ;;
;; ------------------------------------------------------ ;;
;; #:param: old-show-name :: string - old show name       ;;
;;                                                        ;;
;; #:param: new-show-name :: string - new show name       ;;
;; ------------------------------------------------------ ;;
(define (set-show-name-db old-show-name new-show-name)
  (call-with-show-list
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (let* ((show*
                 (find-show old-show-name show-list))
               (show
                 (if (not show*)
                   (throw 'show-not-found-exception
                          (format #f "cannot rename '~a': No such show" old-show-name))
                   (remake-show show* #:name new-show-name))))
          (if (find-show new-show-name show-list)
            (if (ask-user-y/n (format #f "Show '~a' already exists. Overwrite? " new-show-name))
              (cons show (remove-show new-show-name (remove-show old-show-name show-list)))
              (throw 'show-already-exists-exception
                     (format #f "cannot rename '~a' to '~a': Show already exists"
                             old-show-name new-show-name)))
            (cons show (remove-show old-show-name show-list)))))))

;; ------------------------------------------------------ ;;
;; Set new path for a show in the database.               ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name :: string - show name               ;;
;;                                                        ;;
;; #:param: new-show-path :: string - new path to show    ;;
;;          directory                                     ;;
;; ------------------------------------------------------ ;;
(define (set-show-path-db show-name new-show-path)
  (call-with-show-list
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (let* ((show*
                 (find-show show-name show-list))
               (show 
                 (if (not show*)
                   (throw 'show-not-found-exception
                          (format #f "cannot set path for '~a': No such show" show-name))
                   (remake-show show* #:path new-show-path))))
          (if (show:ep/index-out-of-bounds? show)
            (throw 'episode-out-of-bounds-exception
                   (format #f "cannot set path for '~a': Episode out of bounds" show-name))
            (cons show (remove-show show-name show-list)))))))

;; ------------------------------------------------------ ;;
;; Mark/unmark a show in the database as airing.          ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name :: string - show name               ;;
;;                                                        ;;
;; #:param: airing? :: bool - determines whether show is  ;;
;;          airing                                        ;;
;; ------------------------------------------------------ ;;
(define (set-show-airing-db show-name airing?)
  (call-with-show-list
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (let* ((show*
                 (find-show show-name show-list))
               (show
                 (if (not show*)
                   (throw 'show-not-found-exception
                          (format #f "cannot mark '~a' as ~a: No such show" 
                                  show-name
                                  (if airing? "airing" "completed")))
                   (remake-show show* #:airing? airing?))))
          (cons show (remove-show show-name show-list))))))

;; ------------------------------------------------------ ;;
;; Set new current episode for a show in the database.    ;;
;; ------------------------------------------------------ ;;
;; #:param: show-name :: string - show name               ;;
;;                                                        ;;
;; #:param: new-ep/current :: int - new current episode   ;;
;; ------------------------------------------------------ ;;
(define (set-show-ep/current-db show-name new-ep/current)
  (call-with-show-list
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (let* ((show*
                 (find-show show-name show-list))
               (show
                 (if (not show*)
                   (throw 'show-not-found-exception
                          (format #f "cannot set current episode for '~a': No such show" 
                                  show-name))
                   (remake-show show* #:ep/current new-ep/current))))
          (if (show:ep/index-out-of-bounds? show)
            (throw 'episode-out-of-bounds-exception
                   (format #f "cannot set current episode for '~a': Episode out of bounds" 
                           show-name))
            (cons show (remove-show show-name show-list)))))))
