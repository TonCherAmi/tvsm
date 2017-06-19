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

(define-module (tvsm remove)
  #:export     (remove-show-db
                remove-finished-db)
  #:use-module (tvsm show))

;; ---------------------------------------------------- ;;
;; Remove show called show-name from the show database. ;;
;; ---------------------------------------------------- ;;
;; #:param: show-name - name of the show to remove      ;;
;; ---------------------------------------------------- ;;
(define (remove-show-db show-name)
  (call-with-show-list
    #:overwrite 
      #t
    #:proc
      (lambda (show-list)
        (if (not (find-show show-name show-list))
          (throw 'show-not-found-exception
                 (format #f "cannot remove '~a': No such show" show-name))
          (remove-show show-name show-list)))))

;; ---------------------------------------------------- ;;
;; Remove completed shows from the database.            ;;
;; ---------------------------------------------------- ;;
(define (remove-finished-db)
  (call-with-show-list
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (filter (lambda (show) (show-finished? show))
                show-list))))
