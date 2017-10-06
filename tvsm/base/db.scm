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

(define-module (tvsm base db)
  #:export     (read-show-list-db
                write-show-list-db)
  #:use-module (tvsm base config))

;; ------------------------------------------------------ ;;
;; Read show database.                                    ;;
;; ------------------------------------------------------ ;;
;; #:return: x :: [show] - show-list (list is empty if    ;;
;;           the database is empty                        ;;
;; ------------------------------------------------------ ;;
(define (read-show-list-db)
  (if (access? (config 'show-db-path) R_OK)
    (with-input-from-file 
      (config 'show-db-path)
      read)
    (list)))

;; ------------------------------------------------------ ;;
;; Write show database.                                   ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list               ;; 
;; ------------------------------------------------------ ;;
(define (write-show-list-db show-list)
  (let* ((db-path 
           (config 'show-db-path))
         (db-dir-path 
           (substring db-path 0 (string-index-right db-path #\/))))
    (when (not (file-exists? db-dir-path))
      (mkdir db-dir-path))
    (with-output-to-file
      db-path
      (lambda ()
        (write show-list)))))
