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
  #:use-module (tvsm base config)
  #:use-module (tvsm util path))

;; ------------------------------------------------------ ;;
;; Read show database.                                    ;;
;; ------------------------------------------------------ ;;
;; #:return: x :: [show] - show-list (list is empty if    ;;
;;           the database is empty                        ;;
;; ------------------------------------------------------ ;;
(define (read-show-list-db)
  (let ((db-path (config 'show-db-path)))
    (if (access? db-path R_OK)
      (with-input-from-file db-path read)
      '())))

;; ------------------------------------------------------ ;;
;; Write show database.                                   ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list               ;; 
;; ------------------------------------------------------ ;;
(define (write-show-list-db show-list)
  (let* ((db-path (config 'show-db-path))
         (db-dir (dirname db-path)))
    (unless (file-exists? db-dir)
      (catch
        #t
        ;; thunk
        (lambda ()
          (mkdirs db-dir))
        ;; handler
        (lambda (key code path)
          (throw 'key (format #f "cannot create directory '~a': ~a" 
                              path
                              (strerror code))))))
    (with-output-to-file
      db-path
      (lambda ()
        (write show-list)))))
