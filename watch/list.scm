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

(define-module (watch list)
  #:export     (list-shows-db)
  #:use-module (watch show-utils))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: verbose - make output more detailed                ;;
;; ----------------------------------------------------------- ;;
(define* (list-shows-db #:optional (verbose #f))
  (let ((show-list-db (read-show-list-db)))
    (if verbose 
      (begin
        (format #t "total: ~a~%" (length show-list-db))
        (let loop ((show-list show-list-db))
          (unless (null? show-list)
            (print-show (car show-list) #t) (newline)
            (loop (cdr show-list)))))
      (unless (null? show-list-db)
        (let* ((columns 
                 (string->number (getenv "COLUMNS")))
               (max-name-length
                 ;; Get length of the longest show name in the show-list-db.
                 (apply max (map (lambda (x) (string-length (show:name x))) show-list-db)))
               (shows-per-line (quotient columns (1+ max-name-length))))
          (let loop ((count shows-per-line)
                     (show-list show-list-db))
            (unless (null? show-list)
              (cond
                ((zero? count) 
                 (newline)
                 (loop shows-per-line show-list))
                (else 
                  (print-show (car show-list)) (display "\t")
                  (loop (1- count) (cdr show-list)))))))))))

;; ------------------------------------------------------ ;;
;; Print show contents to (current-output-port)           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show to print                        ;;
;; #:param: verbose - make output more detailed           ;;
;; ------------------------------------------------------ ;;
(define* (print-show show #:optional (verbose #f))
  (call-with-values 
    (lambda () 
      (let ((format-string (string-append "~a" 
                                          (if verbose "\t~a\t~a" ""))))
        (if verbose
          (values 
             #t
             format-string
            (show:name show)
            (if (number? (show:current-episode show))
                  (1+ (show:current-episode show))
                  (show:current-episode show))
            (show:path show))
          (values
             #t
             format-string
            (show:name show)))))
     format))
