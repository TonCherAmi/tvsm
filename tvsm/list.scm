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

(define-module (tvsm list)
  #:export     (list-shows-db)
  #:use-module (tvsm show)
  #:use-module (tvsm util)
  #:use-module (tvsm config))

;; ------------------------------------------------------ ;;
;; Print contents of show-list database in a neat manner. ;;
;; ------------------------------------------------------ ;;
;; #:param: long-format - make output more detailed       ;;
;; ------------------------------------------------------ ;;
(define* (list-shows-db #:key all long)
  (call-with-show-list
    #:overwrite
      #f
    #:proc
      (lambda (show-list)
        (let ((show-list (if (not all)
                           (filter (lambda (x) (not (show-finished? x))) show-list)
                           show-list)))
          ((if long list-shows-long list-shows-short) show-list)))))

;; ------------------------------------------------------ ;;
;; Print show-list in long format.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - show-list to print                ;;
;; ------------------------------------------------------ ;;
(define (list-shows-long show-list)
  (format #t "total ~a~%" (length show-list))
  (let loop ((lst show-list))
    (unless (null? lst)
      (let* ((show (car lst))
             (finished? (show-finished? show)))
        (format #t "~a  [~a~a] ~5@a  ~a~%"
                (show:date show)
                ;; 'f' for finished, 'w' for watching
                (if finished? #\f #\w)
                ;; 'a' for airing, 'c' for completed
                (if (show:airing? show) #\a #\c)
                (format #f "~a/~a"
                        (if finished? #\- (+ (show:current-episode show)
                                             (show:episode-offset show)))
                        (+ (length (show:episode-list show))
                           (if (zero? (show:episode-offset show))
                             0
                             (1- (show:episode-offset show)))))
                (show:name show)))
      (loop (cdr lst)))))

;; ------------------------------------------------------ ;;
;; Print show-list in short format (names only).          ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - show-list to print                ;;
;; ------------------------------------------------------ ;;
(define (list-shows-short show-list)
  (call-with-output-pipe
    "column -t"
    (lambda (port)
      (let ((output-string
              (let loop ((lst show-list)
                         (count (config 'columns)))
                (cond 
                  ((null? lst)
                   "")
                  ((>= 0 (- count (+ 2 (string-length (show:name (car lst))))))
                   (++ "\n" (loop lst (config 'columns))))
                  (else
                   (++ (show:name (car lst))
                       "  " 
                       (loop (cdr lst) 
                             (- count (string-length (show:name (car lst)))))))))))
        (display output-string port)
        (newline port)))))
