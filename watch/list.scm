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

(define-module (watch list)
  #:export     (list-shows-db)
  #:use-module (ice-9 popen)
  #:use-module (watch show)
  #:use-module (watch util)
  #:use-module (watch config))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: long-format - make output more detailed            ;;
;; ----------------------------------------------------------- ;;
(define* (list-shows-db #:optional long-format)
  (call-with-show-list
    #:overwrite
      #f
    #:proc
      (if long-format 
        list-shows-long
        list-shows-short)))

;; ------------------------------------------------------ ;;
;; Print show-list in long format.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - show-list to print                ;;
;; ------------------------------------------------------ ;;
(define (list-shows-long show-list)
  (format #t "total ~a~%" (length show-list))
  (let loop ((lst show-list))
    (unless (null? lst)
      (format #t 
              "~a ~4@a ~a~%" 
              '<date> 
              (show:current-episode (car lst)) 
              (show:name (car lst)))
      (loop (cdr lst)))))

;; ------------------------------------------------------ ;;
;; Print show-list in short format (names only).          ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list - show-list to print                ;;
;; ------------------------------------------------------ ;;
(define (list-shows-short show-list)
  (let* ((port
           (open-output-pipe "column -t"))
         (output-string 
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
                    (loop (cdr lst) (- count (string-length (show:name (car lst)))))))))))
    (display output-string port)
    (newline port)
    (close-pipe port)))
