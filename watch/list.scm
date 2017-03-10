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

(define-module  (watch list)
  #:export      (list-shows-db)
  #:use-module  (watch show)
  #:use-module ((watch config)
                  #:prefix config:))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: verbose - make output more detailed                ;;
;; ----------------------------------------------------------- ;;
(define* (list-shows-db #:optional (verbose #f))
  (call-with-show-list
    #:overwrite 
      #f
    #:proc
      (lambda (show-list)
        (if verbose 
          (begin
            (format #t "total: ~a~%" (length show-list))
            (let loop ((show-list show-list))
              (unless (null? show-list)
                (print-show (car show-list) 20 #t) (newline)
                (loop (cdr show-list)))))
          (unless (null? show-list)
            (let* ((columns 
                     79)
                   (max-name-length
                     ;; Get length of the longest show name in the show-list.
                     (apply max (map (lambda (x) (string-length (show:name x))) show-list)))
                   (shows-per-line (quotient columns (1+ max-name-length))))
              (let loop ((count shows-per-line)
                         (show-list show-list))
                (unless (null? show-list)
                  (cond
                    ((zero? count) 
                     (newline)
                     (loop shows-per-line show-list))
                    (else 
                      (print-show (car show-list) max-name-length)
                      (loop (1- count) (cdr show-list))))))))))))

(define (list-shows-short show-list)
  (let* ((lines
           (1+ (quotient (+ (apply + (map (lambda (x) (string-length (show:name x))) show-list))
                            (* 2 (length show-list)))
                         config:columns)))
         (names-per-line
           (/ (length show-list) lines))
         (name-list 
           (let loop ((names '(()))
                      (shows show-list)
                      (count names-per-line)
             (cond 
               ((null? shows) 
                names)
               ((zero? count)
                (loop (cons '() names)
                      names
                      names-per-line))
               (else
                 (loop (cons (cons (show:name (car shows)) (car names))
                             (cdr names))
                       (cdr names)
                       (1- count))))))))
    ))
  

;; ------------------------------------------------------ ;;
;; Print show contents to (current-output-port)           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show to print                        ;;
;; #:param: verbose - make output more detailed           ;;
;; ------------------------------------------------------ ;;
(define* (print-show show minwidth #:optional (verbose #f))
  (call-with-values 
    (lambda () 
      (let ((format-string (string-append "~" (number->string minwidth) "a " 
                                          (if verbose " ~a  ~a" ""))))
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
