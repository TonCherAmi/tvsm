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
  #:use-module (tvsm config)
  #:use-module (tvsm color))

;; ------------------------------------------------------ ;;
;; Print contents of the show database in a neat manner.  ;;
;; ------------------------------------------------------ ;;
;; #:param: all :: bool - do not ignore finished shows    ;;
;;                                                        ;;
;; #:param: long-format :: bool - makes output more       ;;
;;          detailed if #t                                ;;
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
;; #:param: show-list :: [show] - show-list to print      ;;
;; ------------------------------------------------------ ;;
(define (list-shows-long show-list)
  (format #t "total ~a~%" (length show-list))
  (let ((cs colorize-string))
    (let loop ((lst show-list))
      (unless (null? lst)
        (let* ((show (car lst))
               (fin? (show-finished? show))
               (air? (show:airing? show)))
          (format #t (++ "~a " (cs "[" 'BLUE) "~a~a" (cs "]" 'BLUE) " ~5@a  ~a~%")
                  (show:date show)
                  ;; 'f' stands for finished, 'w' for watching
                  (if fin? (cs "f" 'RED) (cs "w" 'GREEN))
                  ;; 'a' stands for airing, 'c' for completed
                  (if air? (cs "a" 'CYAN) (cs "c" 'MAGENTA))
                  (format #f "~a/~a"
                          (if fin? #\- (show:current-episode show #:with-offset #t))
                          (+ (length (show:episode-list show))
                             (if (zero? (show:episode-offset show))
                               0 
                               (1- (show:episode-offset show)))))
                  (cs (show:name show) 'BOLD)))
        (loop (cdr lst))))))

;; ------------------------------------------------------ ;;
;; Print show-list in short format (names only).          ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list to print      ;;
;; ------------------------------------------------------ ;;
(define (list-shows-short show-list)
  (for-each
    (lambda (show)
      (display (show:name show)) (newline))
    show-list))
