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

(define-module (tvsm cmd ls)
  #:export     (list-shows-db)
  #:use-module (tvsm common)
  #:use-module (tvsm syntax call-if)
  #:use-module (tvsm base show)
  #:use-module (tvsm base config)
  #:use-module (tvsm util func)
  #:use-module (tvsm util color))

;; ------------------------------------------------------ ;;
;; Print contents of the show database in a neat manner.  ;;
;; ------------------------------------------------------ ;;
;; #:param: long :: bool - if #t makes output more        ;;
;;          detailed                                      ;;
;;                                                        ;;
;; TODO: proper comments                                  ;;
;; ------------------------------------------------------ ;;
(define* (list-shows-db #:key long?
                              watching?  finished?
                              airing?    completed?
                              watchable? non-watchable?)
         ;; group constraints by their compatability and
         ;; pair them up with their corresponding predicates
                        ;; watching / finished
  (let* ((constraints `(((,watching?      . ,(negate show:finished?))
                         (,finished?      . ,show:finished?))
                        ;; airing / completed
                        ((,airing?        . ,show:airing?)
                         (,completed?     . ,(negate show:airing?)))
                        ;; watchable / non-watchable
                        ((,watchable?     . ,show:watchable?)
                         (,non-watchable? . ,(negate show:watchable?)))))
         (disjoint-preds (map (lambda (xs)
                                (let ((sub-preds (map cdr (filter car xs))))
                                  (apply disjoin (if (null? sub-preds)
                                                   (list (const #t))
                                                   sub-preds))))
                              constraints))
         (conjoint-pred (apply conjoin disjoint-preds)))
    (call-with-show-list
      #:overwrite
        #f
      #:proc
        (lambda (show-list)
          (call-if long?
            (list-shows-long | list-shows-short)
              (filter conjoint-pred show-list))))))

;; ------------------------------------------------------ ;;
;; Print show-list in long format.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list to print      ;;
;; ------------------------------------------------------ ;;
(define (list-shows-long show-list)
  (format #t "total ~a~%" (length show-list))
  (let ((c colorize))
    (for-each
      (lambda (show)
        (let ((fin? (show:finished? show))
              (air? (show:airing? show)))
          (format #t (++ "~a " (c "[" 'BLUE) "~a~a" (c "]" 'BLUE) " ~5@a ~a~%")
                  (strftime (config 'date-format) (localtime (show:date show)))
                  ;; 'f' stands for finished, 'w' for watching
                  (if fin? (c #\f 'RED) (c #\w 'GREEN))
                  ;; 'a' stands for airing, 'c' for completed
                  (if air? (c #\a 'CYAN) (c #\c 'MAGENTA))
                  (format #f "~a/~a"
                          (show:ep/watched show)
                          (+ (length (show:ep/list show))
                             (show:ep/offset show)))
                  (c (show:name show) 'BOLD))))
      show-list)))

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
