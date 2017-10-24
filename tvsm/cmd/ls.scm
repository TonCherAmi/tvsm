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
;; #:param: long? :: bool - if #t makes output more       ;;
;;          detailed                                      ;;
;;                                                        ;;
;; #:param: nocolor? :: bool - if #t disable color output ;;
;;                                                        ;;
;; Constraints can be used to filter show db contents.    ;;
;; Grouped constraints are combined using 'or', and those ;;
;; groups are combined using 'and'. If no constraints in  ;;
;; a group were specified all constraints in a group are  ;;
;; set to true.                                           ;;
;;                                                        ;;
;; #:param: watching? :: bool - include shows the user is ;;
;;          currently watching                            ;;
;; #:param: finsihed? :: bool - include shows the user    ;;
;;          had finished watching                         ;;
;;                                                        ;;
;; #:param: airing? :: bool - include shows that are      ;; 
;;          marked as airing                              ;;
;; #:param: completed? :: bool - include shows that are   ;;
;;          not marked as airing                          ;;
;;                                                        ;;
;; #:param: watchable? :: bool - include shows that can   ;;
;;          be watched at the moment                      ;;
;; #:param: non-watchable? :: bool - include shows that   ;;
;;          cannot be watched at the moment               ;;
;; ------------------------------------------------------ ;;
(define* (list-shows-db #:key long?      nocolor?
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
              (filter conjoint-pred show-list) nocolor?)))))

;; ------------------------------------------------------ ;;
;; Print show-list in long format.                        ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list to print      ;;
;;                                                        ;;
;; #:param: nocolor? :: bool - if #t disable color output ;;
;; ------------------------------------------------------ ;;
(define (list-shows-long show-list nocolor?)
  (format #t "total ~a~%" (length show-list))
  (let* ((c  (if nocolor?
                (lambda xs
                  (object->string (car xs) display))
                colorize))
         (fmt (++ "~a " (c #\[ 'BLUE) "~a~a" (c #\] 'BLUE) " ~5@a ~a~%")))
    (for-each
      (lambda (show)
        (let ((fin? (show:finished? show))
              (air? (show:airing? show)))
          (format #t fmt
                  (strftime (config 'date-format) (localtime (show:date show)))
                  ;; 'f' stands for finished, 'w' for watching
                  (if fin? (c #\f 'RED) (c #\w 'GREEN))
                  ;; 'a' stands for airing, 'c' for completed
                  (if air? (c #\a 'CYAN) (c #\c 'MAGENTA))
                  (format #f "~a/~a"
                          (show:ep/watched show)
                          (+ (length (show:ep/list show))
                             (show:ep/offset show)))
                  (c (show:name show)
                     (if (show:watchable? show)
                       'BOLD
                       'CLEAR)))))
      show-list)))

;; ------------------------------------------------------ ;;
;; Print show-list in short format (names only).          ;;
;; ------------------------------------------------------ ;;
;; #:param: show-list :: [show] - show-list to print      ;;
;;                                                        ;;
;; #:param: nocolor? :: bool - if #t disable color output ;;
;; ------------------------------------------------------ ;;
(define (list-shows-short show-list nocolor?)
  (let ((c (if nocolor?
             (lambda xs
               (object->string (car xs) display))
             colorize)))
    (for-each
      (lambda (show)
        (format #t "~a~%"
                (c (show:name show)
                   (if (show:watchable? show)
                     'BOLD
                     'CLEAR))))
    show-list)))
