;; tvsm - a tv show manager.
;; Copyright © 2017-2018 Vasili Karaev
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

(define-module  (tvsm util func)
  #:export      (conjoin
                 disjoin)
  #:use-module ((srfi srfi-1)
  #:select      (every any)))

;; ------------------------------------------------------ ;;
;; Get a procedure that combines calls to each argument   ;;
;; with 'and'. Equivalent to '(and (f x ...) ...)'.       ;;
;; ------------------------------------------------------ ;;
;; #:param: f ... :: a ... -> b - procedure(s)            ;;
;;                                                        ;;
;; #:return: x :: a ... -> bool - conjoined procedure     ;;
;; ------------------------------------------------------ ;;
(define (conjoin . fs)
  (lambda xs
    (every (lambda (f) (apply f xs)) fs)))

;; ------------------------------------------------------ ;;
;; Get a procedure that combines calls to each argument   ;;
;; with 'or'. Equivalent to '(or (f x ...) ...)'.         ;;
;; ------------------------------------------------------ ;;
;; #:param: f ... :: a ... -> b - procedure(s)            ;;
;;                                                        ;;
;; #:return: x :: a ... -> bool - disjoined procedure     ;;
;; ------------------------------------------------------ ;;
(define (disjoin . fs)
  (lambda xs
    (any (lambda (f) (apply f xs)) fs)))
