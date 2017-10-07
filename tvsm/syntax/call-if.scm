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

(define-module (tvsm syntax call-if)
  #:export     (call-if))

;; ------------------------------------------------------ ;;
;; More readable version of conditional procedure         ;;
;; application: ((if test? proc proc*) args ...)          ;;
;; ------------------------------------------------------ ;;
;; #:syntax: call-if test? (proc | proc*) arg ...         ;;
;; ------------------------------------------------------ ;;
(define-syntax call-if
  (syntax-rules (|)
    ((_ test (proc | proc*) arg ...)
     (if test
       (proc arg ...)
       (proc* arg ...)))))
