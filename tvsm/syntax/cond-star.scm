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

(define-module (tvsm syntax cond-star)
  #:export     (cond*))

;; ------------------------------------------------------ ;;
;; Version of 'cond' that evaluates EVERY 'cond'-clause   ;;
;; where TEST evaluates to a true value.                  ;;
;; Expressions following the 'else'-clause are evaluated  ;;
;; only if none of the preceding TESTs were true.         ;;
;; Clauses of forms (TEST => EXPRESSION) and (TEST) are   ;;
;; not supported.                                         ;;
;; ------------------------------------------------------ ;;
(define-syntax cond*
  (syntax-rules (else)
    ((_ (else exp exp* ...))
     (begin exp exp* ...))
    ((_ (test exp exp* ...) (else else-exp else-exp* ...))
     (if test
       (begin exp exp* ...)
       (begin else-exp else-exp* ...)))
    ((_ (test exp exp* ...) clause clause* ... (else else-exp else-exp* ...))
     (if test
       (begin exp exp* ... (cond* clause clause* ...))
       (cond* clause clause* ... (else else-exp else-exp* ...))))
    ((_ (test exp exp* ...))
     (if test
       (begin exp exp* ...)))
    ((_ (test exp exp* ...) clause clause* ...)
     (if test
       (begin exp exp* ... (cond* clause clause* ...))
       (cond* clause clause* ...)))))
