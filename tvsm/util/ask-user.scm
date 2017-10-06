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

(define-module (tvsm util ask-user)
  #:export     (ask-user-y/n)
  #:use-module (ice-9 rdelim))

;; ------------------------------------------------------ ;;
;; Print a message and prompt the user for a reply until  ;;
;; either a 'y/yes/n/no' reply is received or an eof      ;;
;; object is detected.
;; ------------------------------------------------------ ;;
;; #:param: message :: string - message to print          ;;
;;                                                        ;;
;; #:return: x :: bool - #t if the answer is 'y/yes',     ;;
;;           #f if the answer is 'n/no' or an eof object  ;;
;;           is detected                                  ;;
;; ------------------------------------------------------ ;;
(define (ask-user-y/n message)
  (display message)
  (let ((answer (read-line)))
    (cond
      ((eof-object? answer)
       #f)
      ((or (string-ci=? answer "y") (string-ci=? answer "yes"))
       #t)
      ((or (string-ci=? answer "n") (string-ci=? answer "no"))
       #f)
      (else
       (ask-user-y/n "Please answer (y/n): ")))))
