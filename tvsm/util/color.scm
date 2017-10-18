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

(define-module (tvsm util color)
  #:export     (color
                colorize)
  #:use-module (tvsm common))

;; ------------------------------------------------------ ;;
;; Get a string containing the ANSI escape sequence for   ;;
;; producing the requested set of parameters.             ;;
;; ------------------------------------------------------ ;;
;; #:param: lst :: [symbol] - desired SGR parameters      ;;
;;                                                        ;;
;; #:return: x :: string - ANSI escape sequense for       ;;
;;           producing the requested set of attributes    ;;
;; ------------------------------------------------------ ;;
(define (color . lst)
  (let ((color-list
          (filter identity
                  (map (lambda (clr) 
                         (assq-ref *sgr-parameters* clr))
                       lst))))
    (if (null? color-list)
      ""
      (++ (string #\esc #\[)
          (string-join color-list ";" 'infix)
          "m"))))
  
;; ------------------------------------------------------ ;;
;; Get a string representation of 'obj' preceded by ANSI  ;;
;; escape sequence necessary to produce desired effects.  ;;
;; At the end of the string any effects are reset.        ;;
;; ------------------------------------------------------ ;;
;; #:param: obj :: a - an object to colorize              ;;
;;                                                        ;;
;; #:param: lst :: [symbol] - desired SGR parameters      ;;
;;                                                        ;;
;; #:return: x :: string - string representation of 'obj' ;;
;;           colorized using ANSI escape sequences        ;;
;; ------------------------------------------------------ ;;
(define (colorize obj . lst)
  (++ (apply color lst)
      (object->string obj display)
      (color 'CLEAR)))

;; ------------------------------------------------------ ;;
;; A list of Select Graphic Rendition parameters.         ;;
;; ------------------------------------------------------ ;;
;; #:global: *sgr-parameters* :: [(symbol . string)]      ;;
;; ------------------------------------------------------ ;;
(define *sgr-parameters*
  '((CLEAR      . "0")
    (BOLD       . "1")
    (DARK       . "2")
    (ITALIC     . "3")
    (UNDERLINED . "4")
    (BLINK      . "5")
    (HIDDEN     . "8")
    (BLACK      . "30")
    (RED        . "31")
    (GREEN      . "32")
    (YELLOW     . "33")
    (BLUE       . "34")
    (MAGENTA    . "35")
    (CYAN       . "36")
    (WHITE      . "37")))
