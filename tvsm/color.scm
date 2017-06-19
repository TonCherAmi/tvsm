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

(define-module (tvsm color)
  #:export     (color
                colorize-string)
  #:use-module (srfi srfi-13))

;; ------------------------------------------------------ ;;
;; An alist of attributes and their corresponding codes.  ;;
;; ------------------------------------------------------ ;;
(define sgr-parameters
  (list '(CLEAR      . "0")
        '(BOLD       . "1")
        '(DARK       . "2")
        '(ITALIC     . "3")
        '(UNDERLINED . "4")
        '(BLINK      . "5")
        '(HIDDEN     . "8")
        '(BLACK      . "30")
        '(RED        . "31")
        '(GREEN      . "32")
        '(YELLOW     . "33")
        '(BLUE       . "34")
        '(MAGENTA    . "35")
        '(CYAN       . "36")
        '(WHITE      . "37")))

;; ------------------------------------------------------ ;;
;; Get a string containing the ANSI escape sequence for   ;;
;; producing the requested set of attributes.             ;;
;; ------------------------------------------------------ ;;
;; #:param: lst<list<symbol>> - desired attributes        ;;
;;                                                        ;;
;; #:return<string>: ANSI escape sequense for producing   ;;
;;                   the requested set of attributes      ;;
;; ------------------------------------------------------ ;;
(define (color . lst)
  (let ((color-list 
          (map cdr
               (filter (lambda (x) x)
                       (map (lambda (color) (assoc color sgr-parameters))
                            lst)))))
    (if (null? color-list)
        ""
        (string-append 
         (string #\esc #\[)
         (string-join color-list ";" 'infix)
         "m"))))
  
;; ------------------------------------------------------ ;;
;; Get a copy of 'str' colorized using ANSI escape        ;;
;; sequences according to attributes specified in         ;;
;; 'color-list'. At the end of the returned string all    ;;
;; the attributes are be reset.                           ;;
;; ------------------------------------------------------ ;;
;; #:param: str<string> - a string to colorize            ;;
;;                                                        ;;
;; #:param: color-list<list<symbol>> - desired attributes ;;
;;                                                        ;;
;; #:return<string>: a copy of 'str' wrapped in ANSI      ;;
;;                   escape sequences required for        ;;
;;                   producing requested colors           ;;
;; ------------------------------------------------------ ;;
(define (colorize-string str . color-list)
  (string-append
   (apply color color-list)
   str
   (color 'CLEAR)))
