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

(define-module (watch util)
  #:export     (++))

;; ------------------------------------------------------ ;;
;; Shorthand for 'string-append'.                         ;;
;; ------------------------------------------------------ ;;
(define-syntax ++
  (syntax-rules ()
    ((++ str ...)
     (string-append str ...))))
