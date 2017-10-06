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

(define-module (tvsm util)
  #:use-module (ice-9 regex)
  #:export     (expand-variables))

;; ------------------------------------------------------ ;;
;; Expand environment variables in a string.              ;; 
;; ------------------------------------------------------ ;;
;; #:param: str :: string - string                        ;;
;;                                                        ;;
;; #:return: x :: string - 'str' with environment         ;;
;;           variables expanded.                          ;;
;; ------------------------------------------------------ ;;
(define (expand-variables str)
  (let ((m (string-match "\\$\\w*\\b" str)))
    (if (not m)
      str
      (let* ((env (getenv (string-trim (match:substring m) #\$))))
        (expand-variables (string-replace str (if env env "")
                                          (match:start m)
                                          (match:end m)))))))
