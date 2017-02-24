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

(define-module (watch list)
  #:export     (list-shows-db)
  #:use-module (watch show-utils))

;; ----------------------------------------------------------- ;;
;; Print contents of show-list database in a neat manner.      ;;
;; ----------------------------------------------------------- ;;
;; #:param: verbose - make output more detailed                ;;
;; ----------------------------------------------------------- ;;
(define* (list-shows-db #:optional (verbose #f))
  (let ((show-list (read-show-list-db)))
    (if verbose (format #t "total: ~a~%" (length show-list)))
    (for-each 
      (lambda (s) (print-show s verbose))
       show-list)))

;; ------------------------------------------------------ ;;
;; Print show contents to (current-output-port)           ;;
;; ------------------------------------------------------ ;;
;; #:param: show - a show to print                        ;;
;; #:param: verbose - make output more detailed           ;;
;; ------------------------------------------------------ ;;
(define* (print-show show #:optional (verbose #f))
  (call-with-values 
    (lambda () 
      (let ((format-string (string-append "~a\t" 
                                          (if verbose "~a\t~a~%" ""))))
        (if verbose
          (values 
             #t
             format-string
            (show:name show)
            (if (number? (show:current-episode show))
                  (1+ (show:current-episode show))
                  (show:current-episode show))
            (show:path show))
          (values
             #t
             format-string
            (show:name show)))))
     format))
