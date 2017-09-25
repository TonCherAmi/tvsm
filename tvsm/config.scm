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

(define-module (tvsm config)
  #:export     (config)
  #:use-module (tvsm util))

;; ---------------------------------------------------------- ;;
;; Get value of 'property' from the config.                   ;;
;; ---------------------------------------------------------- ;;
;; #:param: property :: symbol - config property identifier   ;;
;;                                                            ;;
;; #:return: x :: a - value of 'property' if it is found,     ;;
;;           #f otherwise                                     ;;
;; ---------------------------------------------------------- ;;
(define (config property)
  (let ((cfg-pair (assoc property config-list)))
    (if cfg-pair
      (cdr cfg-pair)
      #f)))

;; ---------------------------------------------------------- ;;
;; Get a list of possible config paths.                       ;;
;; ---------------------------------------------------------- ;;
;; #:return: x :: [string] - list of possible config paths    ;;
;; ---------------------------------------------------------- ;;
(define (path-list) 
  (let ((home (++ (getenv "HOME") "/")))
    (list (++ home ".config/tvsm/config")
          (++ home ".tvsm")
          "config")))

;; ---------------------------------------------------------- ;;
;; Expand environment variables in string properties of       ;;
;; a config.                                                  ;;
;; ---------------------------------------------------------- ;;
;; #:param: cfg-lst :: [(symbol . a)] - config list           ;;
;;                                                            ;;
;; #:return: x :: [(symbol . a)] - copy of that config list   ;;
;;           with environment variables in string properties  ;;
;;           expanded                                         ;;
;; ---------------------------------------------------------- ;;
(define (expand-config cfg-lst)
  (map (lambda (property)
         (let ((key   (car property))
               (value (cdr property)))
         (cons key
               (if (string? value)
                 (expand-variables value)
                 value))))
       cfg-lst))

;; ---------------------------------------------------------- ;;
;; An alist containing config properties with their values.   ;;
;; ---------------------------------------------------------- ;;
(define config-list
  (let loop ((paths (path-list)))
    (cond
      ((null? paths)
       (throw 'config-not-found-exception
              "cannot continue: Configuration file not found"))
      ((access? (car paths) R_OK)
       (expand-config (with-input-from-file (car paths) read)))
      (else
       (loop (cdr paths))))))
