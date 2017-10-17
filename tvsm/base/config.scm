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

(define-module (tvsm base config)
  #:export     (config)
  #:use-module (tvsm common)
  #:use-module (tvsm util path))

;; ---------------------------------------------------------- ;;
;; Get a property value from the config.                      ;;
;; ---------------------------------------------------------- ;;
;; #:param: key :: symbol - config property key               ;;
;;                                                            ;;
;; #:return: x :: a - value of property if it is found,       ;;
;;           #f otherwise                                     ;;
;; ---------------------------------------------------------- ;;
(define (config key)
  (let ((property (assoc key (force *config-list*))))
    (and=> property cdr)))

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
  (let ((expand-all (compose expand-user expand-variables)))
    (map (lambda (property)
           (let ((key   (car property))
                 (value (cdr property)))
             (cons key
                   (if (string? value)
                     (expand-all value)
                     value))))
         cfg-lst)))

;; ---------------------------------------------------------- ;;
;; A list containing config properties.                       ;;
;; ---------------------------------------------------------- ;;
;; #:global: *config-list* :: promise [(symbol . a)]          ;;
;; ---------------------------------------------------------- ;;
(define *config-list*
  ;; lazy evaluation prevents uncaught exception on module load
  (delay
    (let loop ((paths (path-list)))
      (cond
        ((null? paths)
         (throw 'config-not-found-exception
                "cannot continue: Configuration file not found"))
        ((access? (car paths) R_OK)
         (expand-config (with-input-from-file (car paths) read)))
        (else
         (loop (cdr paths)))))))
