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
  #:use-module (ice-9 rdelim)
  #:use-module (tvsm util))

;; ---------------------------------------------------------- ;;
;; Get value of 'property' from the config.                   ;;
;; ---------------------------------------------------------- ;;
;; #:param: property<symbol> - name of the config property    ;;
;;                                                            ;;
;; #:return: x<a> - value of 'property' if it is found,       ;;
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
;; #:return: x<list<string>>: list of possible config paths   ;;
;; ---------------------------------------------------------- ;;
(define (path-list) 
  (let ((home (++ (getenv "HOME") "/")))
    (list (++ home ".tvsm")
          (++ home ".config/tvsm/config")
          "config")))

;; ---------------------------------------------------------- ;;
;; Read the config from (current-input-port).                 ;;
;; ---------------------------------------------------------- ;;
;; #:return: x<alist<symbol, a>> - association list where the ;;
;;           first item of an element pair is a property      ;;
;;           identifier and the second is that property's     ;;
;;           value.                                           ;;
;; ---------------------------------------------------------- ;;
(define (read-config)
  (let loop ((cfg-lst (read)))
    (cond
      ((null? cfg-lst)
       '())
      ((string? (cdar cfg-lst))
       (cons (cons (caar cfg-lst) (expand-variables (cdar cfg-lst)))
             (loop (cdr cfg-lst))))
      (else
       (cons (car cfg-lst)
             (loop (cdr cfg-lst)))))))

;; ---------------------------------------------------------- ;;
;; Expand environment variables in a string.                  ;; 
;; ---------------------------------------------------------- ;;
;; #:param: str<string> - a string                            ;;
;;                                                            ;;
;; #:return: x<string> - 'str' with environment variables     ;;
;;           expanded.                                        ;;
;; ---------------------------------------------------------- ;;
(define (expand-variables str)
  (call-with-input-pipe 
    (++ "echo " str) 
    (lambda (port) 
      (read-line port))))
 
;; ---------------------------------------------------------- ;;
;; Association list containing config properties with their   ;;
;; values.                                                    ;;
;; ---------------------------------------------------------- ;;
(define config-list
  (let loop ((paths (path-list)))
    (cond
      ((null? paths)
       (throw 'config-not-found-exception
              "cannot continue: Configuration file not found"))
      ((access? (car paths) R_OK)
       (with-input-from-file (car paths) read-config))
      (else
        (loop (cdr paths))))))
