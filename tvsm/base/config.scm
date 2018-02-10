;; tvsm - a tv show manager.
;; Copyright © 2017-2018 Vasili Karaev
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
  #:use-module (srfi srfi-1)
  #:use-module (tvsm common)
  #:use-module (tvsm util path))

;;------------------------------------------------------- ;;
;; Get a property value from the config or throw if it is ;;
;; not found.                                             ;;
;; ------------------------------------------------------ ;;
;; #:param: key :: symbol - config property key           ;;
;;                                                        ;;
;; #:return: x :: a - value of property if it is found    ;;
;; ------------------------------------------------------ ;;
(define (config key)
  (let ((property (assoc key (force *config*))))
    (if property
      (cdr property)
      (throw 'config-parameter-missing
             (format #f "missing config parameter: '~a'" key)))))

;; ------------------------------------------------------ ;;
;; Get a valid config location or throw if none found.    ;;
;; ------------------------------------------------------ ;;
;; #:return: x :: string - valid config path              ;;
;; ------------------------------------------------------ ;;
(define (config-path)
  (let* ((paths (map expand-variables
                     '("$HOME/.config/tvsm/config"
                       "$HOME/.tvsm"
                       "config")))
         (valid-paths (filter (lambda (path)
                                (access? path R_OK))
                              paths)))
    (if (null? valid-paths)
      (throw 'config-not-found-exception
             "cannot continue: Configuration file not found")
      (car valid-paths))))

;; ------------------------------------------------------ ;;
;; Read config from a file.                               ;;
;; ------------------------------------------------------ ;;
;; #:param: path :: string - path to the config           ;;
;;                                                        ;;
;; #:return: x :: [(symbol . a)] - config                 ;;
;; ------------------------------------------------------ ;;
(define (read-config path)
  (catch 'read-error
    (lambda ()
      (let ((cfg (with-input-from-file path read)))
        (if (and (list? cfg) (every pair? cfg))
          cfg
          (throw 'read-error))))
    (lambda (key . args)
      (throw 'config-malformed-exception
             "cannot continue: Configuration file malformed"))))

;; ------------------------------------------------------ ;;
;; Expand environment variables in string properties of   ;;
;; a config.                                              ;;
;; ------------------------------------------------------ ;;
;; #:param: cfg :: [(symbol . a)] - config                ;;
;;                                                        ;;
;; #:return: x :: [(symbol . a)] - copy of that config    ;;
;;           with environment variables in string         ;;
;;           properties expanded                          ;;
;; ------------------------------------------------------ ;;
(define (expand-config cfg)
  (let ((expand-all (compose expand-user expand-variables)))
    (map (lambda (property)
           (let ((key   (car property))
                 (value (cdr property)))
             (cons key
                   (if (string? value)
                     (expand-all value)
                     value))))
         cfg)))

;; ------------------------------------------------------ ;;
;; A list containing config properties.                   ;;
;; ------------------------------------------------------ ;;
;; #:global: *config-list* :: promise [(symbol . a)]      ;;
;; ------------------------------------------------------ ;;
(define *config*
  ;; lazy evaluation prevents uncaught exception on module load
  (delay (expand-config (read-config (config-path)))))
