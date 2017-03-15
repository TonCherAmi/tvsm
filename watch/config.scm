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

(define-module (watch config)
  #:export     (home-directory
                resources-directory
                show-database-path
                media-player-command
                episode-format-list
                columns)
  #:use-module (watch util))

(define (config property)
  (assoc property config-list))

(define path-list (list "../config"
                        (++ (getenv "HOME") "/" ".config/watch/config")))

(define config-list (let loop ((paths path-list))
                      (cond
                        ((null? paths)
                         (throw 'config-not-found-exception
                                "cannot continue: Configuration file not found"))
                        ((access? (car paths) R_OK)
                         (with-input-from-file (car paths) read-config))
                        (else
                         (loop (cdr paths))))))

(define (read-config)
  (let loop ((cfg-lst (read)))
    (cond
      ((null? cfg-lst)
       '())
      ((string? (cadar cfg-lst))
       (cons (cons (caar cfg-lst) (expand-varables (cadar cfg-lst)))
             (loop (cdr cfg-lst))))
      (else
       (cons (car cfg-lst)
             (loop (cdr cfg-lst)))))))

(define (expand-varables str)
  (call-with-input-pipe 
    (++ "echo " str) 
    (lambda (port) 
      (read port))))
