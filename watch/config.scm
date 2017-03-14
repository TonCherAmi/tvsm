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

(define home-directory (format #f "~a/" (getenv "HOME")))

;(define resources-directory (string-append home-directory ".local/share/watch"))
; Development ONLY!
(define resources-directory (string-append home-directory "code/scheme-projects/watch"))

(define show-database-path (format #f "~a/~a" resources-directory "shows"))

(define media-player-command "mpv")

(define episode-format-list '(".mkv" ".avi" ".mp4" ".mpeg" ".mpv" ".mov" ".qt" ".m4v" ".svi" ".ogv" ".flv" ".webm" ".vob" ".wmv"))

(define columns 79)

(define config-path (++ (getenv "HOME") "/" ".config/watch/config"))

(define config-list (with-input-from-file config-path read))

(define (config property)
  (assoc property config-list))

(define (read-config)
  (let loop ((cfg-lst (read)))
    (cond
      ((null? cfg-lst)
       '())
      ((string? (cadr cfg-lst))
       (cons (cons (caar cfg-lst) (expand-var (cadr cfg-lst)))
             (loop (cdr cfg-lst))))
      (else
       (cons (car cfg-lst)
             (loop (cdr cfg-lst)))))))
