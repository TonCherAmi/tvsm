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

(define-module (tvsm cmd watch)
  #:export     (watch-show-db)
  #:use-module (tvsm base show)
  #:use-module (tvsm base config)
  #:use-module (tvsm util color))

;; ----------------------------------------------------- ;;
;; Watch an episode of a show.                           ;;
;; ----------------------------------------------------- ;;
;; #:param: show-name :: string - show name              ;;
;;                                                       ;;
;; #:param: set? :: bool - if #t write progress to db    ;;
;;                                                       ;;
;; #:param: ep :: int - episode to watch instead of      ;;
;;          show's current episode                       ;;
;; ----------------------------------------------------- ;;
(define* (watch-show-db show-name #:key (set? #t) (ep #f))
  (call-with-show-list
    #:overwrite
      set?
    #:proc
      (lambda (show-list)
        (let* ((show*
                 (find-show show-name show-list))
               (show
                 (cond
                   ((not show*)
                    (throw 'show-not-found-exception
                           (format #f "cannot watch '~a': No such show" show-name)))
                   (ep
                    (remake-show show* #:ep/current ep))
                   (else
                    show*))))
          (unless (show:watchable? show)
            (throw 'show-not-watchable-exception
                   (format #f "cannot watch '~a': ~a"
                           show-name
                           (if ep
                             "Episode out of bounds"
                             "No episodes left"))))
          (format #t "Watching '~a', episode no. ~a/~a~%"
                  (colorize (show:name show) 'BOLD)
                  (colorize (show:ep/current show) 'BOLD)
                  (colorize (length (show:ep/list show)) 'BOLD))
          (let ((episode-path (show:ep/current-path show)))
            (catch 'media-player-command-exception
              (lambda ()
                (play-file episode-path)
                (cons (show:ep/index-inc show)
                      (remove-show show-name show-list)))
              (lambda (key message)
                (throw key (format #f "cannot watch '~a': ~a" show-name message)))))))))

;; ----------------------------------------------------- ;;
;; Get absolute path to current episode of a show.       ;;
;; ----------------------------------------------------- ;;
;; #:param: show :: show - show                          ;;
;;                                                       ;;
;; #:return: x :: string - absolute path to current ep   ;;
;; ----------------------------------------------------- ;;
(define (show:ep/current-path show)
  (let ((fmt (if (string-suffix? "/" (show:path show))
               "\"~a~a\""
               "\"~a/~a\"")))
    (format #f fmt
            (show:path show)
            (list-ref (show:ep/list show)
                      (show:ep/index show)))))

;; ----------------------------------------------------- ;;
;; Play a file using user-defined media player command.  ;;
;; ----------------------------------------------------- ;;
;; #:param: path :: string - absolute path to a file     ;;
;; ----------------------------------------------------- ;;
(define (play-file path)
  (let ((mpcmd-err (lambda (what)
                     (throw 'media-player-command-exception
                            (format #f "Media player command ~a" what))))
        (command (config 'media-player-command)))
    (cond
      ((not command)
       (mpcmd-err "not set"))
      ((not (and (string? command) (string-contains command "~a")))
       (mpcmd-err "malformed"))
      ((not (zero? (system (format #f command path))))
       (mpcmd-err "failed")))))
