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

(define-module (tvsm play)
  #:export     (play-show-db)
  #:use-module (ice-9 ftw)
  #:use-module (tvsm show)
  #:use-module (tvsm color)
  #:use-module (tvsm config))

;; ------------------------------------------------------------------------ ;;
;; Play an episode of show called show-name.                                ;;
;; ------------------------------------------------------------------------ ;;
;; #:param: show-name - a string representing the name of the show that is  ;;
;;          played                                                          ;;
;;                                                                          ;;
;; #:param: increment? - a boolean value that determines whether show's     ;;
;;          current-episode index will be incremented after current episode ;;
;;          is played.                                                      ;;
;;                                                                          ;;
;; #:param: episode - an integer that represents an episode that will be    ;;
;;          used instead of shows's current-epsidoe if specified.           ;;
;; ------------------------------------------------------------------------ ;;
(define* (play-show-db show-name #:key (increment? #t) (episode #f))
  (call-with-show-list 
    #:overwrite
      #t
    #:proc
      (lambda (show-list)
        (let* ((show-db (find-show show-name show-list))
               (show 
                 (cond 
                   ;; If show called show-name is not found in the db throw an exception.
                   ((not show-db)
                    (throw 'show-not-found-exception
                           (format #f "cannot play '~a': No such show" show-name)))
                   ;; If custom-episode-index was passed make a show that has it as its 
                   ;; current-episode index.
                   (episode 
                     (remake-show show-db 
                                  #:current-episode episode
                                  #:subtract-offset? #t))
                   (else show-db))))
          (if (not (show-playable? show))
            (throw 'show-not-playable-exception
                   (format #f "cannot play '~a': ~a" 
                           show-name
                           (if (show:airing? show)
                             "No new episodes"
                             "No episodes left")))
            (let ((episode-path (show:current-episode-path show))) 
              (format #t "Playing episode no. ~a of '~a'~%"
                      (colorize-string (number->string (show:current-episode show #:with-offset #t)) 'BOLD)
                      (colorize-string (show:name show) 'BOLD))
              (cond
                ;; Shell will return 0 on successful command execution.
                ((not (zero? (play-episode episode-path)))
                 (throw 'external-command-fail-exception 
                        (format #f "cannot play '~a': Media player command failed" show-name)))
                (increment?
                  (let ((updated-show (show:current-episode-inc show)))
                    (cons updated-show (remove-show show-name show-list))))
                (else show-list))))))))

;; ------------------------------------------------------------ ;;
;; Get full path to show's current-episode.                     ;;
;; ------------------------------------------------------------ ;;
;; #:param: show - a show                                       ;;
;;                                                              ;;
;; #:return: a string representing a full path to the episode   ;;
;;           of show pointed to by current-episode index.       ;;
;; ------------------------------------------------------------ ;;
(define (show:current-episode-path show)
  (let ((format-string (if (string-suffix? "/" (show:path show)) 
                         "~a~a" 
                         "~a/~a")))
    (format #f format-string 
            (show:path show)
            (list-ref (show:episode-list show) 
                      (show:current-episode show)))))

;; ------------------------------------------------------------ ;;
;; Play an episode using user-defined media player command.     ;;
;; ------------------------------------------------------------ ;;
;; #:param: episode-path - a string representing full path to a ;;
;;          media file representing an episode                  ;;
;;                                                              ;;
;; #:return: zero is returned on successful command execution   ;;
;;           non-zero error code is returned otherwise          ;;
;; ------------------------------------------------------------ ;;
(define (play-episode episode-path)
  ;; This doesn't work for paths with double quotes in them
  ;; but who would put a double quote in a filename anyway?
  (system (format #f "~a \"~a\"" 
                  (config 'media-player-command) episode-path)))
