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
;; You should have received a copy of the GNU General Public License
;; along with tvsm. If not, see <http://www.gnu.org/licenses/>.

(define-module (tvsm main)
  #:export     (main)
  #:use-module (tvsm add)
  #:use-module (tvsm play)
  #:use-module (tvsm ls)
  #:use-module (tvsm rm)
  #:use-module (tvsm set)
  #:use-module (tvsm config)
  #:use-module (ice-9 getopt-long))

;; ------------------------------------------------------ ;;
;; Main procedure.                                        ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - command line arguments     ;;
;; ------------------------------------------------------ ;;
(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options (getopt-long args 
                               option-spec 
                               #:stop-at-first-non-option #t))
         (version-wanted  (option-ref options 'version #f))
         (help-wanted     (option-ref options 'help #f)))
    (if (or version-wanted help-wanted)
      (begin
        (if version-wanted
          (display-version))
        (if help-wanted 
          (display-help)))
      (let* ((stripped-args 
               (option-ref options '() '()))
             ;; In case no arguments whatsoever were passed, 'command' will just slip 
             ;; through the 'case' and general help will be printed.
             (command 
               (unless (null? stripped-args) 
                 (string->symbol (car stripped-args)))))
        (catch 
          #t
          ;; thunk
          (lambda ()
            (case command
              ((add) 
               (add stripped-args))
              ((play)
               (play stripped-args))
              ((ls)
               (ls stripped-args))
              ((rm)
               (rm stripped-args))
              ((set)
               (set args))
              (else
               (display-help))))
          ;; handler
          (lambda (key message)
            (die message)))))))

;; ------------------------------------------------------ ;;
;; 'add' subcommand.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (add args)
  (let* ((option-spec '((help             (single-char #\h) (value #f))
                        (name             (single-char #\n) (value #t))
                        (path             (single-char #\p) (value #t))
                        (airing           (single-char #\a) (value #f))
                        (starting-episode (single-char #\e) (value #t))
                        (episode-offset   (single-char #\o) (value #t))))
         (options          (getopt-long args option-spec))
         (help-wanted      (option-ref options 'help #f))
         (name             (option-ref options 'name #f))
         (path             (option-ref options 'path #f))
         (airing?          (option-ref options 'airing #f))
         (starting-episode (option-ref options
                                       'starting-episode 
                                       (number->string 
                                         (config 'episode-offset))))
         (episode-offset   (option-ref options 
                                       'episode-offset
                                       (number->string
                                         (config 'episode-offset)))))
    (cond 
      (help-wanted 
        (display-help 'add))
      ((not (and name path))
       (throw 'insufficient-args-exception
              "insufficient arguments
Try 'tvsm add --help' for more information."))
      (else 
       (let ((ep     (string->number starting-episode))
             (offset (string->number episode-offset)))
         (if (not (and ep offset))
            (throw 'wrong-option-type-exception
                   "fatal error: Cannot parse numerical value")
            (add-show-db #:name name 
                         #:path path 
                         #:airing? airing?
                         #:starting-episode (if (> offset ep) offset ep)
                         #:episode-offset offset)))))))

;; ------------------------------------------------------ ;;
;; 'play' subcommand.                                     ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (play args)
  (let* ((option-spec '((help        (single-char #\h) (value #f))
                        (episode     (single-char #\e) (value #t))
                        (set         (single-char #\s) (value #f))))
         (options      (getopt-long args option-spec))
         (help-wanted  (option-ref options 'help #f))
         (episode      (option-ref options 'episode #f))
         (set-wanted   (option-ref options 'set #f))
         ;; Here we get a list that should consist of one element
         ;; which is the show name.
         (show-name    (option-ref options '() '())))
    (cond 
      (help-wanted
        (display-help 'play))
      ;; If the list is empty then the required argument is missing.
      ((null? show-name)
       (throw 'insufficient-args-exception
              "missing show name
Try 'tvsm play --help' for more information."))
      (episode 
        (play-show-db (car show-name)
                      #:increment? set-wanted 
                      #:episode (string->number episode)))
      (else 
       (play-show-db (car show-name))))))

;; ------------------------------------------------------ ;;
;; 'ls' subcommand.                                       ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (ls args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                        (all  (single-char #\a) (value #f))
                        (long (single-char #\l) (value #f))))
         (options     (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (all-wanted  (option-ref options 'all  #f))
         (long-wanted (option-ref options 'long #f)))
    (if help-wanted 
      (display-help 'list)
      (list-shows-db #:all all-wanted #:long long-wanted))))

;; ------------------------------------------------------ ;;
;; 'rm' subcommand.                                       ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (rm args)
  (let* ((option-spec '((help     (single-char #\h) (value #f))
                        (finished (single-char #\f) (value #f))))
         (options         (getopt-long args option-spec))
         (help-wanted     (option-ref options 'help #f))
         (finished-wanted (option-ref options 'finished #f))
         ;; Here we get a list that should consist of one element
         ;; which is the show name passed as an argument.
         (show-name      (option-ref options '() '())))
    (cond 
      (help-wanted 
        (display-help 'remove))
      (finished-wanted
        (remove-finished-db))
      ((null? show-name)
       (throw 'insufficient-args-exception
              "insufficient arguments
Try 'tvsm remove --help' for more information."))
      (else 
        (remove-show-db (car show-name))))))

;; ------------------------------------------------------ ;;
;; 'set' subcommand.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (set args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                        (name            (single-char #\n) (value #t))
                        (path            (single-char #\p) (value #t))
                        (airing          (single-char #\a) (value #f))
                        (completed       (single-char #\c) (value #f))
                        (current-episode (single-char #\e) (value #t))))
         (options         (getopt-long args option-spec))
         (help-wanted     (option-ref options 'help #f))
         (name            (option-ref options 'name #f))
         (path            (option-ref options 'path #f))
         (airing?         (option-ref options 'airing #f))
         (completed?      (option-ref options 'completed #f))
         (current-episode (option-ref options 'current-episode #f))
         ;; Here we get a list that should consist of one element
         ;; which is the show name passed as an argument.
         (show-name       (option-ref options '() '())))
    (cond
      (help-wanted
        (display-help 'set))
      ;; This checks for the case where no show name was passed as an argument.
      ((null? show-name)
       (throw 'insufficient-args-exception
              "insufficient arguments
Try 'tvsm set --help' for more information."))
      (name
        (set-show-name-db (car show-name) name))
      (path
        (set-show-path-db (car show-name) path))
      (airing?
        (set-show-airing-db (car show-name) #t))
      (completed?
        (set-show-airing-db (car show-name) #f))
      (current-episode
        (set-show-current-episode-db (car show-name) 
                                     (string->number current-episode))))))

;; ------------------------------------------------------ ;;
;; Print a message and exit with a non-zero return value. ;;
;; ------------------------------------------------------ ;;
;; #:param: message :: string - error message             ;;
;; ------------------------------------------------------ ;;
(define (die message)
  (format #t "tvsm: ~a~%" message)
  (exit 1))

;; ------------------------------------------------------ ;;
;; Print a help message.                                  ;;
;; ------------------------------------------------------ ;;
;; #:param: command :: symbol - a sub-command name        ;;
;;          whose help message is to be printed.          ;;
;;          if not specified or not matching any existing ;;
;;          sub-command general help message is printed.  ;;
;; ------------------------------------------------------ ;;
(define* (display-help #:optional command)
  (case command
    ((add) 
     (display "\
Usage: tvsm add <required-arguments> [<options>]

required-arguments: 
    -n, --name <name>:                  show name, a unique identifier.
    -p, --path <path>:                  path to the directory that contains 
                                        episodes of the new show.
options:
    -s, --starting-episode <integer>:   number of the episode the new show will
                                        start playing from.
    -o, --episode-offset   <integer>:   useful when episode numbering of a show
                                        deviates from the usual sequential numbering
                                        i.e. when first episode is not numbered 'E01'.
    -a, --airing:                       mark show as airing."))
    ((play)
     (display "\
Usage: tvsm play [<options>] <show> 

options:
    -e, --episode <integer>:    number of the episode to play instead of 
                                current episode.
    -s, --set:                  if specified together with '--episode' show
                                will continue to play from that specified episode
                                in the future."))
    ((list)
     (display "\
Usage: tvsm ls [<options>]

options:
    -a, --all:      do not ignore finished shows.
    -l, --long:     use a long listing format."))
    ((remove)
     (display "\
Usage: tvsm rm [<options>] [<name>...]

options:
    -f, --finished:     remove all shows that you have finished watching."))
    ((set)
     (display "\
Usage: tvsm set [<options>] <name>

options:
    -n, --name <name>:                  set a new name.
    -p, --path <path>:                  set a new path.
    -a, --airing:                       mark show as airing.
    -c, --completed:                    mark show as completed.
    -e, --current-episode <integer>:    set current episode."))
    (else 
      (display "\
Usage: tvsm [--version] [--help] <command> [<options>]

available commands:
    add:      add a show.
    play:     play a show.
    ls:       list existing shows.
    rm:       remove shows.
    set:      modify a show.
    
See 'tvsm <command> --help' to learn more about a specific command.")))
  (newline))

;; ------------------------------------------------------ ;;
;; Print current application version.                     ;;
;; ------------------------------------------------------ ;;
(define (display-version)
  (display "tvsm version 0.2") (newline))
