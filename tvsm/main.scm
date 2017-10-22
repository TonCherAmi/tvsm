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
  #:use-module (ice-9 getopt-long)
  #:use-module (tvsm syntax cond-star)
  #:use-module (tvsm cmd add)
  #:use-module (tvsm cmd watch)
  #:use-module (tvsm cmd ls)
  #:use-module (tvsm cmd rm)
  #:use-module (tvsm cmd set))

;; ------------------------------------------------------ ;;
;; Main procedure.                                        ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - command line arguments     ;;
;; ------------------------------------------------------ ;;
(define (main args)
  (let* ((option-spec '((help    (single-char #\h))
                        (version (single-char #\v))))
         (options        (getopt-long args
                                      option-spec
                                      #:stop-at-first-non-option #t))
         (help-wanted    (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (cond*
      (help-wanted
       (display-help))
      (version-wanted
       (display-version))
      (else
       (let* ((stripped-args (option-ref options '() '()))
             ;; In case no arguments whatsoever were passed, command will slip 
             ;; through case and general help will be printed.
             (command (unless (null? stripped-args)
                        (string->symbol (car stripped-args)))))
         (catch #t
           (lambda ()
             (case command
               ((add) 
                (add stripped-args))
               ((watch)
                (watch stripped-args))
               ((ls)
                (ls stripped-args))
               ((rm)
                (rm stripped-args))
               ((set)
                (set stripped-args))
               (else
                (display-help))))
           (lambda (key message)
             (die message))))))))

;; ------------------------------------------------------ ;;
;; 'add' subcommand.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (add args)
  (let* ((option-spec
           `((help            (single-char #\h))
             (name            (single-char #\n)
                              (value #t)
                              (required? #t))
             (path            (single-char #\p)
                              (value #t)
                              (required? #t))
             (airing          (single-char #\a))
             (current-episode (single-char #\e)
                              (value #t)
                              (predicate ,string->number))
             (episode-offset  (single-char #\o)
                              (value #t)
                              (predicate ,string->number))))
         (options     (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (name        (option-ref options 'name #f))
         (path        (option-ref options 'path #f))
         (airing?     (option-ref options 'airing #f))
         (ep/current  (option-ref options 'current-episode "1"))
         (ep/offset   (option-ref options 'episode-offset "0")))
    (if help-wanted
      (display-help 'add)
      (let ((ep     (string->number ep/current))
            (offset (string->number ep/offset)))
        (add-show-db #:name name
                     #:path path
                     #:airing? airing?
                     #:ep/current (if (>= offset ep) (1+ offset) ep)
                     #:ep/offset offset)))))

;; ------------------------------------------------------ ;;
;; 'watch' subcommand.                                    ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (watch args)
  (let* ((option-spec
           `((help    (single-char #\h))
             (set     (single-char #\s))
             (episode (single-char #\e)
                      (value #t)
                      (predicate ,string->number))))
         (options     (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (set-wanted  (option-ref options 'set #f))
         (episode     (option-ref options 'episode #f))
         ;; Here we get a list that should consist of one element
         ;; which is the show name.
         (show-name   (option-ref options '() '())))
    (cond 
      (help-wanted
       (display-help 'watch))
      ((null? show-name)
       (throw 'insufficient-args-exception
              "insufficient arguments
Try 'tvsm watch --help' for more information."))
      (episode 
       (watch-show-db (car show-name)
                      #:set? set-wanted
                      #:ep (string->number episode)))
      (else 
       (watch-show-db (car show-name))))))

;; ------------------------------------------------------ ;;
;; 'ls' subcommand.                                       ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (ls args)
  (let* ((option-spec
           '((help          (single-char #\h))
             (long          (single-char #\l))
             (all           (single-char #\A))
             (watching      (single-char #\w))
             (finished      (single-char #\f))
             (airing        (single-char #\a))
             (completed     (single-char #\c))
             (watchable     (single-char #\W))
             (non-watchable (single-char #\N))))
         (options        (getopt-long args option-spec))
         (help-wanted    (option-ref options 'help #f))
         (long?          (option-ref options 'long #f))
         (all?           (option-ref options 'all  #f))
         (watching?      (option-ref options 'watching all?))
         (finished?      (option-ref options 'finished all?))
         (airing?        (option-ref options 'airing all?))
         (completed?     (option-ref options 'completed all?))
         (watchable?     (option-ref options 'watchable all?))
         (non-watchable? (option-ref options 'non-watchable all?)))
    (if help-wanted 
      (display-help 'ls)
      (list-shows-db #:long? long?
                     ;; default constraint
                     #:watching? (or watching?
                                     (not (or finished?
                                              airing?
                                              completed?
                                              watchable?
                                              non-watchable?)))
                     #:finished? finished?
                     #:airing? airing?
                     #:completed? completed?
                     #:watchable? watchable?
                     #:non-watchable? non-watchable?))))

;; ------------------------------------------------------ ;;
;; 'rm' subcommand.                                       ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (rm args)
  (let* ((option-spec
           '((help     (single-char #\h))
             (finished (single-char #\f))))
         (options         (getopt-long args option-spec))
         (help-wanted     (option-ref options 'help #f))
         (finished-wanted (option-ref options 'finished #f))
         ;; Here we get a list of show names passed as arguments
         (show-names      (option-ref options '() '())))
    (cond 
      (help-wanted 
       (display-help 'rm))
      (finished-wanted
       (remove-finished-db))
      ((null? show-names)
       (throw 'insufficient-args-exception
              "insufficient arguments
Try 'tvsm remove --help' for more information."))
      (else
       (remove-shows-db show-names)))))

;; ------------------------------------------------------ ;;
;; 'set' subcommand.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: args :: [string] - subcommand arguments       ;;
;; ------------------------------------------------------ ;;
(define (set args)
  (let* ((option-spec
           `((help            (single-char #\h))
             (name            (single-char #\n)
                              (value #t))
             (path            (single-char #\p)
                              (value #t))
             (airing          (single-char #\a))
             (completed       (single-char #\c))
             (current-episode (single-char #\e)
                              (value #t)
                              (predicate ,string->number))))
         (options     (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (name        (option-ref options 'name #f))
         (path        (option-ref options 'path #f))
         (airing?     (option-ref options 'airing #f))
         (completed?  (option-ref options 'completed #f))
         (ep/current  (option-ref options 'current-episode #f))
         ;; Here we get a list that should consist of one element
         ;; which is the show name passed as an argument.
         (show-name   (option-ref options '() '())))
    (if help-wanted
      (display-help 'set)
      (cond*
        ((null? show-name)
         (throw 'insufficient-args-exception
                "insufficient arguments
Try 'tvsm set --help' for more information."))
        (airing?
         (set-show-airing-db (car show-name) #t))
        (completed?
         (set-show-airing-db (car show-name) #f))
        (ep/current
          (set-show-current-episode-db (car show-name)
                                       (string->number ep/current)))
        (path
         (set-show-path-db (car show-name) path))
        (name
         (set-show-name-db (car show-name) name))))))

;; ------------------------------------------------------ ;;
;; Print a message and exit with a non-zero return value. ;;
;; ------------------------------------------------------ ;;
;; #:param: message :: string - error message             ;;
;; ------------------------------------------------------ ;;
(define (die message)
  (format (current-error-port) "tvsm: ~a~%" message)
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
                                        episodes of the show.
options:
    -a, --airing:                       mark show as airing.
    -e, --current-episode <integer>:    number of the current episode
    -o, --episode-offset  <integer>:    useful for shows whose first episode is
                                        numbered differently than 'E01'."))
    ((watch)
     (display "\
Usage: tvsm watch [<options>] <show> 

options:
    -e, --episode <integer>:    number of the episode to watch instead of 
                                current episode.
    -s, --set:                  if specified together with '--episode' show
                                will continue to progress further from that
                                specified episode."))
    ((ls)
     (display "\
Usage: tvsm ls [<options>]

options:
    -l, --long:    use a long listing format.

constraints (combinable):
    -A, --all:              all existing shows.

    -w, --watching:         shows you are currently watching (default).
    -f, --finished:         shows you had finished watching.

    -a, --airing:           airing shows.
    -c, --completed:        completed shows.

    -W, --watchable:        shows that can be watched at the moment.
    -N, --non-watchable:    shows that cannot be watched at the moment."))


    ((rm)
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
    watch:    watch a show.
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
