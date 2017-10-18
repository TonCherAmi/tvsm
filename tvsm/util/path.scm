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

(define-module (tvsm util path)
  #:export     (mkdirs
                directory?
                expand-user
                expand-variables)
  #:use-module (ice-9 regex))

;; ------------------------------------------------------ ;;
;; Create directories recursively. Like 'mkdir', but      ;;
;; makes all intermediate-level directories needed to     ;;
;; contain the leaf directory.                            ;;
;; ------------------------------------------------------ ;;
;; #:param: path :: string - path                         ;;
;;                                                        ;;
;; #:param: mode :: int(octal) - permissions              ;;
;; ------------------------------------------------------ ;;
(define* (mkdirs path #:optional (mode #o777))
  (let ((parent-directory (dirname path)))
    (cond
      ((file-exists? path)
       (throw 'file-exists-exception EEXIST path))
      ((not (file-exists? parent-directory))
       (mkdirs parent-directory))
      ((not (directory? parent-directory))
       (throw 'not-directory-exception ENOTDIR parent-directory))
      ((not (access? parent-directory W_OK))
       (throw 'permission-denied-exception EACCES path)))
    (mkdir path mode)))

;; ------------------------------------------------------ ;;
;; Check whether 'path' is an existing directory.         ;;
;; Follows symlinks.                                      ;;
;; ------------------------------------------------------ ;;
;; #:param: path :: string - path                         ;;
;;                                                        ;;
;; #:return: x :: bool - #t if 'path' is an existing dir, ;;
;;           #f otherwise                                 ;;
;; ------------------------------------------------------ ;;
(define (directory? path)
  (and (file-exists? path)
       (eq? 'directory (stat:type (stat path)))))

;; ------------------------------------------------------ ;;
;; Expand initial '~' or '~user' in a path by replacing   ;;
;; it with user's home directory. If the path does not    ;;
;; begin with '~', 'path' is returned unchanged.          ;;
;; If 'user' is not found path is returned unchanged.     ;;
;; ------------------------------------------------------ ;;
;; #:param: path :: string - path                         ;;
;;                                                        ;;
;; #:return: x :: string - 'path' with initial '~' or     ;;
;;           '~user' expanded                             ;;
;; ------------------------------------------------------ ;;
(define (expand-user path)
  (if (not (string-prefix? "~" path))
    path
    (catch 'misc-error
      (lambda ()
        (let* ((i (string-index path #\/ 1))
               (index (if i i (string-length path)))
               (pw (if (eq? index 1)
                     ;; if path is of form '~/path'
                     (getpwuid (getuid))
                     ;; if path is of form '~user/path'
                     (getpwnam (substring path 1 index)))) ;; <- throws if no such user
               (home (passwd:dir pw)))
          (string-replace path home 0 index)))
      (lambda (key . args)
        path))))

;; ------------------------------------------------------ ;;
;; Expand environment variables in a path. Only '$var'    ;;
;; form is supported ('${var}' is not).                   ;;
;; ------------------------------------------------------ ;;
;; #:param: path :: string - path                         ;;
;;                                                        ;;
;; #:return: x :: string - 'path' with environment        ;;
;;           variables expanded.                          ;;
;; ------------------------------------------------------ ;;
(define (expand-variables path)
  (let ((m (string-match "\\$\\w*\\b" path)))
    (if (not m)
      path
      (let ((env (getenv (string-trim (match:substring m) #\$))))
        (expand-variables (string-replace path (if env env "")
                                          (match:start m)
                                          (match:end m)))))))
