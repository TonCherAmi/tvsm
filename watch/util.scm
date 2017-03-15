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
;; You should have received a copy of the GNU Lesser General Public License
;; along with watch. If not, see <http://www.gnu.org/licenses/>.

(define-module (watch util)
  #:use-module (ice-9 popen)
  #:export     (++
                call-with-input-pipe
                call-with-output-pipe))

;; ------------------------------------------------------ ;;
;; Shorthand for 'string-append'.                         ;;
;; ------------------------------------------------------ ;;
(define ++ string-append)

;; ------------------------------------------------------ ;;
;; Execute 'command' with a pipe from it and call 'proc'  ;;
;; with the resulting port. Pipe is closed afterwards and ;;
;; value returned from 'proc' is returned.                ;;
;; ------------------------------------------------------ ;;
;; #:param: command - a string representing a shell       ;;
;;          command                                       ;;
;; #:param: proc - a procedure that takes an input port   ;;
;;          as its only required argument.                ;;
;;                                                        ;;
;; #:return: return value of 'proc'                       ;;
;; ------------------------------------------------------ ;;
(define (call-with-input-pipe command proc)
  (let* ((port (open-input-pipe command))
         (ret  (proc port)))
    (close-pipe port)
    ret))

;; ------------------------------------------------------ ;;
;; Execute 'command' with a pipe to it and call 'proc'    ;;
;; with the resulting port. Pipe is closed afterwards and ;;
;; value returned from 'proc' is returned.                ;;
;; ------------------------------------------------------ ;;
;; #:param: command - a string representing a shell       ;;
;;          command                                       ;;
;; #:param: proc - a procedure that takes an input port   ;;
;;          as its only required argument.                ;;
;;                                                        ;;
;; #:return: return value of 'proc'                       ;;
;; ------------------------------------------------------ ;;
(define (call-with-output-pipe command proc)
  (let* ((port (open-output-pipe command))
         (ret  (proc port)))
    (close-pipe port)
    ret))
