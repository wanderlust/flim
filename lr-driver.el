;; ---------------------------------------------------------------------- ;;
;; FICHIER               : lr-dvr.scm                                     ;;
;; DATE DE CREATION      : Fri May 31 15:47:05 1996                       ;;
;; DERNIERE MODIFICATION : Fri May 31 15:51:13 1996                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1996 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; The LR parser driver                                                   ;;
;;                                                                        ;;
;; lr-dvr.scm is part of the lalr.scm distribution which is free          ;;
;; software; you can redistribute it and/or modify                        ;;
;; it under the terms of the GNU General Public License as published by   ;;
;; the Free Software Foundation; either version 2, or (at your option)    ;;
;; any later version.                                                     ;;
;;                                                                        ;;
;; lalr.scm is distributed in the hope that it will be useful,            ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of         ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          ;;
;; GNU General Public License for more details.                           ;;
;;                                                                        ;;
;; You should have received a copy of the GNU General Public License      ;;
;; along with lalr.scm; see the file COPYING.  If not, write to           ;;
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  ;;
;; ---------------------------------------------------------------------- ;;

;; 1998/08/16: Tanaka Akira <akr@jaist.ac.jp> transplants from Scheme to Emacs-Lisp.

(provide 'lr-driver)

(defconst lr-max-stack-size 500)

(defun lr-push (stack sp new-cat goto-table lval)
  (let* ((state     (aref stack sp))
	 (new-state (cdr (assq new-cat (aref goto-table state))))
	 (new-sp    (+ sp 2)))
    (if (>= new-sp lr-max-stack-size)
	(error "PARSE ERROR : stack overflow")
	(progn
	  (aset stack new-sp new-state)
	  (aset stack (- new-sp 1) lval)
	  new-sp))))

(defun lr-parse (lexerp errorp action-table goto-table reduction-table token-defs)
  (let ((stack (make-vector lr-max-stack-size 0)) (sp 0) (input (funcall lexerp)))
    (catch 'parser
      (while t
        (let* ((state (aref stack sp))
               (i     (car input))
               (act   (let* ((l (aref action-table state)) (y (assq i l))) (if y (cdr y) (cdar l)))))

          (cond

           ;; Input succesfully parsed
           ((eq act 'accept)
            (throw 'parser (aref stack 1)))

           ;; Syntax error in input
           ((eq act '*error*)
            (throw 'parser
              (funcall errorp "PARSE ERROR : unexpected token : " 
                       (cdr (assq i token-defs)))))

           ;; Shift current token on top of the stack
           ((>= act 0)
            (aset stack (+ sp 1) (cdr input))
            (aset stack (+ sp 2) act)
            (setq sp (+ sp 2))
            (setq input (funcall lexerp)))

           ;; Reduce by rule (- act)
           (t 
            (setq sp (funcall (aref reduction-table (- act)) stack sp goto-table)))))))))
