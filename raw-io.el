;;; raw-io.el --- input/output without code-conversion

;; Copyright (C) 1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
;; Keywords: definition, MIME, multimedia, mail, news

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when-compile (require 'static))

(require 'pces)

(defalias 'binary-insert-file-contents 'insert-file-contents-as-binary)

(defalias 'binary-write-region 'write-region-as-binary)

(defalias 'binary-find-file-noselect 'find-file-noselect-as-binary)

(defalias 'binary-open-network-stream 'open-network-stream-as-binary)

(defun binary-start-process (name buffer program &rest program-args)
  "Like `start-process', q.v., but don't code conversion."
  (as-binary-process
   (apply (function start-process) name buffer program program-args)))

(defun binary-start-process-shell-command (name buffer &rest args)
  "Like `start-process-shell-command', q.v., but don't code conversion."
  (static-if (fboundp 'start-process-shell-command)
      (as-binary-process
       (apply (function start-process-shell-command) name buffer args))
    (as-binary-process
     (start-process name buffer shell-file-name shell-command-switch
		    (mapconcat (function identity) args " ")))))

(defalias 'raw-text-insert-file-contents 'insert-file-contents-as-raw-text)

(defalias 'raw-message-write-region 'write-region-as-raw-text-CRLF)


;;; @ end
;;;

(provide 'raw-io)

;;; raw-io.el ends here
