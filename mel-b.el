;;; mel-b.el: Base64 encoder/decoder for GNU Emacs

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: MIME, Base64

;; This file is part of FLIM (Faithful Library about Internet Message).

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

(defvar base64-dl-module
  (and (fboundp 'dynamic-link)
       (let ((path (expand-file-name "base64.so" exec-directory)))
	 (and (file-exists-p path)
	      path))))


;;; @ implementations
;;;

(cond (base64-dl-module
       (require 'mel-b-dl)
       )
      ((and (featurep 'mule)
	    (not (featurep 'xemacs)))
       (require 'mel-b-ccl)
       )
      (t
       (require 'mel-b-el)
       ))

(defun base64-encoded-length (string)
  (let ((len (length string)))
    (* (+ (/ len 3)
	  (if (= (mod len 3) 0) 0 1)
	  ) 4)
    ))


;;; @ end
;;;

(provide 'mel-b)

;;; mel-b.el ends here.
