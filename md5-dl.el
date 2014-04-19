;;; md5-dl.el --- MD5 Message Digest Algorithm using DL module.

;; Copyright (C) 1999, 2001  Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: MD5, RFC 1321

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(provide 'md5-dl)			; beware of circular dependency.
(eval-when-compile (require 'md5))	; md5-dl-module.

;;; This file is loaded (from "md5.el") only when md5-dl-module is exists.
(defvar md5-dl-handle (dynamic-link md5-dl-module))

;;; md5-dl-module provides `md5-string'.
(dynamic-call "emacs_md5_init" md5-dl-handle)

(defun md5-region (beg end)
  (md5-string (buffer-substring-no-properties beg end)))

;;; Note that v21 `md5' takes two more args: CODING and NOERROR.
(defun md5 (object &optional beg end)
  "Return the MD5 (a secure message digest algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT."
  (if (stringp object)
      (md5-string object)
    (save-excursion
      (set-buffer object)
      (md5-region (or beg (point-min)) (or end (point-max))))))

(provide 'md5-dl)

;;; md5-dl.el ends here
