;;; smtp.el --- basic functions to send mail with SMTP server

;; Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;;	Simon Leinen <simon@switch.ch> (ESMTP support)
;;	MORIOKA Tomohiko <tomo@m17n.org> (separate smtp.el from smtpmail.el)
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: SMTP, mail

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'poe)
(require 'poem)
(require 'pcustom)
(require 'mail-utils)			; mail-strip-quoted-names

(eval-when-compile (require 'cl))	; push

(defgroup smtp nil
  "SMTP protocol for sending mail."
  :group 'mail)

(defcustom smtp-default-server nil
  "*Specify default SMTP server."
  :type '(choice (const nil) string)
  :group 'smtp)

(defcustom smtp-server (or (getenv "SMTPSERVER") smtp-default-server)
  "*The name of the host running SMTP server.  It can also be a function
called from `smtp-via-smtp' with arguments SENDER and RECIPIENTS."
  :type '(choice (string :tag "Name")
		 (function :tag "Function"))
  :group 'smtp)

(defcustom smtp-service "smtp"
  "*SMTP service port number. \"smtp\" or 25."
  :type '(choice (integer :tag "25" 25)
                 (string :tag "smtp" "smtp"))
  :group 'smtp)

(defcustom smtp-use-8bitmime t
  "*If non-nil, use ESMTP 8BITMIME if available."
  :type 'boolean
  :group 'smtp)

(defcustom smtp-local-domain nil
  "*Local domain name without a host name.
If the function (system-name) returns the full internet address,
don't define this value."
  :type '(choice (const nil) string)
  :group 'smtp)

(defvar smtp-debug-info nil)
(defvar smtp-read-point nil)

(defun smtp-make-fqdn ()
  "Return user's fully qualified domain name."
  (let ((system-name (system-name)))
    (cond
     (smtp-local-domain
      (concat system-name "." smtp-local-domain))
     ((string-match "[^.]\\.[^.]" system-name)
      system-name)
     (t
      (error "Cannot generate valid FQDN. Set `smtp-local-domain' correctly.")))))

(defun smtp-via-smtp (sender recipients smtp-text-buffer)
  (let ((server (if (functionp smtp-server)
		    (funcall smtp-server sender recipients)
		  smtp-server))
	process response extensions)
    (save-excursion
      (set-buffer
       (get-buffer-create
	(format "*trace of SMTP session to %s*" server)))
      (erase-buffer)
      (make-local-variable 'smtp-read-point)
      (setq smtp-read-point (point-min))

      (unwind-protect
	  (catch 'done
	    (setq process (open-network-stream-as-binary
			   "SMTP" (current-buffer) server smtp-service))
	    (or process (throw 'done nil))

	    (set-process-filter process 'smtp-process-filter)

	    ;; Greeting
	    (setq response (smtp-read-response process))
	    (if (or (null (car response))
		    (not (integerp (car response)))
		    (>= (car response) 400))
		(throw 'done (car (cdr response))))

	    ;; EHLO
	    (smtp-send-command process
			       (format "EHLO %s" (smtp-make-fqdn)))
	    (setq response (smtp-read-response process))
	    (if (or (null (car response))
		    (not (integerp (car response)))
		    (>= (car response) 400))
		(progn
		  ;; HELO
		  (smtp-send-command process
				     (format "HELO %s" (smtp-make-fqdn)))
		  (setq response (smtp-read-response process))
		  (if (or (null (car response))
			  (not (integerp (car response)))
			  (>= (car response) 400))
		      (throw 'done (car (cdr response)))))
	      (let ((extension-lines (cdr (cdr response))))
		(while extension-lines
		  (push (intern (downcase (substring (car extension-lines) 4)))
			extensions)
		  (setq extension-lines (cdr extension-lines)))))

	    ;; ONEX --- One message transaction only (sendmail extension?)
	    (if (or (memq 'onex extensions)
		    (memq 'xone extensions))
		(progn
		  (smtp-send-command process "ONEX")
		  (setq response (smtp-read-response process))
		  (if (or (null (car response))
			  (not (integerp (car response)))
			  (>= (car response) 400))
		      (throw 'done (car (cdr response))))))

	    ;; VERB --- Verbose (sendmail extension?)
	    (if (and smtp-debug-info
		     (or (memq 'verb extensions)
			 (memq 'xvrb extensions)))
		(progn
		  (smtp-send-command process "VERB")
		  (setq response (smtp-read-response process))
		  (if (or (null (car response))
			  (not (integerp (car response)))
			  (>= (car response) 400))
		      (throw 'done (car (cdr response))))))

	    ;; XUSR --- Initial (user) submission (sendmail extension?)
	    (if (memq 'xusr extensions)
		(progn
		  (smtp-send-command process "XUSR")
		  (setq response (smtp-read-response process))
		  (if (or (null (car response))
			  (not (integerp (car response)))
			  (>= (car response) 400))
		      (throw 'done (car (cdr response))))))

	    ;; MAIL FROM:<sender>
	    (smtp-send-command
	     process
	     (format "MAIL FROM:<%s>%s%s"
		     sender
		     ;; SIZE --- Message Size Declaration (RFC1870)
		     (if (memq 'size extensions)
			 (format " SIZE=%d"
				 (save-excursion
				   (set-buffer smtp-text-buffer)
				   (+ (- (point-max) (point-min))
				      ;; Add one byte for each change-of-line
				      ;; because or CR-LF representation:
				      (count-lines (point-min) (point-max))
				      ;; For some reason, an empty line is
				      ;; added to the message.	Maybe this
				      ;; is a bug, but it can't hurt to add
				      ;; those two bytes anyway:
				      2)))
		       "")
		     ;; 8BITMIME --- 8bit-MIMEtransport (RFC1652)
		     (if (and (memq '8bitmime extensions)
			      smtp-use-8bitmime)
			 " BODY=8BITMIME"
		       "")))
	    (setq response (smtp-read-response process))
	    (if (or (null (car response))
		    (not (integerp (car response)))
		    (>= (car response) 400))
		(throw 'done (car (cdr response))))

	    ;; RCPT TO:<recipient>
	    (while recipients
	      (smtp-send-command process
				 (format "RCPT TO:<%s>" (car recipients)))
	      (setq recipients (cdr recipients))
	      (setq response (smtp-read-response process))
	      (if (or (null (car response))
		      (not (integerp (car response)))
		      (>= (car response) 400))
		  (throw 'done (car (cdr response)))))

	    ;; DATA
	    (smtp-send-command process "DATA")
	    (setq response (smtp-read-response process))
	    (if (or (null (car response))
		    (not (integerp (car response)))
		    (>= (car response) 400))
		(throw 'done (car (cdr response))))

	    ;; Mail contents
	    (smtp-send-data process smtp-text-buffer)

	    ;; DATA end "."
	    (smtp-send-command process ".")
	    (setq response (smtp-read-response process))
	    (if (or (null (car response))
		    (not (integerp (car response)))
		    (>= (car response) 400))
		(throw 'done (car (cdr response))))

	    t)

	(if (and process
		 (eq (process-status process) 'open))
	    (progn
	      ;; QUIT
	      (smtp-send-command process "QUIT")
	      (smtp-read-response process)
	      (delete-process process)))))))

(defun smtp-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun smtp-read-response (process)
  (let ((case-fold-search nil)
	(response-strings nil)
	(response-continue t)
	(return-value '(nil ()))
	match-end)

    (while response-continue
      (goto-char smtp-read-point)
      (while (not (search-forward "\r\n" nil t))
	(accept-process-output process)
	(goto-char smtp-read-point))

      (setq match-end (point))
      (setq response-strings
	    (cons (buffer-substring smtp-read-point (- match-end 2))
		  response-strings))
	
      (goto-char smtp-read-point)
      (if (looking-at "[0-9]+ ")
	  (let ((begin (match-beginning 0))
		(end (match-end 0)))
	    (if smtp-debug-info
		(message "%s" (car response-strings)))

	    (setq smtp-read-point match-end)

	    ;; ignore lines that start with "0"
	    (if (looking-at "0[0-9]+ ")
		nil
	      (setq response-continue nil)
	      (setq return-value
		    (cons (string-to-int
			   (buffer-substring begin end))
			  (nreverse response-strings)))))
	
	(if (looking-at "[0-9]+-")
	    (progn (if smtp-debug-info
		     (message "%s" (car response-strings)))
		   (setq smtp-read-point match-end)
		   (setq response-continue t))
	  (progn
	    (setq smtp-read-point match-end)
	    (setq response-continue nil)
	    (setq return-value
		  (cons nil (nreverse response-strings)))))))
    (setq smtp-read-point match-end)
    return-value))

(defun smtp-send-command (process command)
  (goto-char (point-max))
  (insert command "\r\n")
  (setq smtp-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun smtp-send-data-1 (process data)
  (goto-char (point-max))
  (if smtp-debug-info
      (insert data "\r\n"))
  (setq smtp-read-point (point))
  ;; Escape "." at start of a line.
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n"))

(defun smtp-send-data (process buffer)
  (let ((data-continue t)
	(sending-data nil)
	this-line
	this-line-end)

    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min)))

    (while data-continue
      (save-excursion
	(set-buffer buffer)
	(beginning-of-line)
	(setq this-line (point))
	(end-of-line)
	(setq this-line-end (point))
	(setq sending-data nil)
	(setq sending-data (buffer-substring this-line this-line-end))
	(if (or (/= (forward-line 1) 0) (eobp))
	    (setq data-continue nil)))

      (smtp-send-data-1 process sending-data))))

(defun smtp-deduce-address-list (smtp-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO:<address>."
  (let ((case-fold-search t)
	(simple-address-list "")
	this-line
	this-line-end
	addr-regexp
	(smtp-address-buffer (generate-new-buffer " *smtp-mail*")))
    (unwind-protect
	(save-excursion
	  ;;
	  (set-buffer smtp-address-buffer)
	  (erase-buffer)
	  (insert (save-excursion
		    (set-buffer smtp-text-buffer)
		    (buffer-substring-no-properties header-start header-end)))
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (if (re-search-forward "^RESENT-TO:" header-end t)
		(setq addr-regexp
		      "^\\(RESENT-TO:\\|RESENT-CC:\\|RESENT-BCC:\\)")
	      (setq addr-regexp	 "^\\(TO:\\|CC:\\|BCC:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines.
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names
			   (buffer-substring this-line this-line-end)))))
	  (erase-buffer)
	  (insert-string " ")
	  (insert-string simple-address-list)
	  (insert-string "\n")
	  ;; newline --> blank
	  (subst-char-in-region (point-min) (point-max) 10 ?  t)
	  ;; comma   --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t)
	  ;; tab     --> blank
	  (subst-char-in-region (point-min) (point-max)	 9 ?  t)

	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list
		    (cons (buffer-substring (match-beginning 1) (match-end 1))
			  recipient-address-list)))
	    recipient-address-list))
      (kill-buffer smtp-address-buffer))))

(provide 'smtp)

;;; smtp.el ends here
