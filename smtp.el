;;; smtp.el --- basic functions to send mail with SMTP server

;; Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; ESMTP support: Simon Leinen <simon@switch.ch>
;; Keywords: SMTP, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defgroup smtp nil
  "SMTP protocol for sending mail."
  :group 'mail)

(defcustom smtp-default-server nil
  "*Specify default SMTP server."
  :type '(choice (const nil) string)
  :group 'smtp)

(defcustom smtp-server 
  (or (getenv "SMTPSERVER") smtp-default-server)
  "*The name of the host running SMTP server."
  :type '(choice (const nil) string)
  :group 'smtp)

(defcustom smtp-service 25
  "*SMTP service port number. smtp or 25 ."
  :type 'integer
  :group 'smtp)

(defcustom smtp-local-domain nil
  "*Local domain name without a host name.
If the function (system-name) returns the full internet address,
don't define this value."
  :type '(choice (const nil) string)
  :group 'smtp)

(defcustom smtp-debug-info nil
  "*smtp debug info printout. messages and process buffer."
  :type 'boolean
  :group 'smtp)

(defcustom smtp-coding-system 'binary
  "*Coding-system for SMTP output."
  :type 'coding-system
  :group 'smtp)


(defun smtp-fqdn ()
  (if smtp-local-domain
      (concat (system-name) "." smtp-local-domain)
    (system-name)))

(defun smtp-via-smtp (recipient smtp-text-buffer)
  (let ((process nil)
	(host smtp-server)
	(port smtp-service)
	response-code
	greeting
	process-buffer
	(supported-extensions '())
	(coding-system-for-read smtp-coding-system)
	(coding-system-for-write smtp-coding-system))
    (unwind-protect
	(catch 'done
	  ;; get or create the trace buffer
	  (setq process-buffer
		(get-buffer-create
		 (format "*trace of SMTP session to %s*" host)))

	  ;; clear the trace buffer of old output
	  (save-excursion
	    (set-buffer process-buffer)
	    (erase-buffer))

	  ;; open the connection to the server
	  (setq process (open-network-stream "SMTP" process-buffer host port))
	  (and (null process) (throw 'done nil))

	  ;; set the send-filter
	  (set-process-filter process 'smtp-process-filter)

	  (save-excursion
	    (set-buffer process-buffer)
	    (make-local-variable 'smtp-read-point)
	    (setq smtp-read-point (point-min))

	    (if (or (null (car (setq greeting (smtp-read-response process))))
		    (not (integerp (car greeting)))
		    (>= (car greeting) 400))
		(throw 'done nil)
	      )

	    ;; EHLO
	    (smtp-send-command process (format "EHLO %s" (smtp-fqdn)))

	    (if (or (null (car (setq response-code (smtp-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(progn
		  ;; HELO
		  (smtp-send-command process (format "HELO %s" (smtp-fqdn)))

		  (if (or (null (car (setq response-code (smtp-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil)))
	      (let ((extension-lines (cdr (cdr response-code))))
		(while extension-lines
		  (let ((name (intern (downcase (substring (car extension-lines) 4)))))
		    (and name
			 (cond ((memq name '(verb xvrb 8bitmime onex xone
						  expn size dsn etrn
						  help xusr))
				(setq supported-extensions
				      (cons name supported-extensions)))
			       (t (message "unknown extension %s"
					   name)))))
		  (setq extension-lines (cdr extension-lines)))))

	    (if (or (member 'onex supported-extensions)
		    (member 'xone supported-extensions))
		(progn
		  (smtp-send-command process (format "ONEX"))
		  (if (or (null (car (setq response-code (smtp-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (and smtp-debug-info
		     (or (member 'verb supported-extensions)
			 (member 'xvrb supported-extensions)))
		(progn
		  (smtp-send-command process (format "VERB"))
		  (if (or (null (car (setq response-code (smtp-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (member 'xusr supported-extensions)
		(progn
		  (smtp-send-command process (format "XUSR"))
		  (if (or (null (car (setq response-code (smtp-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    ;; MAIL FROM: <sender>
	    (let ((size-part
		   (if (member 'size supported-extensions)
		       (format " SIZE=%d"
			       (save-excursion
				 (set-buffer smtp-text-buffer)
				 ;; size estimate:
				 (+ (- (point-max) (point-min))
				    ;; Add one byte for each change-of-line
				    ;; because or CR-LF representation:
				    (count-lines (point-min) (point-max))
				    ;; For some reason, an empty line is
				    ;; added to the message.  Maybe this
				    ;; is a bug, but it can't hurt to add
				    ;; those two bytes anyway:
				    2)))
		     ""))
		  (body-part
		   (if (member '8bitmime supported-extensions)
		       ;; FIXME:
		       ;; Code should be added here that transforms
		       ;; the contents of the message buffer into
		       ;; something the receiving SMTP can handle.
		       ;; For a receiver that supports 8BITMIME, this
		       ;; may mean converting BINARY to BASE64, or
		       ;; adding Content-Transfer-Encoding and the
		       ;; other MIME headers.  The code should also
		       ;; return an indication of what encoding the
		       ;; message buffer is now, i.e. ASCII or
		       ;; 8BITMIME.
		       (if nil
			   " BODY=8BITMIME"
			 "")
		     "")))
;	      (smtp-send-command process (format "MAIL FROM:%s@%s" (user-login-name) (smtp-fqdn)))
	      (smtp-send-command process (format "MAIL FROM: <%s>%s%s"
						     user-mail-address
						     size-part
						     body-part))

	      (if (or (null (car (setq response-code (smtp-read-response process))))
		      (not (integerp (car response-code)))
		      (>= (car response-code) 400))
		  (throw 'done nil)
		))
	    
	    ;; RCPT TO: <recipient>
	    (let ((n 0))
	      (while (not (null (nth n recipient)))
		(smtp-send-command process (format "RCPT TO: <%s>" (nth n recipient)))
		(setq n (1+ n))

		(setq response-code (smtp-read-response process))
		(if (or (null (car response-code))
			(not (integerp (car response-code)))
			(>= (car response-code) 400))
		    (throw 'done nil)
		  )
		))
	    
	    ;; DATA
	    (smtp-send-command process "DATA")

	    (if (or (null (car (setq response-code (smtp-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;; Mail contents
	    (smtp-send-data process smtp-text-buffer)

	    ;;DATA end "."
	    (smtp-send-command process ".")

	    (if (or (null (car (setq response-code (smtp-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;;QUIT
;	    (smtp-send-command process "QUIT")
;	    (and (null (car (smtp-read-response process)))
;		 (throw 'done nil))
	    t ))
      (if process
	  (save-excursion
	    (set-buffer (process-buffer process))
	    (smtp-send-command process "QUIT")
	    (smtp-read-response process)

;	    (if (or (null (car (setq response-code (smtp-read-response process))))
;		    (not (integerp (car response-code)))
;		    (>= (car response-code) 400))
;		(throw 'done nil)
;	      )
	    (delete-process process))))))

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
		  (cons nil (nreverse response-strings)))
	    )
	  )))
    (setq smtp-read-point match-end)
    return-value))

(defun smtp-send-command (process command)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert "PASS <omitted>\r\n")
    (insert command "\r\n"))
  (setq smtp-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun smtp-send-data-1 (process data)
  (goto-char (point-max))

  (if smtp-debug-info
      (insert data "\r\n"))

  (setq smtp-read-point (point))
  ;; Escape "." at start of a line
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n")
  )

(defun smtp-send-data (process buffer)
  (let
      ((data-continue t)
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

      (smtp-send-data-1 process sending-data)
      )
    )
  )

(defun smtp-deduce-address-list (smtp-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
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
	  (insert-buffer-substring smtp-text-buffer
				   header-start header-end)
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (if (re-search-forward "^RESENT-TO:" header-end t)
		(setq addr-regexp
		      "^\\(RESENT-TO:\\|RESENT-CC:\\|RESENT-BCC:\\)")
	      (setq addr-regexp  "^\\(TO:\\|CC:\\|BCC:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names
			   (buffer-substring this-line this-line-end))))
	    )
	  (erase-buffer)
	  (insert-string " ")
	  (insert-string simple-address-list)
	  (insert-string "\n")
	  ;; newline --> blank
	  (subst-char-in-region (point-min) (point-max) 10 ?  t)
	  ;; comma   --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t)
	  ;; tab     --> blank
	  (subst-char-in-region (point-min) (point-max)  9 ?  t)

	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list
		    (cons (buffer-substring (match-beginning 1) (match-end 1))
			  recipient-address-list))
	      )
	    recipient-address-list)
	  )
      (kill-buffer smtp-address-buffer))
    ))

(defun smtp-do-bcc (header-end)
  "Delete BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all BCC: lines
      (while (re-search-forward "^BCC:" header-end t)
	(delete-region (match-beginning 0) (progn (forward-line 1) (point)))
	;; get rid of any continuation lines
	(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
	  (replace-match ""))
	)
      ) ;; save-excursion
    ) ;; let
  )

(provide 'smtp)

;;; smtp.el ends here
