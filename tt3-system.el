;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'url)
(require 'json)
(require 'tt3-system-property) ;; thinktank-resource

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (thinktank3-format &optional param )
;;
;; (thinktank3-util-open-directory) (interactive)
;;
;; thinktank3-util-user-directories
;; thinktank3-util-current-memo-directory
;; thinktank3-util-thinktank-directory
;; thinktank3-util-memo-directory
;; thinktank3-util-user-directory
;; thinktank-directory
;;

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; thinktank3-format
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

(defun* thinktank3-format ( type &optional param ) "
* [説明] thinktank関連のID文字列を作成する。
  [引数]
    type     :memoid, :memofile, :memodir, :memopath, :memotitle, :logfile, :logpath, :tmpdir, :logpath-wc
    memoid   :now, xxxx-xx-xx-xxxxxx(を含む文字列), :home, nil
  [用例]
    (thinktank3-format :tmpdir)
    (thinktank3-format :rememoid \"0000-00-00-000000\")
    (thinktank3-format :memopath \"0000-00-00-000001.dic\")
    (thinktank3-format :logpath  \"1999-08-01-000000\") "

				(let (year month day hms memodir logdir logid tmpdir baseurl memoid)
					(setq memoid param)
					(setq tmpdir (convert-standard-filename (thinktank3-config :tempdir)))
					(case type
						(:tmpdir  tmpdir)
						(t
						 (when (setq memoid (cond ((equal   memoid :now) (format-time-string "%Y-%m-%d-%H%M%S"))
																			((assoc-default memoid (thinktank3-config :memoalias)))
																			((stringp memoid)      (and (string-match "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]" memoid)
																																	(match-string 0 memoid)))))
							 (setq year  (substring memoid 0 4))
							 (setq month (substring memoid 5 7))
							 (setq day   (substring memoid 8 10))
							 (setq hms   (substring memoid 11))
							 (case type
								 (:rememoid (format "%s\\-%s\\-%s\\-%s" year month day hms))
								 (t
									(setq memodir (convert-standard-filename (concat (thinktank3-config :memodir) (if (string-match "0000" year) (format "%s/" memoid) (format "%s/%s/%s/" year month memoid)))))
									(setq logdir  (convert-standard-filename (concat (thinktank3-config :memodir) (unless (string-match "0000" year) (format "%s/%s/" year month)))))
									(case type
										(:memoid     memoid)
										(:memofile   (concat memoid ".howm"))
										(:memodir    memodir)
										(:memopath   (concat memodir memoid ".howm"))
										(:memolink   (cons (concat memoid ".howm") (and (string-match (concat memoid "\\.howm::\\([^ 　\t]+\\)") param) (match-string 1 param))))
										(:memotitle  (with-temp-buffer (ignore-errors (insert-file-contents (concat memodir memoid ".howm"))
																																	(buffer-substring (progn (beginning-of-buffer) (point)) (progn (end-of-line) (point))))))
										(:logfile    (concat memoid ".log"))
										(:logpath    (concat logdir memoid ".log"))
										(:logpath-wc (concat logdir (format "%s-%s-%s-??????.log" year month day)))))))))))




;; ---------------------------------------------------------------------------------------------------------------------------------
;;
;; [4/6] directories
;; 
;; ---------------------------------------------------------------------------------------------------------------------------------

(defvar thinktank3-util-user-directories (thinktank3-property (concat "Directories.directory@" (upcase (system-name)))))

(defvar thinktank3-util-current-memo-directory
	'((name . "current memo directory")
		(candidates . (lambda ()
										(condition-case err
																				;(let ((bufname (thinktank3-format :memofile (buffer-name helm-current-buffer))))
																				;	(list (cons bufname (file-name-directory (thinktank3-format :memodir bufname)))))
												(let (bufname)
													(if (setq bufname (thinktank3-format :memofile (buffer-name helm-current-buffer)))
															(list (cons bufname (file-name-directory (thinktank3-format :memodir bufname))))
														(when (buffer-file-name helm-current-buffer)
															(list (cons (buffer-name helm-current-buffer) (file-name-directory (buffer-file-name helm-current-buffer)))))))
											(error '()))))
		(type . thinktank-directory)))

(defvar thinktank3-util-thinktank-directory
	'((name . "thinktank directory")
		(candidates . (lambda () (append tt3-dnd-config-env-dir (list (cons "lisp" (file-name-directory (locate-library "tt.el")))))))
		(type . thinktank-directory)))


(defvar thinktank3-util-memo-directory
	'((name . "memo directory")
		(candidates . (lambda () (loop for buf in (buffer-list)
																	 if (and (string-match "howm$" (buffer-name buf)) 
																					 (null (eql tt3-util-curmemo (buffer-name buf))))
																	 collect (cons (thinktank3-format :memoid (buffer-name buf))
																								 (thinktank3-format :memodir (buffer-name buf))))))
		(type . thinktank-directory)))

(defvar thinktank3-util-user-directory
	'((name . "user directory")
		(candidates . thinktank3-util-user-directories)
		(type . thinktank-directory)))

(defun thinktank3-util-open-directory () (interactive)
	(setq tt3-util-curmemo (buffer-name))
	(helm :sources '(thinktank3-util-current-memo-directory
									 thinktank3-util-thinktank-directory
									 thinktank3-util-memo-directory
									 thinktank3-util-user-directory
									 )))

(define-helm-type-attribute 'thinktank-directory
	'((action     ("open"  . (lambda (items) (mapc (lambda (item) (open-directory-with-os-filemanager item)) (helm-marked-candidates))))
								("shell" . (lambda (items) (mapc (lambda (item) (open-directory-with-os-shellwindow item)) (helm-marked-candidates))))
								("dired" . (lambda (items) (dired (car (helm-marked-candidates))))))
		(candidate-number-limit . 10 )))



(require 'tt3-http)


(provide 'tt3-system)

