;; tt-system-extended.l; -*- mode: emacs-lisp; coding: utf-8 -*-

;; ---------------------------------------------------------------------------------------------------------------------------------
;;
;; directories
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

(defun tt:util-open-directory () (interactive)
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





(provide 'tt3-system-extended)

