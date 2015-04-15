;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-mode.el  thinktank

;; ---------------------------------------------------------------------------------------------------------------------------------
;; key-binding
;; ---------------------------------------------------------------------------------------------------------------------------------
;; minor-mode
(setq thinktank-minor-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-/") 'thinktank3-menu-show-tree-menu)
		(define-key map (kbd "C-?") 'thinktank3-menu-show-list-menu)
		(define-key map (kbd "C-q") '(lambda nil (interactive) (kill-buffer)))

		(define-key map (kbd "C-M-/") 'thinktank3-resource-show-web-url-list)
		(define-key map (kbd "M-/")   'thinktank3-resource-show-web-url-tree)

		(define-key map (kbd "M-P")   'org-backward-heading-same-level)
		(define-key map (kbd "M-N")   'org-forward-heading-same-level)

		(thinktank3-dnd-initialize)

		map))

;; ---------------------------------------------------------------------------------------------------------------------------------
;; button-lock
;; ---------------------------------------------------------------------------------------------------------------------------------
(require 'button-lock)
(global-button-lock-mode 1)

(defface thinktank-button-lock-face
	'((((class color) (background light)) (:foreground "darkblue"  :bold t :underline t))
		(((class color) (background dark))  (:foreground "lightblue" :bold t :underline t))) nil)

;; ---------------------------------------------------------------------------------------------------------------------------------
;; minor-mode定義
;; ---------------------------------------------------------------------------------------------------------------------------------
(define-minor-mode thinktank-minor-mode "ThinkTankマイナーモード" :lighter " TT" :global t :init-value t
	(unless (equal major-mode 'org-mode) (org-mode)	(helm-mode))

	;; xxxx-xx-xx-xxxxxx.howm::......
	;;(button-lock-set-button "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\.howm\\)\\(::[^ 　\t\n]+\\)?"
	;;												'(lambda () (interactive) (let (ext)
	;;																										(setq ext (apply 'buffer-substring (button-lock-find-extent)))
	;;																										(string-match "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\.howm\\)\\(::[^ 　\t\n]+\\)?" ext)
	;;																										(tt3-resource-show-memo :memoid (match-string 1 ext) :jump (match-string 2 ext))))
	;;												:keyboard-binding "RET"
	;;												:face 'thinktank-button-lock-face)

	;; xxxx-xx-xx-xxxxxx.howm
	(button-lock-set-button "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\.howm"
													'(lambda () (interactive) (tt3-resource-show-memo :memoid (apply 'buffer-substring (button-lock-find-extent)) t))
													:keyboard-binding "RET"
													:face 'thinktank-button-lock-face)

	;; new.howm
	(button-lock-set-button "new\\.howm"
													'(lambda () (interactive) (thinktank3-resource-create-memo-link))
													:keyboard-binding "RET"
													:face 'thinktank-button-lock-face))

;; thinktank-mode initialize
;(add-to-list 'auto-mode-alist '("\\.howm$" . (lambda ()	(thinktank-minor-mode 1))))


;; ---------------------------------------------------------------------------------------------------------------------------------
;; minor-mode定義
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


(provide 'tt3-mode)
