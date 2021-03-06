;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-mode.el  thinktank


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 

;; ---------------------------------------------------------------------------------------------------------------------------------
;; key-binding
;; ---------------------------------------------------------------------------------------------------------------------------------
;; minor-mode
(setq thinktank-minor-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-/") 'tt:menu-show-tree-menu)
		(define-key map (kbd "C-?") 'tt:menu-show-list-menu)
		(define-key map (kbd "C-q") '(lambda nil (interactive) (kill-buffer)))
		(define-key map (kbd "C-M-/") 'tt:resource-show-web-url-list)
		(define-key map (kbd "M-/")   'tt:resource-show-web-url-tree)
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
	'((car (thing-at-point 'word)))


(define-minor-mode thinktank-minor-mode "ThinkTankマイナーモード" :lighter " TT" :global t :init-value t
	(unless (equal major-mode 'org-mode) (org-mode)	(helm-mode))
	(setq frame-title-format (format "emacs@%s -- TT3 <%%b>" (system-name)))


	;; xxxx-xx-xx-xxxxxx.howm
	(button-lock-set-button "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\.howm"
													'(lambda () (interactive) (tt3-resource-show-memo :memoid (apply 'buffer-substring (button-lock-find-extent)) t))
													:keyboard-binding "RET"
													:face 'thinktank-button-lock-face)

	;; new.howm
	(button-lock-set-button "new\\.howm"
													'(lambda () (interactive) (tt:resource-create-memo-link))
													:keyboard-binding "RET"
													:face 'thinktank-button-lock-face))



(provide 'tt3-mode)
