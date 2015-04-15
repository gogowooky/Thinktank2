;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt.el  thinktank
(setq debug-on-error t)
(require 'cl)
(require 'em-glob)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tt-util)        ;; elisp, emacs
(require 'tt3-system)     ;; thinktank-resource
(require 'tt3-resource)   ;; thinktank-resource
(require 'tt3-calfw)      ;; emacs, thinktank-resource
(require 'tt3-oauth2)     ;; emacs, thinktank-resource
(require 'tt3-dnd)        ;; emacs, thinktank-system
(require 'tt3-org)        ;; emacs, thinktank-system
(require 'tt3-mozrepl)    ;; emacs, thinktank-system
(require 'tt3-menu)       ;; thinktank-system
(require 'tt3-mode)       ;; thinktank-system
(thinktank3-menu-initialize)

;; to monitor thinktank.log

(add-hook 'find-file-hook (lambda () (when (string-match "log$" (buffer-name)) (auto-revert-tail-mode t))))
(add-hook 'after-revert-hook (lambda () (when auto-revert-tail-mode (end-of-buffer))))

(provide 'tt)




