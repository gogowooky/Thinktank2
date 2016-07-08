;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-

(defun tt3-system-open-webrick ( &optional option ) (interactive) "
* [説明] webrickを起動する。
  [注意] win/macではscriptのあるディレクトリにバッチファイルを作成する。"
	(progn
		(setq default-directory (file-name-directory (locate-library "tt.el")))  '((locate-file))

		;; 起動中local serverがあれば立ち上げない。
		(if (string= "HTTP/1.1 200 OK " (assoc-default "Status" (tt3-system-http-request :resource :memos :action :show :id "0000-00-00-000000")))
				(message "webrick process had been working already")
			
			;; shell窓でrubyを立ち上げる
			(case (window-system)
				('w32 (defun set-shell-cmdproxy()
								(interactive)
								(setq shell-file-name "cmdproxy")
								(setq explicit-shell-file-name "cmdproxy")
								(setenv "SHELL" explicit-shell-file-name)
								(setq w32-quote-process-args t)
								(setq shell-command-switch "-c")
								)
							(set-shell-cmdproxy)
							(with-temp-file (concat default-directory "thinktank.bat")
								(insert (format "%s:\n" (substring default-directory 0 1))
												(format "cd %s\n" default-directory)
												(format "ruby thinktank.rb %s" option)))
							(start-process-shell-command "thinktank-server" nil "start cmd.exe /k thinktank.bat"))
				;;(shell-command "start cmd.exe /k thinktank.bat"))
				
				('ns  (with-temp-file (concat default-directory "thinktank.applescript")
								(insert "tell application \"Terminal\"\n"
												"do script \"echo dummy\"\n"
												(format "do script \"cd %s\" in window 1\n" default-directory)
												(format "do script \"ruby thinktank.rb %s\" in window 1\n" option)
												"end tell\n"))
							(cd default-directory)
							(shell-command (format "/usr/bin/osascript %sthinktank.applescript" default-directory)))
				
				('x (start-process-shell-command "xfce" nil (format "xfce4-terminal --working-directory='%s' -H --command='ruby thinktank.rb %s'" default-directory option)))))))

(defun thinktank3-webrick () (tt3-system-open-webrick "version2.1"))


(provide 'tt3-webrick)

