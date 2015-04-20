;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'url)
(require 'json)

(require 'tt3-system-property)

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; Thinktankシステム値関数
;;
;; [3/6] thinktank3-format
;; [4/6] directories
;; [5/6] リソースへのアクセス:  http-request, (thinktank3-system-*) save, open, destroy, index
;; [6/6] webrickの起動 
;;
;; property と config は、emacs lip側, ruby側ともに 相互に依存せずに動作するように実装している。
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [3/6] thinktank3-format
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







;; ---------------------------------------------------------------------------------------------------------------------------------
;;
;; [5/6] リソースへのアクセス
;;
;; ---------------------------------------------------------------------------------------------------------------------------------

;;
;; open, save, destroy, index
;;
																				; メモを開く
(defun* thinktank3-system-open-memo ( &key memoid name )
	(cond (memoid (tt3-system-http-request :resource :memos
																				 :action   :show
																				 :id       (thinktank3-format :memoid memoid)))
				(name   (tt3-system-http-request :resource :memos
																				 :action   :index
																				 :query    `((:lookup . [(:AND "object" :type "thinktankproperty")
																																 (:AND "search" :target "name" :text ,name)
																																 (:RESPONSE "first" :type "memo")]))))))

																				; メモを保存する
(defun* thinktank3-system-save-memo ( &key memoid content verup name )
	(tt3-system-http-request :resource :memos
													 :action   :update
													 :id       (or (thinktank3-format :memoid memoid)
																				 (thinktank3-format :memoid :now))
													 :body     (cond ((stringp content) content)
																					 ((get-buffer content) (with-current-buffer content (buffer-string))))
													 :query    `((:optional . (:verup ,verup :name ,name)))))


 																				; メモを削除する
(defun* thinktank3-system-destroy-memo ( &key memoid )
	(tt3-system-http-request :resource :memos
													 :action   :destroy
													 :id       memoid ))

																				; メモを列挙する
(defun* thinktank3-system-index-memo ( &key action body lookup     prop min max begin end keyword )
	(setq keyword (url-hexify-string (encode-coding-string (or keyword "") 'utf-8-unix)))
	(setq lookup (or lookup (case action
														(:calfw-timed-text  `[(:AND "object" :type "linetimetag")
																									(:AND "string" :prop "id" :min ,begin :max ,end)
																									(:RESPONSE "list" :type "linetimetag" :sort "id" :order "asc" 
																														 :display ":date \"[id]\" :link \"[memo.id].howm\" :jump \"[point]\" :caption \"[contents]\"")])
														(t nil))))
	(setq body (or body (case action 
												(:memo-initialize  "initialize-memo")
												(:memo-synchronize "synchronize-memo")
												(t ""))))
								 
	(when (stringp lookup) (setq lookup (json-read-from-string lookup)))

	(tt3-system-http-request
	 :resource :memos
	 :action   :index
	 :body     body
	 :query    `((:lookup . ,lookup) (:optional . ()))))


'((tt3-system-http-request :resource :memos :action :index :query `((:lookup . ((:query . [ "[ThinktankChapter]" ] )
																																								(:list . "(memo.id).howm | (memo.title,%-30s) | memo:(title,%-30s)")
																																								(:limit . "30") (:offset . "0")
																																								(:sort . "address") (:order . "dsc")
																																								))))
	(thinktank3-system-index-memo :lookup '((:query . [ "[ThinktankChapter]" ] )
																					(:list . "(memo.id).howm | (memo.title,%-30s) | memo:(title,%-30s)")
																					(:limit . "30") (:offset . "0") (:sort . "address") (:order . "dsc")))

	(thinktank3-system-index-memo :lookup '((:query . [ "[ThinktankMemo]" "content =~ orexin" ] )
																					(:list . "(id).howm | (weekday) | (title,%-30s)")
																					(:limit . "30")	(:offset . "0")	(:sort . "id") (:order . "dsc")))


	(thinktank3-system-index-memo :action :memo-initialize)
	(thinktank3-system-index-memo :action :memo-synchronize)
	)

;;
;; webrick access
;;

(defvar tt3-http-response-buffer "*http-res*")

(defun* tt3-system-http-request ( &key resource action id query body ) "
* [説明] webrick serverにアクセスする。
  [引数] resource :  :memos
         action   :  :index, :create, :new, :edit, :show, :update, :destroy, (:maintain, :analyze)
         id       :  \"xxxx-xx-xx-xxxxxx\"
         query    :  alist
         body     :  メモテキストやIDリスト等の参照用テキストデータ
  [返値] alistの形で response header が返る(下記)
         buffer(tt3-http-response-buffer)に response body が返る。
  [注意] win/macではscriptのあるディレクトリにバッチファイルを作成する。

	;;
	;; ((\"Buffer\" . #<buffer *http-res*>)
	;;  (\"Status\" . \"HTTP/1.1 200 OK \")
	;;  (\"Filename\" . \"E:/Dropbox/MyData/tt/0000-00-00-000000/0000-00-00-000000.howm\")
	;;  (\"Content-Type\" . \"text/howm;charset=utf-8\")
	;;  (\"Server\" . \"WEBrick/1.3.1 (Ruby/2.0.0/2013-05-14)\")
	;;  (\"Date\" . \"Wed, 23 Apr 2014 07:04:55 GMT\")
	;;  (\"Content-Length\" . \"5749\")
	;;  (\"Connection\" . \"Keep-Alive\"))
	;; "
				
				(let* (url-request-data url-request-extra-headers tmpurl params)
					;;
					;; resource, action, id, query から url-request-method, url を得る
					;;
					;; (setq tmpurl (concat (thinktank3-config resource) (if id (concat "/" id) "")))  ;; (thinktank3-config :memos) ;; "http://127.0.0.1:20080/thinktank/memo" "/" "0000-00-00-00000"
					(setq tmpurl (concat (thinktank3-config :baseurl)
															 (symbol-to-string resource)
															 (if id (concat "/" id) "")))

					;; for test
					(setq tmpurl (replace-regexp-in-string "20080" "20090" tmpurl))

					(setq params (join-string (loop for ( key . val ) in query                      ;; lookup=....&optional=....
																					collect (format "%s=%s"
																													(url-hexify-string (cond ((symbolp key) (symbol-to-string key))
																																									 (t key)))
																													(url-hexify-string (cond ((vectorp val) (json-encode val))
																																									 ((listp val)   (json-encode val))
																																									 ((numberp val) (number-to-string val))
																																									 ((symbolp val) (symbol-name val))
																																									 (t val))))
																					) "&" ))
					
					;;
					;; url
					;; url-request-method
					;; url-request-data
					;; url-request-extra-headers
					;;
					;; 以上を設定して、(url-retrieve-synchronously url) を実行するとbufferにresponseが返る。
					;;
					(destructuring-bind ( url-request-method . url )
							(case action
								;; (:new     (cons "GET"    (concat tmpurl "/new.howm"  "?" params )))
								;; (:edit    (cons "GET"    (concat tmpurl "/edit.howm" "?" params )))
								(:create  (cons "POST"   (concat tmpurl ".howm"      "?" params )))
								(:show    (cons "GET"    (concat tmpurl ".howm"      "?" params )))
								(:update  (cons "PUT"    (concat tmpurl ".howm"      "?" params ))) ;; createもupdateで処理してる?
								(:destroy (cons "DELETE" (concat tmpurl ".howm"      "?" params )))
								(:index   (cons "GET" (concat tmpurl "/index.howm"      "?" params )))
								)
						(setq url-request-data           (url-hexify-string (encode-coding-string (or body "") 'utf-8-unix)))
						(setq url-request-extra-headers `(("Content-Type"   . "text/howm")
																							("Content-Length" . ,(format "%d" (+ 2 (length url-request-data)))))) ;; url packageが "\r\n" を追加しているので +2 している。
						
						(save-excursion (condition-case nil
																(let (response response-header)
																	(when (get-buffer tt3-http-response-buffer) (kill-buffer tt3-http-response-buffer))
																	(set-buffer (generate-new-buffer tt3-http-response-buffer))
																	(insert (decode-coding-string 
																					 (with-current-buffer (url-retrieve-synchronously url) (buffer-string)) ;; ← http通信でテキストを得る
																					 'utf-8-unix))
																	
																	;; header部 を切り出しalist化し、response-headerとして返す。　バッファー(tt3-http-response-buffer)にはbody部が返る。
																	(goto-char (point-min)) (re-search-forward "\n\n") ;; 最初の改行x2がheader-body境界
																	(setq response-header (split-string (buffer-substring (point) (point-min)) "\n"))
																	(delete-region (point) (point-min))
																	(setq response (cons (cons "Buffer" (current-buffer))                           ;; ("Buffer" . "buffer-name")
																											 (loop for lin in response-header
																														 if (string-match "\\(: \\|HTTP\\)" lin)
																														 collect (let ((tmp (split-string lin "\\(: \\)")))
																																			 (case (length tmp)
																																				 (1 (cons "Status" (car tmp)))            ;; ("Status" . "" )
																																				 (2 (cons (car tmp) (cadr tmp))))))))
																	response)
															(error "error" '(("Status" . "error"))))))))






;; ---------------------------------------------------------------------------------------------------------------------------------
;;
;; [6/6] webrick起動：　　　　　　　　shell script を作成しそれでwebrickを非同期起動する。　既起動は無為
;; 
;; ---------------------------------------------------------------------------------------------------------------------------------
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
							(start-process-shell-command "thinktank-server" nil "osascript" "thinktank.applescript"))

				
				('x (start-process-shell-command "xfce" nil (format "xfce4-terminal --working-directory='%s' -H --command='ruby thinktank.rb %s'" default-directory option)))))))

(defun thinktank3-webrick () (tt3-system-open-webrick "version2.1"))

(thinktank3-webrick)
;; QSyncのためか、ディレクトリ内に同名のファイルが存在することが発生する。

(provide 'tt3-system)
