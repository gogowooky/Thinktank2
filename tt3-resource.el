;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-user.el  thinktank
(require 'url)
(require 'helm-config)
(require 'helm)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [1/3] リソースアクセスのためのインターフェイス関数
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ruby側で必要な引値
;;

;;
;; バッファー毎にリソース元が違うような場合、ここでリソースを切り替える。
;;

																				; :show
(defun* tt3-*-open-memo ( &key memoid name ) (thinktank3-system-open-memo :memoid memoid :name name))
																				; ruby側 thinktank.get_memo( req.id )
																				; lisp側 examples
																				; (tt3-*-open-memo :memoid "0000-00-00-000002")  ;; アクセス可、結果良
																				; (tt3-*-open-memo :name "gtd-inbox")            ;; アクセス不可、結果不可

																				; :create, :update
(defun* tt3-*-save-memo ( &key memoid content verup name ) (thinktank3-system-save-memo :memoid memoid :content content :verup verup :name name))
																				; ruby側 thinktank.update_memo!( req.id, req.body ) : id=nilのとき内部でcreate_memo呼び出す
																				;        thinktank.create_memo!( req.id, req.body ) : 直接には呼び出されない
																				; lisp側 examples
																				; (tt3-*-save-memo :content "practice")
																				; (tt3-*-save-memo :memoid "0000-00-00-000004" :content "practice")

																				; :destroy
(defun* tt3-*-destroy-memo ( &key memoid ) (thinktank3-system-destroy-memo :memoid memoid ))
																				; ruby側 thinktank.delete_memo!( req.id )
																				; lisp側 examples

																				; :index
(defun* tt3-*-index-memo ( &key action body lookup    prop min max begin end keyword ) 
	(thinktank3-system-index-memo :action action :body body :prop prop :lookup lookup      :min min :max max :begin begin :end end :keyword keyword  ))
																				; ruby側 thinktank.index_object( req.lookup, req.body )
																				; lisp側 examples
																				; (tt3-*-index-memo :action :memo-initialize)  ;; アクセス可、結果良
																				; (tt3-*-index-memo :action :memo-synchronize) ;; アクセス可、結果良




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [2/3] インターフェイス関数を直接用いる一次関数
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

																				; メモを削除する
(defun tt3-resource-destroy-memo ( &rest params ) (let* ((res (eval (push 'tt3-*-destroy-memo params))))))

																				; メモを読む：　バッファーをアクティベートしない
(defun tt3-resource-open-memo ( &rest params ) (interactive)
	(let* ((memoid   (plist-get params :memoid))
				 (name     (plist-get params :name))
				 (memofile (thinktank3-format :memofile memoid)))
		(or (and memofile (get-buffer memofile))
				(let (res)
					(setq res (tt3-*-open-memo :memoid memoid :name name))
					(or (get-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
							(with-current-buffer (assoc-default "Buffer" res)
								(rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
								(thinktank-minor-mode 1)
								(current-buffer)))))))


																				;
																				; メモを表示する：　バッファーをアクティベートする
																				;
																				; * 自作popupメニューから開く (tt3-menu.el)    ex) 0000-00-00-000000.howm 専用 
																				; * helmのメニューから開く (下; open memos)
																				; * button-lockから開く (tt3-mode.el で定義)   ex) xxxx-xx-xx-xxxxxx.howm
																				; * org-linkから開く (tt3-org.elで定義)        ex) [tt:xxxx-xx-xx-xxxxxx.howm::xxxxxx]
																				;
(defun tt3-resource-show-memo ( &rest params ) (interactive)
	(let* ((memoid   (plist-get params :memoid))
				 (hilight  (plist-get params :hilight))
				 (jump     (plist-get params :jump))
				 (memofile (thinktank3-format :memofile memoid))
				 res)

		(cond ((get-buffer memofile)
					 (switch-to-buffer memofile))
					(t
					 (setq res (tt3-*-open-memo :memoid memoid))
					 (switch-to-buffer (assoc-default "Buffer" res))
					 (rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
					 (thinktank-minor-mode 1)))

		(cond (jump
					 ;;
					 ;; 位置指定ジャンプ
					 ;;
					 ;; [[tt:0000-00-00-000000.howm::255]]              :ポジション指定jump
					 ;; [[tt:0000-00-00-000000.howm::My Target]]        :検索文字列指定jump
					 ;; [[tt:0000-00-00-000000.howm::*My Target]]       :node指定jump
					 ;; [[tt:0000-00-00-000000.howm::/regexp/]]         :検索正規表現指定jump
					 ;;
					 (when (numberp jump) (setq jump (number-to-string jump)))
					 (when (string-match "^::" jump) (setq jump (substring jump 2))) ; '::' を削る

					 ;; (beginning-of-buffer)
					 (cond ((< 0 (string-to-number jump))              ; 数値
									(goto-char (+ 1 (string-to-number jump))))
								 
								 ((string-match "^/.*" jump)                 ; 正規表現
									(thinktank-highlight-word :regexp (substring jump 1 -1))
									(re-search-forward (substring jump 1 -1)))
								 
								 ((string-match "^\*.*" jump)                ; node
									(thinktank-highlight-word :regexp (concat "^\*+[ \t　]*" (substring jump 1)))
									(re-search-forward (concat "^\*+[ \t　]*" (substring jump 1 -1)))
									(beginning-of-line))
								 
								 (t                                          ; 文字列
									(thinktank-highlight-word :keyword jump)
									(or (search-forward jump nil t) (progn (goto-char (point-min)) (search-forward jump nil t)))
									(kill-new jump))))
					
					(hilight
					 (thinktank-highlight-word :keyword hilight)))

		(ignore-errors
			(redraw-display)
			(loop for view in (split-string (org-entry-get (point-min) "initial-view") "\[, \]")
						do (cond ((string= view "window:full")    (w32-send-sys-command #xf030))
										 ((string= view "window:nonfull") (w32-send-sys-command 61728))
										 ((string= view "menu:t")   (menu-bar-mode 1))
										 ((string= view "menu:nil") (menu-bar-mode 0))
										 ((string= view "tool:t")   (tool-bar-mode 1))
										 ((string= view "tool:nil") (tool-bar-mode 0))
										 ((string= view "scroll:t")   (scroll-bar-mode 1))
										 ((string= view "scroll:nil") (scroll-bar-mode 0))
										 ((string= view "folding:t")  (setq truncate-lines nil))
										 ((string= view "folding:nil") (setq truncate-lines t))))
			(redraw-display))


																				; (msgbox "%S" (macroexpand '(tt3-resource-show-memo-color-keyword)))
		(ignore-errors
			(macrolet ;; マクロである必要がある
					((tt3-resource-show-memo-fcolor-keyword () (let* ((fcolors (mapcar (lambda (x) (split-string x "[ :]"))
																																						 (split-string (org-entry-get (point-min) "fcolor-keyword") ","))))
																											 (if fcolors `(progn
																																			,@(mapcar (lambda (x) `(defface 
																																															 ,(intern (format "ttf-%s-%s" (car x) (buffer-name)))
																																															 '((t (:foreground ,(car x) :bold t :line t))) nil)) fcolors)
																																			,@(mapcar (lambda (x) `(font-lock-add-keywords nil 
																																																										 '((,(concat "\\(" (mapconcat 'regexp-quote (cdr x) "\\|") "\\)") . 
																																																												',(intern (format "ttf-%s-%s" (car x) (buffer-name))))))) fcolors)
																																			))))
					 (tt3-resource-show-memo-bcolor-keyword () (let* ((bcolors (mapcar (lambda (x) (split-string x "[ :]"))
																																						 (split-string (org-entry-get (point-min) "bcolor-keyword") ","))))
																											 (if bcolors `(progn
																																			,@(mapcar (lambda (x) `(defface 
																																															 ,(intern (format "ttb-%s-%s" (car x) (buffer-name)))
																																															 '((t (:background ,(car x)))) nil)) bcolors)
																																			,@(mapcar (lambda (x) `(font-lock-add-keywords nil 
																																																										 '((,(concat "\\(" (mapconcat 'regexp-quote (cdr x) "\\|") "\\)") . 
																																																												',(intern (format "ttb-%s-%s" (car x) (buffer-name))))))) bcolors)
																																			)))))
				
				(tt3-resource-show-memo-fcolor-keyword)
				(tt3-resource-show-memo-bcolor-keyword)))
		(font-lock-fontify-buffer)
		(current-buffer)))

																				; メモを保存する
																				;
(defun* tt3-resource-save-memo ( &key memoid content verup name show mode url-link coment) (interactive)
				(let* (res)

					(setq content (encode-coding-string content 'utf-8-unix))				 
					(setq res (tt3-*-save-memo :memoid memoid :content content :verup verup :name name))
					
					(case mode
						(:create (let ((filename (thinktank3-format :memofile (assoc-default "Filename" res))))
											 (when show 
												 (switch-to-buffer (assoc-default "Buffer" res))
												 (rename-buffer filename)
												 (thinktank-minor-mode 1)
												 (current-buffer))))

						(:feedback (let ((pos (point)))
												 (switch-to-buffer (thinktank3-format :memofile memoid))
												 (erase-buffer)
												 (insert-buffer-substring-no-properties (assoc-default "Buffer" res))
												 (goto-char pos)))

						(:append (let ((filename (thinktank3-format :memofile (assoc-default "Filename" res))) pos)
											 (when (get-buffer filename)
												 (set-buffer (thinktank3-format :memofile filename))
												 (setq pos (point))
												 (erase-buffer)
												 (insert-buffer-substring-no-properties (assoc-default "Buffer" res))
												 (goto-char pos))))
						
						(:replace (message "REPLACED")))
					
					res ))

;;
;; メモを列挙する
;;
(defun thinktank3-resource-index ( &rest plist ) "
* [説明] thinktank serverへlookupを投げ、結果を得る。
  [引数] lookup   :  検索式                                                         (vector JSON文字列)
         name     :  system memoのExtension.Queries.(name)のnodeから他paramを得る。 (文字列) (:memo-synchronize, :memo-initialize)
         input    :  ユーザー入力を得て、lookup式中の%sを置換する。                 (:select :point :clip :key :cursord :emacs :any 文字列)
         message  :  emacsでキー入力する場合のメッセージ                            (文字列)
         senddata :  サイズの大きいデータを送る場合はsenddataを介して送信する。     (:current-memo :buffered-memos :displayed-memos :nodata(def) 文字列 ファイル名)
         cache    :  基本的に結果ファイルはキャッシュされ、次回以降それを使う。     (:no :daily :once(def))
         output   :  結果をどのような形式で返すか                                   (:text :list :lisp :source :helm(def))
         attrib   :  helm表示のためのフラグ                                         (:tag :memo(def))
  [主な使用例]
　　　　 1) system memo #+BEGIN_SRC thinktank 〜 #+END_SRC を実行する。
         2) 関数で実行   ex> (thinktank3-resource-index2 :name ''Misc.MemoObject'')
            menuには関数実行形式で登録してある
  [注意] current-memoを送信した場合、結果はmemoのdirectoryに格納される。 "

	(catch 'arrest
		(let* (lookup name senddata cache message input output attrib lower upper
									(cachefile (thinktank3-config :tempdir))) ;; デフォルトはtmpdir
			
			;; 各種パラメータ取得
			(setq name     (getf plist :name))
			(setq lookup   (or (getf plist :lookup)   (thinktank3-property name :src-block)))
			(setq senddata (or (getf plist :senddata) (thinktank3-property name "senddata") :nodata))
			(setq cache    (or (getf plist :cache)    (thinktank3-property name "cache") :once))
			(setq message  (or (getf plist :message)  (thinktank3-property name "message") "input:"))
			(setq input    (or (getf plist :input)    (thinktank3-property name "input")))
			(setq output   (or (getf plist :output)   (thinktank3-property name "output") :helm))
			(setq attrib   (or (getf plist :attrib)   (thinktank3-property name "attrib") :memo))
			(setq lower    (or (getf plist :lower)    (thinktank3-property name "lower") ""))
			(setq upper    (or (getf plist :upper)    (thinktank3-property name "upper") ""))

			;; 中断処理
			(unless name (throw 'arrest "no name"))
			(case name ((:memo-synchronize :memo-initialize) 
									(tt3-*-index-memo :action name)
									(throw 'arrest (symbol-to-string name))))

			;; スイッチ類のシンボル化
			(when (stringp cache)  (setq cache  (intern cache)))
			(when (stringp output) (setq output (intern output)))
			(when (stringp attrib) (setq attrib (intern attrib)))

			;; ユーザー入力取得 ( input と senddata ) 
			(setq input (cond ((stringp input)
												 (cond ((equal ":" (substring input 0 1))
																(tt3-tt3-menu-get-input (intern input) message))
															 (t input)))
												(t nil)))
			(setq lookup (if (stringp lookup) lookup (json-encode lookup)))
			(setq lookup (replace-regexp-in-string "%input" input lookup))
			(setq lookup (replace-regexp-in-string "%upper" upper lookup))
			(setq lookup (replace-regexp-in-string "%lower" lower lookup))

			(when (stringp senddata)
				(setq senddata (cond ((file-exists-p senddata) 
															(with-temp-buffer (insert-file-contents senddata) (buffer-substring-no-properties (point-min) (point-max))))
														 ((string-match "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]" senddata)
															senddata)
														 (t
															(intern senddata)))))
			(when (symbolp senddata)
				(setq senddata (case senddata
												 (:nodata         nil)
												 (:current-memo   (let ((bufname (or (thinktank3-menu-buffer-on-useraction) (buffer-name))))
																						(setq cachefile (thinktank3-format :memodir bufname )) ;; cacheの格納先変更
																						bufname))
												 (:buffered-memos (join-string (loop for buf in (buffer-list)
																														 for name = (buffer-name buf)
																														 if (string-match "\.howm$" name)
																														 collect name)))
												 (:displayed-memos (buffer-string)))))

			;; server response保存ファイルの再作成
			(setq cachefile (concat cachefile name (if input (format "--%s--" input) "") ".lhowm" ))
			;; (popup-tip (format "cachefile:%s\nname:%s\nlookup:%s\ninput:%s\nattrib:%s\nmessage:%s\ncache:%s\nsenddata:%80s" cachefile name lookup input attrib message cache senddata)) ;; 矢印キーで表示閉じる
			(when (or (null (file-exists-p cachefile))                 ;; Cacheファイル無い場合
								(< 0 (tt3-tt3-menu-context-prefix))     ;; C-u指定ある場合
								(equal cache :no)                                ;; Cache指定noの場合
								(and (equal cache :daily)                        ;; Cache指定dailyで既存が本日作成されていない場合
										 (string< (format-time-string "%Y-%m-%d" (nth 5 (file-attributes cachefile))) (format-time-string "%Y-%m-%d")))) ;; (nth 5 (file-attributes "tt.el")) (current-time) 
				
				(with-temp-file cachefile (insert-buffer-substring (assoc-default "Buffer" (tt3-*-index-memo :lookup lookup :body senddata))))) ;; server通信＆データcache

			;; helm sourceの作成
			;; 値を返す or helm実行 
			(case output
				((:text :list :lisp) (let ((text (with-temp-buffer (insert-file-contents cachefile) (buffer-substring-no-properties (point-min) (point-max)))))
															 (case output
																 ;;(:lisp (car (read-from-string (concat "(" text ")"))))
																 (:lisp (loop for item in (split-string text "\n")
																							for valid-item = (condition-case nil (car (read-from-string item)) (error nil)) ;; captionにbackslashを含むとread-from-stringでerrorになって表示されなくなる。
																							if valid-item 
																							collect valid-item ))
																 (:list (split-string text "\n"))
																 (:text text))))
				(:no nil)
				(t (let* ((source `((name . ,(upcase name))
														(candidates-in-buffer)
														(init . (lambda () (with-current-buffer (helm-candidate-buffer 'global) (insert-file-contents ,cachefile))))
														(header-name . (lambda (x) ,name))
														(type . ,(case attrib
																			 (:memo 'thinktank-memo)
																			 (:tag 'thinktank-tag))))))
						 (case output
							 (:source source)
							 (t       (helm :sources source :buffer (if name (format "*%s*" name) "*index*"))))))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [3/3] 一次関数を用いる高次関数
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

																				; reload
(defun thinktank3-resource-reload () (interactive)
	(thinktank3-resource-index :name :memo-initialize))

																				; restruct
(defun thinktank3-resource-restruct () (interactive)
	(thinktank3-resource-index :name :memo-synchronize))

																				; replace
(defun thinktank3-resource-replace () (interactive)
	(thinktank3-resource-index :name :memo-synchronize))

																				; system-propertyを設定する
																				;(defun thinktank3-resource-update-system-property ( key value )
																				;	(with-current-buffer 
																				;		(assoc-default "Buffer" (tt3-local-maintain-memo :syskey key :value value))
																				;		(buffer-string)))
																				;
																				;'((thinktank3-resource-get-system-property "OAuth2.GoogleCalendar.not-requred"))
																				;'((thinktank3-resource-update-system-property "OAuth2.GoogleCalendar.not-requred" "100"))


																				; トップメモを開く
(defun thinktank3-resource-show-top-memo () (interactive) (tt3-resource-show-memo :memoid "0000-00-00-000000"))


																				; メモを削除
(defun thinktank3-resource-destroy-memo nil (interactive)
	(tt3-resource-destroy-memo :memoid (thinktank3-format :memoid (buffer-name))) (kill-buffer))


;;
;; 一次関数: tt3-resource-save-memo
;;
																				; メモを保存
(defadvice save-buffer (around tt3-io-save-memo activate) 
	;; createの場合 (org-capture) 内部で使用されるsave-bufferからcallされる。
	(cond ((and (string-match "howm$" (buffer-name)) (null (buffer-file-name)))
				 (message "saving howm file")
				 (let ((updatep (if (file-exists-p (thinktank3-format :memopath (buffer-name))) :replace :create)))
					 (tt3-resource-save-memo :memoid (thinktank3-format :memoid (buffer-name))
																	 :content (buffer-string)
																	 :mode    updatep)))
				(t ad-do-it)))

'((ad-disable-advice 'save-buffer 'around 'tt3-io-save-memo))

;; 新規メモを選択範囲で作成・保存
(defun thinktank3-resource-create-memo-from-region () (interactive) 
	(when (use-region-p) (let ((res (tt3-resource-save-memo :content (buffer-substring (region-beginning) (region-end)) :mode :create)))
												 (switch-to-buffer (assoc-default "Buffer" res))
												 (rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
												 (thinktank-minor-mode 1)
												 (goto-char (point-min)))))

;; 新規メモを作成・保存
(defun thinktank3-resource-create-memo () (interactive)
	(let* ((template-memo    (replace-regexp-in-string "\\\\n" "\n" (thinktank3-config :template-memo)))
				 (template-oneline (thinktank3-config :template-oneline))
				 (res (tt3-resource-save-memo :content (format-time-string template-memo) :mode :create)))

		(switch-to-buffer (assoc-default "Buffer" res))
		(rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
		(thinktank-minor-mode 1)
		(goto-char (point-min))
		(search-forward "?") (backward-char) (delete-char 1)
		))

;; 新規メモを作成・保存 (new.howm)
(defun thinktank3-resource-create-memo-link () (interactive)
	(let ((created (thinktank3-format :memofile :now)))
		(narrow-to-region (progn (end-of-line) (point)) (progn (beginning-of-line) (point)))
		(when (re-search-forward "new\.howm") (replace-match created))
		(widen)
		(save-buffer)
		(tt3-resource-save-memo :memoid created
														:content (format "no content\n%s" created)
														:mode :create
														:show t)))

;; 現メモを保存
(defun thinktank3-resource-update-memo () (interactive)
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name)) 
													:content (buffer-string)
													:mode    :replace))

;; 現メモをマイナー version up
(defun thinktank3-resource-major-version-up () (interactive) 
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name))
													:content (buffer-string)
													:verup   :major
													:mode    :feedback))

;; 現メモをメジャー version up
(defun thinktank3-resource-minor-version-up () (interactive)
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name))
													:content (buffer-string) 
													:verup   :minor
													:mode    :feedback))


;;
;;
;;
(defun tt3-resource-append-oneline ( &rest params )
	(let* (comm supl
							(memoid (thinktank3-format :memoid (plist-get params :memoid)))
							(name   (thinktank3-format :name   (plist-get params :name)))
							template plist)
		(with-current-buffer (tt3-resource-open-memo :memoid memoid :name name) ;; :name name
			(setq template (format-time-string (org-entry-get (point-min) "thinktank-template")))
			(setq plist (loop for item in (split-string-and-unquote (org-entry-get (point-min) "thinktank-memo") ",")
												collect (split-string (trim-string item) ":")))

			(setq name (car (assoc-default "name" plist))) ;; 名前
			(setq comm (car (assoc-default "comm" plist))) ;; 入力時のメッセージ
			(setq supl (car (assoc-default "supl" plist))) ;; 追加情報
			(setq template (replace-regexp-in-string "%comm%" (read-string (or comm (format "comment for %s:" name))) template))
			(setq template (replace-regexp-in-string "%supl%" (cond ((equal supl "url")      (thinktank3-mozrepl-get-current-page-link))
																															((equal supl "clip")     (clipboarded-string))
																															((equal supl "calendar") (format "<%s>" (org-read-date nil nil nil nil
																																																										 (thinktank3-menu-calendar-on-useraction)
																																																										 "")))
																															(t ""))
																							 template))
			(beginning-of-buffer)
			(re-search-forward "^$")
			(insert (format "\n%s" (replace-regexp-in-string "\\\\n" "\n" template)))
			(thinktank3-resource-update-memo))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm型の定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
;; thinktank-memo型
;;--------------------------------------------------------------------------------------------------------------------------------------------
(define-helm-type-attribute 'thinktank-memo
	'((action ("O|Open Memos"    . (lambda (x) (loop for item in (helm-marked-candidates)
																									 for ( memoid . jump ) = (thinktank3-format :memolink item)
																									 do  (tt3-resource-show-memo :memoid memoid :jump jump))))
						("M|Squeeze More"  . (lambda (x) (thinktank3-resource-index :name "Search" :senddata (mapconcat 'identity (helm-marked-candidates) "\n"))))
						("L|insert Lines"  . (lambda (x) (mapc (lambda (item) (insert (thinktank3-format :memofile item) "\n")) (helm-marked-candidates))))
						("I|Insert Ids"    . (lambda (x) (mapc (lambda (item) (insert (thinktank3-format :memofile item) "\n")) (helm-marked-candidates))))
						("D|Delete Memos"  . (lambda (x) (mapc (lambda (item) (tt3-resource-destroy-memo :memoid (thinktank3-format :memoid item))) (helm-marked-candidates))))
						("F|File Manager"  . (lambda (x) (mapc (lambda (item) (open-directory-with-os-filemanager (thinktank3-format :memodir item))) (helm-marked-candidates))))
						("S|Shell"         . (lambda (x) (mapc (lambda (item) (open-directory-with-os-shellwindow (thinktank3-format :memodir item))) (helm-marked-candidates))))
						("H|Html"          . (lambda (x) (loop for item in (helm-marked-candidates)
																									 for ( memoid . jump ) = (thinktank3-format :memolink item)
																									 do  (thinktank3-mozrepl-browse-memo memoid))))
						("E|diered"        . (lambda (x) (dired (thinktank3-format :memodir (car (helm-marked-candidates))))))
						)
		(action-transformer . (lambda ( actions candidate )
														(cond ((string-match "type:oneline" candidate) (cons '("R|Record One Line" . (lambda (x) (tt3-resource-append-oneline :memoid x) t)) actions)) ;; 一行メモの場合のaction
																	(t actions))))
		(candidate-number-limit . 1000 )))

(defun tt3-resource-thinktank-memo-action-transformer ( actions candidate )
	(cond ((string-match "type:oneline" candidate) (cons '("R|Record One Line" . (lambda (x) (tt3-resource-append-oneline :memoid x) t)) actions)) ;; 一行メモの場合のaction
				(t actions)))

;;--------------------------------------------------------------------------------------------------------------------------------------------
;; thinktank-tag型
;;--------------------------------------------------------------------------------------------------------------------------------------------
(define-helm-type-attribute 'thinktank-tag
	'((filtered-candidate-transformer . (lambda ( cands src )
																				(let* ((tagfmts '(("TODOTAG" "%s")
																													("ORGTAG" ":%s:")
																													("TITLETAG" "[%s]")
																													("PRIORITY" "#%s")))
																							 (fmt      (car (assoc-default (assoc-default 'name src) tagfmts))))
																					(loop for cand in cands
																								collect (destructuring-bind (key val)	(split-string cand ",")
																													(cons (format (concat fmt "(%s)") key val) (format fmt key)))))))
		(action ("L|insert"     . (lambda (x) (mapc (lambda (item) (insert item)) (helm-marked-candidates))))
						("F|find memos" . (lambda (x) (let* ((keyword (car (helm-marked-candidates))))
																						(helm :sources thinktank3-resource-index-search-result :buffer "*search-keyword*")))))
		(candidate-number-limit . 1000 )))


(provide 'tt3-resource)
