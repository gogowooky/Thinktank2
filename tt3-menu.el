;;  tt-menu.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'cl)

(require 'helm-config)
(require 'helm)

(require 'popwin)

;;
;; [1/6] 固定メニュー
;; [2/6] menuの初期化
;; [3/6] メニューデータ整形
;; [4/6] helm自動選択
;; [5/6] コンテキスト記録
;; [6/6] メニュー表示関数
;;

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [1/6] 固定メニュー
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defvar thinktank3-menu-string-list nil)
(setq thinktank3-menu-default-string-list 
			'(( "M|Memo C|Clipboard I|Id" :context "ext:howm" :func (push-string-to-clipboard (thinktank3-format :memofile (buffer-name))) :help "メモのIDをコピーする")
				( "M|Memo C|Clipboard T|Title" :context "ext:howm" :func (save-excursion (goto-char (point-min)) (push-string-to-clipboard (current-line-string))) :help "メモのtitleをコピーする")
				( "M|Memo C|Clipboard F|Filepath" :context "ext:howm"	:func (push-string-to-clipboard (thinktank3-format :memopath (buffer-name))) :help "メモのfilepathをコピーする")
				( "M|Memo D|Delete" :context "ext:howm" :command thinktank3-resource-destroy-memo :help "メモを削除")
				( "M|Memo V|Version M|Minor-up" :context "ext:howm" :command thinktank3-resource-minor-version-up :help "メモをマイナーバージョンアップ" )
				( "M|Memo V|Version J|Major-up" :context "ext:howm" :command thinktank3-resource-major-version-up :help "メモをメジャーバージョンアップ" )
				( "M|Memo B|Browse" :context "ext:howm" :command thinktank3-mozrepl-browse-memo :help "メモをbrowse")
				( "M|Memo W|winword" :context "ext:howm" :command thinktank3-mozrepl-memo-docx :help "wordファイルを表示")
				( "M|Memo M|Home" :command thinktank3-resource-show-top-memo :help "トップページ" )                              ;;;;;;;; tt3 OK
				( "M|Memo N|New" :command thinktank3-resource-create-memo :help "新しいメモを作成" )      
				( "M|Memo S|Save" :command thinktank3-resource-update-memo :help "メモを保存" )    
				( "M|Memo X|Close" :command kill-buffer :help "メモを閉じる")
				( "M|Memo R|Save-Region" :command thinktank3-resource-create-memo-from-region :help "選択領域を新規memoとして保存する")
				
				( "C|Calendar C|Show" :command thinktank3-calfw-show :help "カレンダーを開く")
				( "C|Calendar G|SelectGroup" :command thinktank3-calfw-show-group :help "カレンダーグループを開く")
				( "C|Calendar O|ShowOne" :command thinktank3-calfw-show-one :help "カレンダーを一種類のみ開く")
				( "C|Calendar U|Update" :command thinktank3-calfw-update :help "カレンダーグループを選択して更新する")
				( "C|Calendar S|SetCurrent" :command thinktank3-calfw-select :help "表示カレンダーを選択する")

				( "U|Usability F|Footnote O|Open-file" :command thinktank3-org-open-footnote-file :help "footnote末尾のファイルを開く" )
				( "U|Usability F|Footnote P|Open-pubmed" :command thinktank3-org-open-footnote-site :help "fornoteの引用論文をpubmedで検索" )
				( "U|Usability E|Emacs H|Helm-imenu" :command helm-imenu :help "helmでimenu")
				( "U|Usability E|Emacs R|Helm-regexp" :command helm-regexp :help "helmでregexp")
				( "U|Usability E|Emacs I|Isearch-occur" :command isearch-occur :help "検索")
				( "U|Usability E|Emacs R|Regexp-Builder" :command regexp-builder :help "Regexp Builder")
				( "U|Usability U|View A|Set-Alpha" :command thinktank-set-alpha :help "透過度を設定する")
				( "U|Usability U|View T|Toggle-Toolbar" :command thinktank-toggle-toolbar :help "toolbar表示をトグル" )
				( "U|Usability U|View M|Toggle-Menubar" :command thinktank-toggle-menubar :help "menubar表示をトグル" )
				( "U|Usability U|View S|Toggle-Scrollbar" :command thinktank-toggle-scrollbar :help "scrollbarをトグル" )
				( "U|Usability U|View F|Toggle-Fullscreen" :command thinktank-toggle-full :help "fullscreenをトグル" )
				( "U|Usability U|View X|Toggle-Maximize" :command thinktank-toggle-expand :help "最大化をトグル" )
				( "U|Usability U|View B|Text-Bending" :command thinktank-truncate-lines :help "折り畳みをトグル" )
				( "U|Usability U|View I|Indent" :command thinktank-indent-all :help "インデントをそろえる" )
				( "U|Usability U|View H|Word-Hilight-on" :command thinktank-highlight-word :help "ハイライトする" )
				( "U|Usability U|View O|Word-Hilight-off" :command thinktank-unhilight :help "ハイライトを止める")	

				( "I|Insert N|Node D|Date" :command thinktank3-org-insert-new-node :help "新しいtimestamp付きの節を挿入")
				( "I|Insert N|Node F|Firefox" :command thinktank3-mozrepl-insert-current-page-node :help "firefoxのcurrent pageへのlinkを挿入" )
				( "I|Insert D|Date" :command thinktank3-org-insert-current-time :help "timestampを挿入")
				( "I|Insert F|Firefox" :command thinktank3-mozrepl-insert-current-page-link :help "firefoxのcurrent pageへのlinkを挿入" )

				( "S|System W|Restart-webrick" :command thinktank3-system-run-webrick3 :help "local webrick serverを起動する" )
				( "S|System D|Directory" :command thinktank3-util-open-directory :help "ディレクトリを開く" )
				( "S|System L|Reload-memo" :command thinktank3-resource-reload :help "メモをリロードする" )
				( "S|System S|Restruct-memo" :command thinktank3-resource-restruct :help "メモをリロードする" )
				( "S|System P|Replace-memo" :command thinktank3-resource-replace :help "メモをリロードする" )
				( "S|System O|Object" :command thinktank3-resource-show-property-object :help "objectの数を数える" )
				( "S|System A|AutoSelect" :command thinktank3-menu-select-one-automatically :help "helmで１つに絞り込んだら自動選択")
				( "S|System M|Menu-Initialize" :command thinktank3-menu-initialize :help "menuを更新する" )

				( "W|WebSearch R|Ruby M|Rurima" :command thinktank3-mozrepl-rurema-search :help "るりまさーち")
				( "W|WebSearch R|Ruby R|RubyRef" :command thinktank3-mozrepl-rubyref-search :help "Rubyリファレンス" )
				
				( "O|OtherMenu L|UrlList" :func (tt3-menu-show-weburl :list) :help "URLリスト" )
				( "O|OtherMenu T|UrlList" :func (tt3-menu-show-weburl :tree) :help "URLリスト" )

				( "h|Help S|Search W|word" :command thinktank3-search-forward-focused-string :help "キーワードジャンプ")
				( "h|Help S|Search L|Elisp" :helm helm-source-info-elisp :help "elisp検索")
				( "h|Help S|Search E|Emacs" :helm helm-source-info-emacs :help "emacs検索")
				( "h|Help S|Search I|Info" :helm helm-source-info-info :help "info検索")
				( "h|Help S|Search C|Cl" :helm helm-source-info-cl :help "common lisp検索")
				( "h|Help S|Search O|Org" :helm helm-source-info-org :help "org検索")
				( "h|Help D|Document H|Help" :command help :help "ヘルプ")
				( "h|Help D|Document U|Url" :info url :help "urlのマニュアル")
				( "h|Help D|Document S|Eshell" :info eshell :help "eshellのマニュアル")
				( "h|Help D|Document O|Orgmode" :info org :help "orgのマニュアル")
				( "h|Help D|Document C|Cl" :info cl :help "clのマニュアル")
				( "h|Help E|Emacs" :info emacs :help "emacsのマニュアル")
				( "h|Help L|Elisp" :info elisp :help "elispのマニュアル")
				( "h|Help T|Test 1|Test1" :func (msgbox "%s" (format-time-string "%Y-%m-%d %H:%M")) :help "テスト1")
				( "h|Help T|Test 2|Test2" :func (msgbox "%s" input) :input :key :help "テスト2")
				( "h|Help T|Test F|File" :func (tt3-menu-show-keymap-menu menu-bar-files-menu :list) :help "ファイルメニュー" )
				( "h|Help T|Test O|Options" :func (tt3-menu-show-keymap-menu menu-bar-options-menu :list) :help "オプションメニュー" )
				( "h|Help T|Test M|Manuals" :func (tt3-menu-show-keymap-menu menu-bar-manuals-menu :list) :help "マニュアルメニュー" )
				( "h|Help C|Context" :func (msgbox "context:%S" tt3-menu-context) :help "contextを表示")
				))




;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [2/6] menuの初期化
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun thinktank3-menu-initialize () (interactive) "   (thinktank3-menu-initialize)
* [説明] 各所で設定されているメニュー設定値を収集し、thinktank menuを再構成する
  [注意] thinktank起動の最後(tt.elの最後)に一度実行している。 "
	(let* ()
		(thinktank3-property :reset)

		;; デフォルトメニューの登録
		(setq thinktank3-menu-string-list thinktank3-menu-default-string-list)

		;; web search engine の登録
		(mapcar-tt3-property-subnode "Menu.WebSearch"
																 (let ((menu (org-entry-get nil "menu"))(url (org-entry-get nil "url")) (help (org-entry-get nil "help")))
																	 (when menu
																		 (when (string-match "\\[\\[\\([^\]]+\\)\\]\\[\\([^\]]+\\)?\\]\\]" url)
																			 (setq help (concat (when help (concat help ": ")) (match-string 2 url)))
																			 (setq url  (match-string 1 url)))
																		 (push `(,menu
																						 :url     ,url
																						 :help    ,help
																						 :input   ,(intern (org-entry-get nil "input"))
																						 :context ,(org-entry-get nil "context")
																						 :message ,(org-entry-get nil "message"))
																					 thinktank3-menu-string-list))))
		
		;; WebSearchEnginesをorg-link-abbrev-alistに登録　文中 [[google:orexin leptin]] で検索できるようになる。
		(setq org-link-abbrev-alist nil)
		(mapcar-tt3-property-subnode "Menu.WebSearch" 
																 (when (org-entry-get nil "orgtag")
																	 (push (cons (org-entry-get nil "orgtag") (replace-regexp-in-string "%input" "%s" (org-entry-get nil "url"))) org-link-abbrev-alist)))

		;; Query 関連の登録
		(mapcar-tt3-property-subnode "Menu.Query"
																 (let* ((menu (org-entry-get nil "menu"))
																				(help (org-entry-get nil "help")))
																	 (when menu (push `(,menu
																											:func    (thinktank3-resource-index :name ,title)
																											:help    ,help
																											:context ,(org-entry-get nil "context"))
																										thinktank3-menu-string-list))))
		;; MozRepl 関連の登録
		(mapcar-tt3-property-subnode "Menu.Mozrepl"
																 (let* ((menu (org-entry-get nil "menu"))
																				(help (org-entry-get nil "help"))
																				(code (trim-string (tt3-tt3-property-get-element :src-block))))
																	 (when menu (push `(,menu
																											:func    (tt3-mozrepl-request ,code)
																											:help    ,help)
																										thinktank3-menu-string-list))))
		;; 一行メモの関数定義＆登録
		;; エラーでる。
		'(loop for ( memoid name comm supl menu ) in (thinktank3-resource-index :name "Menu.Query.AssociateFile.oneline" :output :lisp)
					 unless (equal menu "")
					do (push `(,menu :func (tt3-resource-append-oneline :memoid ,memoid :name ,name) :help ,(concat "一行メモ:" name)) thinktank3-menu-string-list))
		
		))


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [3/6] メニューデータ整形
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun thinktank3-menu-show-tree-menu ( &optional arg )        (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu thinktank3-menu-string-list :tree)))
(defun thinktank3-menu-show-list-menu ( &optional arg )        (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu thinktank3-menu-string-list :list)))
(defun thinktank3-menu-show-popup-menu ( &optional arg )       (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu thinktank3-menu-string-list :popup)))
(defun thinktank3-resource-show-web-url-list ( &optional arg ) (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-weburl :list)))
(defun thinktank3-resource-show-web-url-tree ( &optional arg ) (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-weburl :tree)))

;; string-list形式メニューをtree/listで表示する
(defun tt3-menu-show-stringlist-menu ( string-list menu-type &optional input ) "
* [説明] strlig-list menuをtree/list/popup表示する
  [引数] string-list  : keymap
         menu-type    : :tree | :list | :popup 
         arg          : C-u 状態を植えとる "
	
	(flet ((do-action ( plist ) ;; stringlistのコマンドを実行
										(catch 'input-error (let* (;; ↓コマンド取得
																							 (cmd (getf plist :command))
																							 (hlm (getf plist :helm))
																							 (inf (getf plist :info))
																							 (url (getf plist :url))
																							 (fnc (getf plist :func))
																							 ;; ↓ユーザー入力処理
																							 (input (tt3-tt3-menu-get-input (getf plist :input) (getf plist :message))))
																					;; ↓コマンドの実行
																					(cond (cmd (funcall cmd))
																								(hlm (helm :sources hlm :buffer "*submenu*"))
																								(inf (info (symbol-name inf)))
																								(url (browse-url (replace-regexp-in-string "%input" (url-hexify-string input) url)))
																								(fnc (eval fnc)))))))
		
		(case menu-type
			(:tree (let* ((res (tt3-popwin-show-menu (tt3-menu-stringlist-to-tree string-list))))
							 (cond ((consp res) (do-action res)) ;; 通常実行
										 ((and (stringp res) (string-match "tohelm:\\(.*\\) [^ ]+$" res)) (tt3-menu-show-stringlist-menu string-list :list (match-string 1 res)))))) ;; helmに移行

			(:list (helm :sources '((name . "thinktank action")
															(candidates . string-list)
															(candidate-transformer .  tt3-menu-stringlist-to-helm-candidates)
															(action ("D|DoAction"  . (lambda (x) (do-action (cdr x))))
																			("S|show" . (lambda (x) (msgbox "%S" (cdr x))))
																			("I|Insert" . (lambda (x) (insert (format "%S" (cdr x))))))
															)
									 :input input))
			(:popup (flet ((tree-to-keymapmenu (tree)
																				 (if (cddr tree)
																						 `(,(intern (car tree)) menu-item ,(car tree) ,(or (getf tree :command) 'test))
																					 `(,(intern (car tree)) menu-item ,(car tree) 
																						 (keymap ,(car tree) ,@(loop for branch in (cadr tree) collect (tree-to-keymapmenu branch)))))))
								(let ((tree (tt3-menu-stringlist-to-tree string-list)))
									(popup-menu (cadddr (tree-to-keymapmenu (list "TT" tree))))))))))


;; urlをtree/listで表示する
(defun tt3-menu-show-weburl ( menu-type ) "
* [説明] thinktank memo内のurlを列挙表示する
  [引数] menu-type  : :tree | :list | :popup
         arg        : C-u 状態を得る "

	(case menu-type
		(:list (let* ((source (thinktank3-resource-index :name "Extension.Queries.Misc.WebUrlList" :output :list)))
						 (helm :sources `((name . "url")
															(candidates . ,source)
															(candidate-number-limit . 1000 )
															(candidate-transformer  . (lambda (cands)
																													(loop for cand in cands
																																collect (progn
																																					(string-match "\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\(.*\\)" cand) ;; (url id point addr)
																																					(let* ((url   (match-string 1 cand))
																																								 (id    (match-string 2 cand))
																																								 (point (match-string 3 cand))
																																								 (addr  (match-string 4 cand)))
																																						(cons (format "%s %s  %s.howm::%s" 
																																													(adjust-string addr 90)
																																													(adjust-string url 40)
																																													id point)
																																									(list (match-string 2 cand) (match-string 3 cand) (match-string 1 cand))))))))
															(action ("B|BrowseUrl" . (lambda (x) (loop for ( memoid jump url ) in (helm-marked-candidates) do (browse-url url))))
																			("O|OpenMemo"  . (lambda (x) (loop for ( memoid jump url ) in (helm-marked-candidates) do (tt3-resource-show-memo :memoid memoid :jump jump))))
																			("M|Msgbox"    . (lambda (x) (loop for item in (helm-marked-candidates) do (message "%S" item))))))
									 :buffer "*web-url*")))
		
		(:tree  (tt3-menu-show-stringlist-menu (thinktank3-resource-index :name "Extension.Queries.Misc.WebUrlTree" :output :lisp) :tree))
		(:popup (tt3-menu-show-stringlist-menu (thinktank3-resource-index :name "Extension.Queries.Misc.WebUrlTree" :output :lisp) :popup))))

;; (tt3-menu-show-weburl :list)



;; keymapメニューをtree/listで表示する  ;; (tt3-menu-show-keymap-menu menu-bar-help-menu :list)
(defun tt3-menu-show-keymap-menu ( keymap-menu menu-type ) "
* [説明] keymap menuをtree/list/popup表示する
  [引数] keymap-menu  : keymap。　menu-bar-* で検索するといろいろ出てくる。
         menu-type    : :tree | :list | :popup 
         arg          : C-u 状態を植えとる "

	(flet ((menustr (str) (concat (substring str 0 1) "|" (replace-regexp-in-string " " "_" str)))
				 (keymapmenu-to-tree (km-menu)
														 (list (menustr (find-if 'stringp km-menu))
																	 (loop for item in (delete-if 'stringp (cdr km-menu))
																				 if (and (cddr item) (symbolp (cddr item))) ;; (id "cap" . func ) but not (id "---")
																				 collect (list (menustr (find-if 'stringp item))
																											 :item
																											 :command (cddr item)
																											 :help (or (getf item :help) (find-if 'stringp item)))
																				 else if (functionp (cadddr item)) ;; function
																				 collect (list (menustr (find-if 'stringp item))
																											 :item
																											 :command (cadddr item)
																											 :help (or (getf item :help) (caddr item)))
																				 else if (and (null (functionp (cadddr item))) (consp (cadddr item))) ;; child menu
																				 collect (keymapmenu-to-tree (cadddr item)))))

				 (itemlist-to-stringlist (item-list)
																 (loop for item in item-list
																			 collect (cons (mapconcat 'identity (car item) " ")
																										 (cddr item)))))
		(case menu-type
			(:popup (popup-menu keymap-menu))
			(:tree (tt3-popwin-show-menu (list (keymapmenu-to-tree (copy-tree keymap-menu)))))
			(:list (helm :sources `((name . "thinktank action")
															(candidates . ,(itemlist-to-stringlist (tree2list (list (keymapmenu-to-tree (copy-tree keymap-menu))))))
															(candidate-transformer .  tt3-menu-stringlist-to-helm-candidates)
															(action ("D|DoAction"  . (lambda (x) (tt3-menu-do-action (cdr x))))
																			("S|show" . (lambda (x) (msgbox "%S" (cdr x))))
																			("I|Insert" . (lambda (x) (insert (format "%S" (cdr x))))))))))))





;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [4/6] helm自動選択
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun thinktank3-menu-select-one-automatically () (interactive) "
* [説明] helm menuで候補が一つに絞られると自動で第１アクションが実行される。"
	(setq tt3-menu-execute-if-one-candidate (not tt3-menu-execute-if-one-candidate))
	(message "Autoselect %s" (if tt3-menu-execute-if-one-candidate "on" "off")))

;;
;; helm候補が１つに絞られると自動実行する。
;;
(defvar tt3-menu-execute-if-one-candidate nil)
(defun tt3-menu-one-candidate ()
	(when (and (eql 1 (helm-approximate-candidate-number)) tt3-menu-execute-if-one-candidate)
		(discard-input)
		(helm-exit-minibuffer)))

(add-hook 'helm-after-update-hook 'tt3-menu-one-candidate)
;; (remove-hook 'helm-update-hook 'tt3-menu-one-candidate)





'(
	(with-tt3-context arg
										(tt3-tt3-context)
										 )
	(loop-tt3-property
	 (tt3-tt3-peoperty)
	 )
)
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [5/6] コンテキスト記録
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defvar tt3-menu-arg '())     ; C-uの状態を保持
(defvar tt3-menu-context '()) ; 幾つかの状態を保持

;; with-tt3-menu-contextのbody内部で使える関数。　開始時のcontext値を返す
(defun tt3-tt3-menu-context-prefix () (string-to-number (or (car (assoc-default "pre" tt3-menu-context)) "0")))
(defun tt3-tt3-menu-context-filext () (car (assoc-default "ext" tt3-menu-context)))
(defun tt3-tt3-menu-context-majmod () (car (assoc-default "maj" tt3-menu-context)))
(defun tt3-tt3-menu-context-minmod () (car (assoc-default "min" tt3-menu-context)))
(defun tt3-tt3-menu-context-seltxt () (car (assoc-default "sel" tt3-menu-context)))
(defun tt3-tt3-menu-context-buffer () (car (assoc-default "buf" tt3-menu-context)))
(defun tt3-tt3-menu-context-calendar () (car (assoc-default "cal" tt3-menu-context)))

(defmacro with-tt3-menu-context ( C-u-arg &rest body) "
	(defun xxxx ( &optional arg ) (interactive \"P\") (with-tt3-menu-context arg (処理)) )
   で xxxx 呼び出し時の C-u 状態が記録される。 (処理)内で上記context関数が使える。
"
	(let ((arg (condition-case nil (car C-u-arg) (error 0))))
		`(progn (setq tt3-menu-arg ,arg)
						(setq tt3-menu-context `(("pre" ,(number-to-string (or tt3-menu-arg 0)))
																		 ("ext" ,(file-name-extension (buffer-name)))
																		 ("maj" ,(symbol-to-string major-mode))
																		 ("cal" ,(condition-case nil (cfw:calendar-to-emacs (cfw:cursor-to-nearest-date)) (error nil)))
																		 ("sel" ,(current-select-string))
																		 ("buf" ,(buffer-name))))

						,@body

						(setq tt3-menu-arg nil)
						(setq tt3-menu-context nil))))


(defun tt3-tt3-menu-context-match ( flag ) ;; menu 表示の確認のため、flagが保存contextに合致するかどうかチェックする
	;; [2014-09-20 Sat 18:01] なんかこのままじゃ使いにくいと思っている。

	;;
	;;  flag: pre:16,ext:howm,
	;;
	;;		pre: C-u状態           ( 0, 4, 16, 64 ,,, )
	;;	　tte: element名         ( ex: paragraph, time )
	;;	　ext: ファイル拡張子    (msgbox "%S" (file-name-extension (buffer-name)))
	;;	　maj: emacs major mode  (msgbox "%S" major-mode)
	;;    min: emacs minor mode  (msgbox "%S" minor-mode-list)
	;;	　sel: 選択状態 ( t ) 
	;;	　org: org element ( selected ) 
	;;	　buf: バッファー名 ( *scrache* )

	(or (not flag)
			(loop for item in (split-string flag ",")
						unless (destructuring-bind ( key val ) (split-string item ":") (equal (car (assoc-default key tt3-menu-context)) val))
						return nil
						finally return t)))

;; (tt3-tt3-menu-context-match "buf:tt3-menu.el")

;; ユーザー入力の収集
(defun tt3-tt3-menu-get-input ( input message ) "
* [説明] 複数情報源からユーザー入力を収集する"
	(setq message (or message "input:"))
	;; (msgbox "%S %S %S" input (current-select-string) (thing-at-point 'word) )
	(case input 
		(:select (or (current-select-string) (throw 'input-error 1)))
		(:point  (or (thing-at-point 'word) (throw 'input-error 2)))
		(:clip   (or (clipboarded-string) (throw 'input-error 3)))
		(:key    (or (and message (read-string message)) (throw 'input-error 4)))
		(:input  (or (current-select-string) (and message (read-string message)) (throw 'input-error 8)))
		(:cursor (or (current-select-string) (thing-at-point 'word) (throw 'input-error 5)))
		(:emacs  (or (current-select-string) (thing-at-point 'word) (and message (read-string message)) (throw 'input-error 6)))
		(:any    (or (current-select-string) (thing-at-point 'word) (clipboarded-string) (and message (read-string message)) (throw 'input-error 7)))
		(t "")))












;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [6/6] メニュー表示関数群
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; thinktankの基本menu formatはthinktank3-menu-default-string-listの形式  (string list形式)
(defun tt3-menu-stringlist-to-helm-candidates ( string-list )
	(let* ((helm-candidates string-list))

		;; context 依存制御
		(setq helm-candidates (loop for item in helm-candidates
																for context = (getf (cdr item) :context)
																if (or (not context) (tt3-tt3-menu-context-match context))
																collect item))
		;; global-keymap表示
		(setq helm-candidates (loop for ( caption . params ) in helm-candidates
																collect (cons (format "%-s %8s %-s %s"
																											(adjust-string caption 40)
																											(concat "[" (mapconcat (lambda (x) (substring x 0 1)) (split-string caption " ") "") "]")
																											(adjust-string (getf params :help) 60)
																											(cond ((find :command params) (getf params :command))
																														((find :url params) (adjust-string (getf params :url) 40))
																														((find :func params) (getf params :func))
																														((find :info params) (getf params :info))
																														((find :helm params) (getf params :helm))))
																							(cons caption params))))
		helm-candidates	))

;; string-list形式から item-list形式を経て、tree形式に変換する。
(defun tt3-menu-stringlist-to-tree ( string-list )
	(let* ((item-list string-list))

		;; context 依存制御
		(setq item-list (loop for item in item-list
													for context = (getf (cdr item) :context)
													if (or (not context) 
																 (and (stringp context) (tt3-tt3-menu-context-match context))
																 (and (symbolp context) (funcall context))
																 (and (consp context) (eval context)))
													collect item))
		
		;; string-list → item-list
		(setq item-list (loop for ( caption . plist ) in item-list
													collect `(,(split-string caption " ") :item ,@plist)))

		;; global-keymap表示
		(setq item-list (loop for ( caps . params ) in item-list
													if (find :command params)
													do (let* ((key-desc (key-description (where-is-internal (getf (cdr params) :command) nil t t))))
															 (when (< 0 (length key-desc))
																 (setq caps (append (subseq caps 0 -1) (list (format "%s [%s]" (car (last caps)) key-desc))))))
													collect `(,caps ,@params)))

		;; item-list → tree
		(list2tree item-list)))





;;
;; popwin ( https://github.com/m2ym/popwin-el/tree/v0.3 ) の設定とメニュー表示
;;

;;
;; 引数menutree は tree形式。 固定メニューは helmに適した string-list形式。
;;
;; string-list:  thinktank3-menu-default-string-list
;; tree:         (tt3-menu-stringlist-to-tree thinktank3-menu-default-string-list)
;;

(defvar tt3-popwin-buffer-name "*popwin-menu*")
(defvar tt3-popwin-buffer-height 15)
(push `(,tt3-popwin-buffer-name :position bottom :dedicated t :stick t :height ,tt3-popwin-buffer-height) popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)

(defun tt3-popwin-show-menu ( menutree ) 
	(when (helm-alive-p) (error "Error: Trying to run helm within a running helm session"))
	(unwind-protect 
			(let* ((curpos '(0)) (maxline 50) seltree candtree curhigh curwidt preceding-input input-key selitems)
				
				;; popwinの設定
				(get-buffer-create tt3-popwin-buffer-name)
				(make-variable-buffer-local 'global-hl-line-mode) 
				(setq global-hl-line-mode nil)				
				(thinktank-switch-ime nil)
				(popwin:display-buffer tt3-popwin-buffer-name)
				
				(catch 'exit-menu
					(while t
						;; ======== メニュー表示 ========
						(setq selitems "")
						(erase-buffer) (loop for row from 0 to maxline do (insert "\n"))            ;; 空行作成
						(flet ((sort-tree (tree) (condition-case nil (sort (copy-tree tree) (lambda (x y) (string< (car x) (car y)))) (error tree))))
							(loop for curtree = (sort-tree menutree) then (sort-tree (cadr seltree))  ;; 列単位でループ
										for currow in curpos
										for col from 0 to 20
										do (progn
												 (setq curhigh (length curtree))
												 (setq curwidt (loop for item in curtree maximize (length (car item))))
												 (set-window-text-height (get-buffer-window) (if (and (< (- tt3-popwin-buffer-height 1) curhigh)	(< curhigh 50)) curhigh tt3-popwin-buffer-height)) ; window高調整
												 (loop for ( caption . param ) in curtree ;; メニュー描画
															 for row from 0 to maxline
															 do (progn (goto-line (+ row 1))
																				 (goto-char (line-end-position))
																				 (when (equal currow row) (setq selitems (format "%s %s" selitems caption)))
																				 (setq caption (concat (adjust-string caption curwidt)
																															 (if (equal :item (car param)) "   " " > ")))
																				 (insert (if (equal currow row) (propertize caption 'face 'highlight) caption))))
												 (loop for row from curhigh to maxline ;; 空行描画
															 do (progn
																		(goto-line (+ row 1))
																		(goto-char (line-end-position))
																		(insert (make-string (+ 3 curwidt) ? ))))
												 (setq seltree (nth currow curtree) candtree curtree))))

						;; ======== キー入力 ========
						(goto-char (point-min))
						(let* ((pos (car (last curpos))))
							(setq input-key (or preceding-input (read-key (format "%s  [%s(%s)]:" selitems (ignore-errors (char-to-string input-key)) input-key))))
							(setq preceding-input nil)
							(case input-key
								(7  (throw 'exit-menu 1)) ;; C-g
								(27 (throw 'exit-menu 2)) ;: ESC
								
								((right 13 ?\C-f) ;; right: 選択
								 (if (equal (cadr seltree) :item)
										 (throw 'exit-menu seltree)
									 (setq curpos (append curpos '(0)))))

								((left ?\C-b) ;; left
								 (if (cdr curpos)
										 (setq curpos (subseq curpos 0 -1))
									 (throw 'exit-menu 3)))

								((up ?\C-p) ;; up
								 (setq curpos (append (subseq curpos 0 -1) (list (if (= pos 0) (- curhigh 1) (- pos 1))))))
								
								((down 32 ?\C-n) ;; down
								 (setq curpos (append (subseq curpos 0 -1) (list (if (= pos (- curhigh 1)) 0 (+ pos 1))))))

								((tab 9) ;; tab
								 (throw 'exit-menu (format "tohelm:%s" selitems)))

								(t (cond ((numberp input-key) ;; initial letter jump
													(let* ((candidates (loop for ( menu . items ) in candtree ;; イニシャル一致項目のrowを得る
																									 for num from 0
																									 if (string-match (format "^%c.*" input-key) menu)
																									 collect num)))
														(cond ((= 1 (length candidates))            ;; 選択
																	 (setq curpos (append (subseq curpos 0 -1) candidates))
																	 (if (equal :item (cadr (nth (car candidates) candtree))) 
																			 (setq preceding-input 'right)
																		 (setq curpos (append curpos '(0)))))
																	((< 1 (length candidates))            ;; 複数なら次の候補に移動
																	 (setq curpos (append (subseq curpos 0 -1) (list (or (find-if (lambda (x) (< pos x)) candidates) (car candidates)))))))))
												 (t (msgbox "%c" input-key))
												 )
									 ))))))
		
		;; popwinの後始末
		(ignore-errors
			(popwin:close-popup-window)
			(kill-buffer tt3-popwin-buffer-name))))


(provide 'tt3-menu)
