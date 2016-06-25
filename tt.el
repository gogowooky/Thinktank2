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


;;
;; property setup
;;
(thinktank3-property :reset)


;;
;; menu setup
;;
(setq thinktank3-menu-string-list 
			'(( "M|Memo C|Clipboard I|Id" :context "ext:howm" :func (push-string-to-clipboard (thinktank3-format :memofile (buffer-name))) :help "メモのIDをコピーする")
				( "M|Memo C|Clipboard T|Title" :context "ext:howm" :func (save-excursion (goto-char (point-min)) (push-string-to-clipboard (current-line-string))) :help "メモのtitleをコピーする")
				( "M|Memo C|Clipboard F|Filepath" :context "ext:howm"	:func (push-string-to-clipboard (thinktank3-format :memopath (buffer-name))) :help "メモのfilepathをコピーする")
				( "M|Memo D|Delete" :context "ext:howm" :command thinktank3-resource-destroy-memo :help "メモを削除")
				( "M|Memo V|Version M|Minor-up" :context "ext:howm" :command thinktank3-resource-minor-version-up :help "メモをマイナーバージョンアップ" )
				( "M|Memo V|Version J|Major-up" :context "ext:howm" :command thinktank3-resource-major-version-up :help "メモをメジャーバージョンアップ" )
				( "M|Memo B|Browse" :context "ext:howm" :command thinktank3-mozrepl-browse-memo :help "メモをbrowse")
				( "M|Memo W|winword" :context "ext:howm" :command thinktank3-mozrepl-memo-docx :help "wordファイルを表示")
				( "M|Memo M|Home" :command thinktank3-resource-show-top-memo :help "トップページ" )
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

				( "S|System W|Restart-webrick" :command thinktank3-system-run-webrick :help "local webrick serverを起動する" )
				( "S|System D|Directory" :command thinktank3-util-open-directory :help "ディレクトリを開く" )
				( "S|System L|Reload-memo" :command thinktank3-resource-reload :help "メモをリロードする" )
				( "S|System S|Restruct-memo" :command thinktank3-resource-restruct :help "メモをリロードする" )
				( "S|System P|Replace-memo" :command thinktank3-resource-replace :help "メモをリロードする" )
				( "S|System O|Object" :command thinktank3-resource-show-property-object :help "objectの数を数える" )
				( "S|System A|AutoSelect" :command thinktank3-menu-select-one-automatically :help "helmで１つに絞り込んだら自動選択")
				( "S|System M|Menu-Initialize" :command thinktank3-menu-initialize :help "menuを更新する" )

				( "W|Web R|Ruby M|Rurima" :command thinktank3-mozrepl-rurema-search :help "るりまさーち")
				( "W|Web R|Ruby R|RubyRef" :command thinktank3-mozrepl-rubyref-search :help "Rubyリファレンス" )
				
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

(defun thinktank3-add-search-engine-to-menu ()	;; web search engine の登録
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
																				 thinktank3-menu-string-list)))))
(add-hook 'thinktank3-menu-after-initialize-hook 'thinktank3-add-search-engine-to-menu)

(defun thinktank3-add-query-to-menu ()	;; Query 関連の登録
	(mapcar-tt3-property-subnode "Menu.Query"
															 (let* ((menu (org-entry-get nil "menu"))
																			(help (org-entry-get nil "help")))
																 (when menu (push `(,menu
																										:func    (thinktank3-resource-index :name ,title)
																										;; :func    (thinktank3-resource-index :name ,menu)
																										:help    ,help
																										:context ,(org-entry-get nil "context"))
																									thinktank3-menu-string-list)))))
(add-hook 'thinktank3-menu-after-initialize-hook 'thinktank3-add-query-to-menu)

(defun thinktank3-add-mozrepl-to-menu ()	;; MozRepl 関連の登録
		;(mapcar-tt3-property-subnode "Menu.Mozrepl"
		;														 (let* ((menu (org-entry-get nil "menu"))
		;																		(help (org-entry-get nil "help"))
		;																		(code (trim-string (tt3-tt3-property-get-element :src-block))))
		;															 (when menu (push `(,menu
		;																									:func    (tt3-mozrepl-request ,code)
		;																									:help    ,help)
		;																								thinktank3-menu-string-list))))
	)
(add-hook 'thinktank3-menu-after-initialize-hook 'thinktank3-add-mozrepl-to-menu)

(defun thinktank3-add-oneline-to-menu ()	;; 一行メモの関数定義＆登録
		;; エラーでる。
		'(loop for ( memoid name comm supl menu ) in (thinktank3-resource-index :name "Menu.Query.AssociateFile.oneline" :output :lisp)
					 unless (equal menu "")
					do (push `(,menu :func (tt3-resource-append-oneline :memoid ,memoid :name ,name) :help ,(concat "一行メモ:" name)) thinktank3-menu-string-list))
		)
(add-hook 'thinktank3-menu-after-initialize-hook 'thinktank3-add-oneline-to-menu)


(thinktank3-menu-initialize)


;;
;; org setup
;;

;; WebSearchEnginesをorg-link-abbrev-alistに登録　文中 [[google:orexin leptin]] で検索できるようになる。
(setq org-link-abbrev-alist nil)
(mapcar-tt3-property-subnode "Menu.WebSearch" 
														 (when (org-entry-get nil "orgtag")
															 (push (cons (org-entry-get nil "orgtag") (replace-regexp-in-string "%input" "%s" (org-entry-get nil "url"))) org-link-abbrev-alist)))


;;
;; to monitor thinktank.log
;;
(add-hook 'find-file-hook (lambda () (when (string-match "log$" (buffer-name)) (auto-revert-tail-mode t))))
(add-hook 'after-revert-hook (lambda () (when auto-revert-tail-mode (end-of-buffer))))

(provide 'tt)




