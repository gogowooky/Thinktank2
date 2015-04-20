;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [1/6] thinktank3-property:  システムメモ(0000-00-00-00000?.howm)に記載したシステムプロパティを読み出す。
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defvar tt3-property-buffer-name "*prop-buf3*") ;; このバッファー上に全system memoが読み込まれる。
(defvar tt3-property nil)                       ;; 実際にはこの変数に格納される。


(defun thinktank3-property ( &optional node-address key value ) "
* [説明] node-addressで指定されるproperty値を取得またはvalueに書き換える。
  [例]  (thinktank3-property \"Thinktank.Host.thinktank\" \"url\" )
"
	(cond ((equal node-address :reset)  
				 (tt3-property-initialize)) ;; 次回更新

				((equal node-address :buffer) ;; buffer取得
				 (unless (get-buffer tt3-property-buffer-name) (tt3-property-initialize))
				 (get-buffer tt3-property-buffer-name))

				((stringp node-address)
				 (cond ((equal key :keyword) 		(last-tt3-property-node node-address (tt3-tt3-property-get-element :keyword value)))               ;; keywordのvalueを返す ( #+KEY: VALUE の形式 )
							 ((equal key :src-block)	(last-tt3-property-node node-address (tt3-tt3-property-get-element :src-block)))                   ;; src-blockのvalueを返す ( #+begin_src のみ )
							 ((equal key :text)       (last-tt3-property-node node-address (tt3-tt3-property-get-element :paragraph)))                   ;; paragraphを返す
							 ((equal key :json) (json-read-from-string (last-tt3-property-node node-address (tt3-tt3-property-get-element :paragraph)))) ;; paragraphをjsonとして読み出す              

							 ;; (thinktank3-property "Thinktank.Host.thinktank" "url")
							 ((stringp key)                                                                                  ;; 以下propertyの編集
								(let* ((prop (last-tt3-property-node node-address (tt3-tt3-property-get-element :property key))))
									(if (equal prop ":local")
											(cond ((equal value :delete) (tt3-property-local node-address key nil))
														((stringp value)       (tt3-property-local node-address key value))
														(t                     (tt3-property-local node-address key)))
										(cond ((equal value :delete)   (tt3-property-put-property node-address key nil))
													((stringp value)         (tt3-property-put-property node-address key value))
													(t                       prop)))))
							 (t nil)))
				
				(t tt3-property)            ;; 全propertyを返す
				))


(defun  tt3-property-initialize () "
* [説明] system memoから全propertyを読み込む。
         propertyは ( node-address  node-position  origin-file ) の list として保存されている。
         property値自体は buffer の node-position の nodeで読み取る
"
	(ignore-errors (kill-buffer tt3-property-buffer-name))                               
	;; 全system file読み込む --------------------------------------------------------------------------------------------------------------------
	(save-excursion
		(set-buffer (generate-new-buffer tt3-property-buffer-name))
		(loop for dir in (sort (directory-files tt3-memodir) 'string<)
					for fil = (condition-case nil (concat tt3-memodir dir "/" (substring dir -17) ".howm") (error "_"))
					if (file-exists-p fil)
					do (progn (insert (format "\n* ======== %s.howm ========\n" (substring dir -17)))
										(insert-file-contents fil)
										(goto-char (point-max))))
		
		(org-mode) (show-all) (beginning-of-buffer)	(outline-next-heading) (sleep-for 0.1) ;; org処理の準備
		
		;; リスト作成 ( node-address  node-position  origin-file ) --------------------------------------------------------------------------------
		(flet ((get-node-heading-and-pos (lvl ttl orig) 
																		 (loop for ( elemtype elemplist ) = (org-element-at-point) ;; (:headline (plist))
																					 for elemlvl = (or (getf elemplist :level) 0)        ;; :headlineはplistに:levelを持つ
																					 with pos = 0
																					 while (and (<= lvl elemlvl) pos)
																					 append (let* ((nodettl (replace-regexp-in-string (concat "@" (upcase (system-name)) "$") "" (getf elemplist :title))) ; 自環境nodeは@MACHINEを取る
																												 (elempos (getf elemplist :begin))
																												 (elemttl (concat ttl "." nodettl)))
																										'(msgbox "[%s][%s][%s][%s:%s][ori:%s]" nodettl elempos elemttl elemlvl lvl orig)
																										(setq pos (outline-next-heading))
																										(when (string-match "\\([0-9\-]\\{17\\}\\.howm\\)" nodettl) (setq orig (match-string 1 nodettl)))
																										(when (= lvl elemlvl) (cons (list (substring elemttl 1) elempos orig)
																																								(get-node-heading-and-pos (+ 1 lvl) elemttl orig)))))))
			;; 最後のｱｲﾃﾑで抜け出せなくなる
			(setq tt3-property (get-node-heading-and-pos 1 "" "")))))   ;;  (insert (format "%S" (tt3-property-initialize t)))



;; 以下３つはlist内の指定nodeを回すマクロ
(defmacro mapcar-tt3-property-subnode ( parent-node-addr &rest body ) "
* [説明] parent-node-addr配下のsubnodeをnarrowingし、bodyを評価した値を回収するマクロ
         body内で以下の変数が使える
         title:            node addr   ex)Extension.Queries.Memo.All
         parent\-node\-addr: 親のアドレス "
	`(with-current-buffer (thinktank3-property :buffer)
		 (loop for ( title pos orig-filename ) in tt3-property
					 with regexp = ,(concat "^" (regexp-quote parent-node-addr) "\\.[a-zA-Z0-9_\\.\\-]+$")
					 if (string-match regexp title)
					 collect (unwind-protect
											 (save-excursion (save-restriction (goto-char pos)
																												 (beginning-of-line)
																												 (org-narrow-to-subtree)

																												 ,@body))
										 (widen)))))


(defmacro last-tt3-property-node ( node-addr &rest body ) "
* [説明] mapcar-tt3-property-node でbodyを評価して得たlistの、最後の非nil値を返すマクロ"
	`(car (last (delq nil (mapcar-tt3-property-node ,node-addr ,@body)))))


(defmacro mapcar-tt3-property-node ( node-addr &rest body ) "
* [説明] node-addr指定に該当する複数nodeをnarrowingしながらbodyで評価し、得た値を回収するマクロ"
	
	`(with-current-buffer (thinktank3-property :buffer)
		 (loop for ( title pos orig-filename ) in tt3-property
					 with node-addr = ,node-addr
					 if (equal title node-addr)
					 collect (unwind-protect
											 (save-excursion (save-restriction (goto-char pos)
																												 (beginning-of-line)
																												 (org-narrow-to-subtree)
																												 ,@body))
										 (widen)))))


;; 以下２つは上記マクロ内で動かす必要がある
(defun tt3-tt3-property-get-original-info ( elemtype &optional key ) "
* [説明] node-addrで指定されるnodeをnarrowingし、body評価が非nilの場合、( original-filename . pos )を返す "
	(when (and (tt3-tt3-property-get-element elemtype key) orig-filename pos) 
		(cons orig-filename (- pos 
													 (car (assoc-default (format "======== %s ========" orig-filename) tt3-property))
													 42))))

(defun tt3-tt3-property-get-element ( elemtype &optional key ) "
* [説明] カーソル位置のnodeでelemtype型の値を返す(マクロ専用version) "
	(case elemtype
		(:node      t)
		(:headline  (loop for (typ plis) = (org-element-at-point)
											while (= 0 (forward-line))
											if (equal typ 'headline)
											return (getf plis :raw-value)))
		(:property  (org-entry-get nil key))
		(:keyword   (loop for (typ plis) = (org-element-at-point)
											while (= 0 (forward-line))
											if (and (equal typ 'keyword) (equal key (getf plis :key)))
											return (getf plis :value)))
		(:src-block  (loop for (typ plis) = (org-element-at-point)
											 while (= 0 (forward-line))
											 if (and (equal typ 'src-block) (find (getf plis :language) '( "thinktank" "mozrepl") :test 'equal))
											 return (getf plis :value)))
		(:paragraph (loop for (typ plis) = (org-element-at-point)
											for lin = (current-line-string)
											with text = ""
											while (= 0 (forward-line))
											if (equal typ 'paragraph)
											do (setq text (concat text lin "\n"))
											finally return (trim-string text)))))

(defun tt3-tt3-property-get-element-general-use ( elemtype &optional key ) "
* [説明] 現カーソル位置のnodeでelemtype型の値を返す(マクロ非専用version) " 
	(unwind-protect
			(save-excursion (save-restriction (org-narrow-to-subtree)
																				(goto-char (point-min))
																				(beginning-of-line)
																				(tt3-tt3-property-get-element elemtype key)))
		(widen)))

(defun tt3-tt3-property-get-node-addr ()
	(save-excursion (save-restriction
										(let (headings)	(condition-case nil
																				(while t
																					(destructuring-bind (typ plis) (org-element-at-point)
																						(when (equal typ 'headline) (push (getf plis :raw-value) headings))
																						(org-up-element)))
																			(error (mapconcat 'identity headings ".")))))))


;; propertyを書き換える。　書き換えられるのはpropertyのみ。　keywordやparagraphを書き換える方法は提供しない。
(defun tt3-property-put-property ( node-address key value ) "
* [説明] node-addrで指定されるnodeのkey propertyの値をvalueに書き換える。"

	(if (and (stringp key) (equal ":local" (thinktank3-property node-address key)))
			(tt3-property-local node-address key value)

		(destructuring-bind ( filename . pos ) (or (last-tt3-property-node node-address (tt3-tt3-property-get-original-info :property key)) ;; 既存key
																							 (last-tt3-property-node node-address (tt3-tt3-property-get-original-info :node)))        ;; 新規key
			(when pos
				(if (get-buffer filename)
						(with-current-buffer filename 
							(org-entry-put pos key value) (save-buffer))
					(save-excursion (find-file (thinktank3-format :memopath filename))
													(org-mode)
													(org-entry-put pos key value)
													(sleep-for 0.1)
													(set-buffer-modified-p nil)
													;;(save-buffer)
													(write-region (point-min) (point-max) filename)
													(sleep-for 0.1)
													(kill-buffer)))

				(tt3-property-initialize)))))

(defun tt3-property-local ( node-address key &optional value ) "
* [説明] node-addrで指定されるproperty値を取得または、valueに書き換える。"
	(let* ((local-prop-file (concat (thinktank3-config :tempdir) node-address ".howm")))
		(unless (file-exists-p local-prop-file) (with-temp-file local-prop-file (insert "* Thinktank Property\n")))
		(save-excursion (find-file local-prop-file)
										(org-mode)
										(prog1
												(cond (value (prog1 (org-entry-put nil key value) (save-buffer) (sleep-for 0.1)))
															(t     (progn (org-entry-get nil key))))
											(kill-buffer)))))






;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [2/6] thinktank3-config: 
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;;
;; システム設定値を返す。
;;
(defun* thinktank3-config ( &optional key value ) "
* [説明] system起動に必要な propertyを thinktank起動時に読み取りconfig値として保持している。
         nest無しの単純alist。
  [用例]
    (thinktank3-config)
    (thinktank3-config :memodir)
    (thinktank3-config :memodir \"C:/\")
    (thinktank3-config :delete-key :memodir) "

				(cond ((null key)           tt3-config)
							((null value)         (assoc-default key tt3-config))
							((eq key :delete-key) (setq tt3-config (delete* value tt3-config :test (lambda ( x y ) (eq x (car y))))))
							(t                    (tt3-config :delete-key key) 
																		(push (cons key value) tt3-config))))


(defvar tt3-memodir ;; memodirは memodir@MACHINE-NAME.conf ファイルの一行目に書かれているものを用いる。
	(with-temp-buffer
		(insert-file-contents (concat (file-name-directory load-file-name)
																	"configuration/memodir@" (upcase (system-name))
																	".conf"))
		(current-line-string)))

(defvar tt3-config nil) ;; 実際には下の処理により左の変数に格納される。



(setq tt3-config `((:memodir . ,tt3-memodir)  ;  (thinktank3-property "Thinktank.Host.thinktank" "memodir")
									 (:tempdir . ,(thinktank3-property "Thinktank.Host.thinktank" "tempdir"))
									 (:syncdir . ,(thinktank3-property "Thinktank.Host.thinktank" "syncdir"))
									 (:baseurl . ,(thinktank3-property "Thinktank.Host.thinktank" "url"))

									 (:firefox . ,(thinktank3-property "Thinktank.Host.thinktank" "firefox"))
									 
									 (:template-memo    . ,(thinktank3-property "Thinktank.Template" "memo"))
									 (:template-oneline . ,(thinktank3-property "Thinktank.Template" "oneline"))

									 (:memo    . ,(concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memo"))
									 (:memos   . ,(concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memos"))))


(provide 'tt3-system-property)
