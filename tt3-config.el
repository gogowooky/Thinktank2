;; tt3-config.el; -*- mode: emacs-lisp; coding: utf-8 -*-

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; 追加メニュー表示コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun tt:resource-show-web-url-list ( &optional arg )   (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-weburl :list)))
(defun tt:resource-show-web-url-tree ( &optional arg )   (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-weburl :tree)))
(defun thinktank3-resource-show-help-menu-list ( &optional arg ) (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-keymap-menu menu-bar-help-menu :list)))
(defun thinktank3-resource-show-help-menu-tree ( &optional arg ) (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-keymap-menu menu-bar-help-menu :tree)))


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; メニューデータ整形
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; urlをtree/listで表示する
(defun tt3-menu-show-weburl ( menu-type ) "
* [説明] thinktank memo内のurlを列挙表示する
  [引数] menu-type  : :tree | :list | :popup
         arg        : C-u 状態を得る "

	(case menu-type
		(:list (let* ((source (tt:resource-index :name "Extension.Queries.Misc.WebUrlList" :output :list)))
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
		
		(:tree  (tt3-menu-show-stringlist-menu (tt:resource-index :name "Extension.Queries.Misc.WebUrlTree" :output :lisp) :tree))
		(:popup (tt3-menu-show-stringlist-menu (tt:resource-index :name "Extension.Queries.Misc.WebUrlTree" :output :lisp) :popup))))

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
			(:tree (tt3-menu-popwin-show-menu (list (keymapmenu-to-tree (copy-tree keymap-menu)))))
			(:list (helm :sources `((name . "thinktank action")
															(candidates . ,(itemlist-to-stringlist (tree2list (list (keymapmenu-to-tree (copy-tree keymap-menu))))))
															(candidate-transformer .  tt3-menu-stringlist-to-helm-candidates)
															(action ("D|DoAction"  . (lambda (x) (tt3-menu-do-action (cdr x))))
																			("S|show" . (lambda (x) (msgbox "%S" (cdr x))))
																			("I|Insert" . (lambda (x) (insert (format "%S" (cdr x))))))))))))




(provide 'tt3-config)

