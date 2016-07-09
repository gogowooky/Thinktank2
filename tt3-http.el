;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'url)
(require 'json)

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



(provide 'tt3-http)

