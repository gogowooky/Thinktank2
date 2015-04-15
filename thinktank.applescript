tell application "Terminal"
do script "echo dummy"
do script "cd /Users/gogowooky/.emacs.d/site-lisp/thinktank/" in window 1
do script "ruby thinktank.rb version2.1" in window 1
end tell
