(defconst mu4e-system-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path mu4e-system-path)

(doom! :lang
       (cc +lsp)
       (go +lsp)
       (json +lsp)
       markdown
       (org +babel +export +present +pandoc +pretty)
       python
       rest
       ruby
       sh
       yaml
       :emacs
       dired
       :completion
       (company +childframe)
       (ivy +fuzzy +childframe)
       :tools
       (eval +overlay)
       lookup
       (debugger +lsp)
       lsp
       magit
       :checkers
       (spell +flyspell +hunspell +aspell +everywhere)
       syntax
       ophints
       minimap
       modeline
       :ui
       doom
       doom-dashboard
       treemacs
       workspaces
       ;indent-guides
       ;tabs
       :os
       (popup +all +defaults)
       tty
       :mail
       mu4e)
