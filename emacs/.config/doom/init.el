(defconst mu4e-system-path "/usr/share/emacs/site-lisp/mu4e")
;;(add-to-list 'load-path mu4e-system-path)

(doom! :checkers
       (spell +flyspell +hunspell +aspell +everywhere)
       syntax
       ophints
       minimap
       modeline
       :completion
       (company +childframe)
       (ivy +fuzzy +childframe)
       :emacs
       dired
       :lang
       (cc +lsp)
       (go +lsp)
       (json +lsp)
       markdown
       (org +babel +export +present +pandoc +pretty)
       (python)
       rest
       (ruby +lsp)
       sh
       yaml
       :mail
       mu4e
       :os
       (popup +all +defaults)
       tty
       :term
       shell
       :tools
       (eval +overlay)
       lookup
       (debugger +lsp)
       lsp
       magit
       :ui
       doom
       doom-dashboard
       treemacs
       workspaces)
