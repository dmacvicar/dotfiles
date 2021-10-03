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
       (javascript +lsp)
       (json +lsp)
       (markdown +grip)
       (org +babel +export +present +pandoc +pretty)
       (python)
       rest
       (ruby +lsp)
       sh
       yaml
       (web +lsp +html +css)
       :editor
       (format)
       :mail
       mu4e
       :os
       (popup +all +defaults)
       tty
       :term
       shell
       vterm
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
