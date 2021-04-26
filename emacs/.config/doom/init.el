(doom! :lang
       (cc +lsp)
       (go +lsp)
       markdown
       (org +babel +export +present)
       python
       rest
       ruby
       sh
       :completion
       (company +childframe)
       (ivy +fuzzy +childframe)
       :tools
       lsp
       magit
       :checkers
       (spell +flyspell +hunspell +aspell +everywhere)
       syntax
       modeline
       :ui
       treemacs
       :os
       (popup +all +defaults)
       tty)
