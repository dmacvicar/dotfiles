(package! windmove)

(package! tree-sitter)
(package! tree-sitter-langs)

(package! mu4e-views
  :recipe
  (:host github :repo "lordpretzel/mu4e-views"))

(package! outlook)
(package! mu4e-jump-to-list :disable t)
;(package! mu4e-contrib :load-path mu4e-system-path)
(package! mu4e-conversation :disable t)
;(package! org-mu4e :load-path mu4e-system-path)
;(package! mu4e-icalendar :load-path mu4e-system-path)
(package! mu4e-views)


(package! ob-markdown
  :recipe (:host github :repo "tnoda/ob-markdown"))
