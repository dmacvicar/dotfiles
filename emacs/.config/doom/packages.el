;; ui stuff
(package! leuven-theme)
(package! windmove)

;; lang stuff
(package! tree-sitter)
(package! tree-sitter-langs)

;; mu4e
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

(package! poly-org)
(package! org-superstar)
(package! ob-markdown
  :recipe (:host github :repo "tnoda/ob-markdown"))
(package! ob-http)

(package! olivetti)

;; hack
;; https://github.com/magit/magit/issues/3749

;; allows to start services
(package! prodigy)

(package! ag)
