(setq package-enable-at-startup nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "emacs/eln-cache" (xdg-cache-home)))))
