(defun duncan/set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(duncan/set-frame-size-according-to-resolution)

(global-set-key (kbd "M-RET") 'ivy-switch-buffer)
(global-set-key (kbd "C-M-j") 'ivy-switch-buffer)

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)
(use-package! leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  :config
  (load-theme 'leuven t))

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

(setq confirm-kill-emacs nil)

;; Window splitting functions
(use-package! windmove
   :config
  ;; use command key on Mac
  ;;(::windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  :bind
  (("C-x -" . (lambda ()
               (interactive) (split-window-vertically) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x |" . (lambda ()
               (interactive) (split-window-horizontally) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x x" . (lambda ()
               (interactive) (kill-buffer (current-buffer)) (if (one-window-p) () (delete-window))))))
(bind-key* "M-<left>" #'windmove-left)
(bind-key* "M-<right>" #'windmove-right)
(bind-key* "M-<up>" #'windmove-up)
(bind-key* "M-<down>" #'windmove-down)
(bind-key* "<XF86Back>" (lambda () (interactive) (other-window -1)))
(bind-key* "<XF86Forward>" (lambda () (interactive) (other-window 1)))

(bind-key "<XF86Back>" (lambda () (interactive) (other-window -1)))
(bind-key "<XF86Forward>" (lambda () (interactive) (other-window 1)))

(use-package! tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! poly-org
  :defer t
  :hook
  (org-mode . poly-org-mode))

;; Other tools and browsers
(use-package! hackernews
  :defer t)

(use-package! org
  :defer t
;  :straight (:type built-in)
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)
  :custom
  (org-latex-listings 'minted)
  (org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode %f"
     "xelatex -shell-escape -interaction nonstopmode %f"))
  (org-todo-keyword-faces
   '(("RED" . (:foreground "red" :weight bold))
     ("YELLOW" . (:foreground "#EA6300" :background "#F6FECD" (:line-width 1 :color "#EA6300") :weight bold))
     ("GREEN" . (:foreground "#556b2f" :background "#20b2aa" (:line-width 1 :color "#556b2f") :weight bold))
     ("SHARE" . (:foreground "#0059b3" :background "#99ccff" (:line-width 1 :color "#0059b3") :weight bold))))
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-crypt-key nil "symmetric encryption")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Does not work in org-ql yet :-(
  (org-agenda-category-icon-alist
   `(("emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)))
  ;;(org-agenda-prefix-format "○ ")
  :config
  ;; we have Alt-Enter map to ivy-switch-buffer
  (unbind-key "M-<return>" org-mode-map)
  (unbind-key "M-RET" org-mode-map)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))
    ;;; (all-the-icons-insert-icons-for 'faicon) inserts all faicon icons to check

(use-package! org-super-agenda
  :defer t)
(use-package! org-ql
  :defer t)

(use-package! org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :config
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  :hook (org-mode . org-superstar-mode))

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package! ob-C
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:C org-babel-execute:C++))
(use-package! ob-ruby
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:ruby))
(use-package! ob-python
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:python))
(use-package! ob-octave
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:octave))
(use-package! ob-gnuplot
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:gnuplot))
(use-package! ob-markdown
  :defer t
    :requires (org-plus-contrib)
    :commands
    (org-babel-execute:markdown
     org-babel-expand-body:markdown))
(use-package! ob-http
  :requires (org-plus-contrib)
  :commands
  (org-babel-execute:http
   org-babel-expand-body:http))
(use-package! ob-shell
  :defer t
  :requires (org-plus-contrib)
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))
(use-package! ob-sql
  :defer t
  :commands (org-babel-execute:sql))
(use-package! ob-diagrams
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:diagrams))
(use-package! ob-ditaa
  :defer t
  :requires (org-plus-contrib)
  :custom
  (org-ditaa-jar-path "/usr/share/java/ditaa.jar")
  :commands (org-babel-execute:ditaa))
(use-package! ob-plantuml
  :defer t
  :requires (org-plus-contrib)
  :custom
  (org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  :commands (org-babel-execute:plantuml))
(use-package! ox-gfm
  :defer t)
(use-package! ox-reveal
  :defer t)

(use-package! htmlize
  :defer t)

(use-package! prodigy
  :defer t)

(use-package! ag
  :defer t)

(use-package! markdown-mode
  :defer t
  :bind (:map markdown-mode-map ("M-RET" . nil)))

(if (file-exists-p "~/.emacs.work.d/init.el")
    (load "~/.emacs.work.d/init.el")
(if (file-exists-p "~/.emacs.home.d/init.el")
    (load "~/.emacs.home.d/init.el")))

(use-package! mu4e
  :defer t
  :bind (:map mu4e-compose-mode-map ("<tab>" . dmacvicar/ivy-select-and-insert-contact))
  :custom
  (smtpmail-queue-dir (expand-file-name "~/Mail/queue/cur"))
  (message-signature-file (expand-file-name "~/.signature"))
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-use-gnus nil)
  (mu4e-view-prefer-html t)
  (mu4e-html2text-command "w3m -dump -T text/html")
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (mu4e-update-interval 1800)
  (mu4e-view-fields '(:subject :to :from :cc :bcc :from-or-to :date :attachments :maildir :mailing-list))
  (mu4e-maildir (expand-file-name "~/Mail"))
  (smtpmail-queue-mail nil)
  (mu4e-get-mail-command "/usr/bin/mbsync -aV")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (sendmail-program "/usr/bin/msmtp"))


;; work setup. It conflicts with the home setup because of mu4e
;; so we load one or the other

(use-package! outlook
  :after mu4e
  :init
  (require 'outlook-mu4e))

(use-package! mu4e-jump-to-list
  :after mu4e
  :defer t)

(use-package! mu4e-contrib
  :after mu4e
  :defer t
  :load-path mu4e-system-path)

(use-package! mu4e-conversation
  :after mu4e
  :defer t)

(use-package! org-mu4e
  :after mu4e
  :defer t
  :load-path mu4e-system-path)

(use-package! mu4e-icalendar
  :after mu4e
  :load-path mu4e-system-path
  :config
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup))

(use-package! mu4e-views
  :after mu4e
  :bind (:map mu4e-headers-mode-map
	      ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	      ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	      ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message)) ;; toggle opening messages automatically when moving in the headers view)
  :config
  (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view)
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-default-view-method "text")
  (mu4e-views-mu4e-use-view-msg-method "text")
  (setq mu4e-views-auto-view-selected-message nil))

;; Contacts completion
;; http://pragmaticemacs.com/emacs/tweaking-email-contact-completion-in-mu4e/
;;need this for hash access
(require 'subr-x)

;;my favourite contacts - these will be put at front of list
(setq dmacvicar/contact-file "~/.favorite-contacts.txt")

(defun dmacvicar/read-contact-list ()
  "Return a list of email addresses"
  (with-temp-buffer
    (when (file-exists-p dmacvicar/contact-file)
      (insert-file-contents dmacvicar/contact-file))
    (split-string (buffer-string) "\n" t)))

;;ivy contact completion
;;based on http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/
(defun dmacvicar/ivy-select-and-insert-contact (&optional start)
  (interactive)
  ;;make sure mu4e contacts list is updated - I was having
  ;;intermittent problems that this was empty but couldn't see why
  ;;(mu4e~request-contacts)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t)))
        ;;append full sorted contacts list to favourites and delete duplicates
        (contacts-list
         (delq nil (delete-dups (append (dmacvicar/read-contact-list) (hash-table-keys mu4e~contacts))))))
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
             (contact
              (ivy-read "Contact: "
                        contacts-list
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input (buffer-substring-no-properties start end))))
        (unless (equal contact "")
          (kill-region start end)
          (insert contact))))))
