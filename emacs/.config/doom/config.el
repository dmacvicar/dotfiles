
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
