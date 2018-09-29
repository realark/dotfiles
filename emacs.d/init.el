;;; init.el --- emacs init file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; Just an Emacs init.  Nothing special here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;;; Standard options and util functions

(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
;; Don't require an ending newline
(setq require-final-newline nil)
(setq next-line-add-newlines nil)
;; Flash instead of beeping
(setq visible-bell t)
;; Remove icons toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1) ; turn on paren match highlighting
(setq-default show-paren-style 'expression) ; highlight entire bracket expression
;; Don't tell me what I can't do!
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; highlight the current line
(global-hl-line-mode t)
(setq large-file-warning-threshold 100000000) ;100mb
(setq create-lockfiles nil)
(blink-cursor-mode -1)
(setq help-window-select t)
(setq echo-keystrokes 0.1)

(mouse-avoidance-mode 'exile)

(setq completion-ignore-case t)

(global-visual-line-mode 1)
(setq-default line-move-visual t)

(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-initial-delay 0)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      backup-by-copying t)

;; pretty lambda
(setq-default prettify-symbols-alist
              ;; lambda -> λ
              '(("lambda" . 955)))
(global-prettify-symbols-mode 1)

(defun my-minibuffer-setup-hook ()
  "Disable GC in the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  "Re-enable GC after minibuffer exit."
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;Turn off tabs and indent two spaces
(setq-default indent-tabs-mode nil
              c-basic-offset 2
              tab-width 2)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Misc elisp utils
(progn
  (defun load-if-exists (file)
    "If FILE exists load it."
    (when (file-exists-p file)
      (load file)))

  (defun first-existing-file (&rest files)
    "Return the first existing file in FILE-LIST or nil of no file in the list exists."
    (cl-loop for file in files do
             (when (file-exists-p file)
               (cl-return file))))

  (defun get-string-from-file (file-path)
    "Return the contents of FILE-PATH as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  (defmacro boundp-and-true (mode-name)
    "Non-nil if the mode specified by MODE-NAME is active."
    `(and (boundp (quote ,mode-name)) ,mode-name))

  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

  (defun toggle-window-split ()
    "Toggle two-window split between horizontal and vertical."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "all buffers refreshed"))

  (defmacro require-install (PCK)
    "Require package PCK, install via \"package-install\" if missing."
    `(unless (require ,PCK nil t)
       (package-install ,PCK)
       (require ,PCK))))

;; Change startup messages
(progn
  (defun display-startup-echo-area-message ()
    "Change the startup message."
    ;; Print deathday from https://www.death-clock.org/
    (message "Sunday, 28th June 2071"))

  (setq initial-scratch-message
        (concat
         (replace-regexp-in-string
          "^" ";; "                     ; comment each line
          (replace-regexp-in-string
           "\n$" ""                     ; remove trailing linebreak
           (get-string-from-file "/etc/motd")))
         "\n\n")))

(load-if-exists "~/.admin/sec.el")

;; use mepla and marmalade for package
(progn
  (require-install 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (require-install 'use-package)
  (setq-default use-package-always-ensure t)

  (use-package try
    :commands try))

;; settings for osx
(progn
  (setq-default mac-command-modifier 'meta)
  (use-package exec-path-from-shell
    :if (memq window-system '(ns))
    :config
    (exec-path-from-shell-initialize)))

;; Themes and UI tweaks
(progn
  (use-package powerline)

  (use-package delight
    :config
    (delight 'eldoc-mode nil 'eldoc)
    (delight 'undo-tree-mode nil 'undo-tree)
    (delight 'auto-revert-mode nil 'autorevert))

  ;; Theme
  (defadvice load-theme (before theme-dont-propagate activate)
    "Change the theme."
    (mapc #'disable-theme custom-enabled-themes))

  (use-package moe-theme
    :if window-system
    :ensure t
    :load-path "themes"
    :config
    (moe-theme-set-color 'cyan)
    (powerline-moe-theme))

  (use-package farmhouse-theme
    :if window-system
    :ensure t
    :defer t
    :load-path "themes")

  (use-package circadian
    :if window-system
    :ensure t
    :config
    (setq-default circadian-themes '(("07:30" . farmhouse-light)
                                     ("18:00" . moe-dark)))
    (circadian-setup))


  ;; Maximize emacs window
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; winner mode allows easy undo/redo of window changes
  (winner-mode 1)

  ;; use ace-window
  (use-package ace-window))

;; comment toggling
(progn
  (defun select-current-line ()
    "Select the current line."
    (end-of-line)                       ; move to end of line
    (set-mark (line-beginning-position)))

  (defun toggle-comment-region-or-line ()
    "Comment or uncomment the selected region.  If no region is selected use the current line."
    (interactive)
    (if (not mark-active)
        (select-current-line))
    (when nil                           ; TODO DWIM lisp comment
      (if (boundp-and-true smartparens-mode)
          (save-excursion
            (message "TODO sp-comment")
            ;; go to end of region
            ;; for each line going up:
            ;;   if not commented
            ;;      goto beginning of line
            ;;      sp-comment
            (beginning-of-line)
            ;; (save-excursion
            ;;   ;; (let ((start (region-beginning))
            ;;   ;;       (end (region-end)))
            ;;   ;;   (deactivate-mark)
            ;;   ;;   ;; (while (<=  (mark) start)
            ;;   ;;   ;;   (beginning-of-line)
            ;;   ;;   ;;   (lispy-comment)
            ;;   ;;   ;;   (forward-line -1))
            ;;   ;;   (message "TODO: lispy!!"))
            ;;   )
            (sp-comment))))
    (comment-or-uncomment-region (region-beginning) (region-end))))

;; Options for M-x rgrep
(progn
  (eval-after-load 'grep
    '(when (boundp 'grep-find-ignored-files)
       (mapc (lambda (file-regex)
               (add-to-list 'grep-find-ignored-files file-regex))
             '("*.fasl"
               "*.class"))))
  (eval-after-load 'grep
    '(when (boundp 'grep-find-ignored-directories)
       (mapc (lambda (dir-regex)
               (add-to-list 'grep-find-ignored-directories dir-regex))
             '("target"
               "build"
               "bin")))))

;;Evil (extensible vi layer for Emacs)
(use-package evil
  :init
  (evil-mode 1)
  ;; Default to normal mode most of the time
  (setq-default evil-insert-state-modes '(nrepl-mode shell-mode git-commit-mode term-mode eshell-mode))
  (setq-default evil-emacs-state-modes '(magit-mode magit-popup-mode))
  (setq-default evil-motion-state-modes '()))

;; general for keybindings
(use-package general
  :init
  (general-evil-setup t)
  :config
  (general-define-key "<escape>" (general-simulate-keys "C-g"))

  (general-define-key "C-+" 'text-scale-adjust)

  (general-nmap "/" (general-simulate-keys "C-s" t))
  (general-nmap "C-j" (general-simulate-keys "n" t))
  (general-nmap "C-k" (general-simulate-keys "p" t))

  (general-nmap :mode 'help-mode "q" (general-simulate-keys "q" t))
  (general-nmap :mode 'help-mode "RET" (general-simulate-keys "RET" t))

  (general-define-key
   :states '(normal visual)
   "<SPC>"  (general-simulate-keys "C-x"))

  (general-define-key
   :prefix "C-x"
   "4"      #'toggle-window-split
   "x"      #'execute-extended-command
   "l"      (lambda ()
              (interactive)
              (if (bound-and-true-p whitespace-mode)
                  (progn
                    (whitespace-mode 0)
                    (linum-mode 0))
                (progn
                  (message "nil branch")
                  (whitespace-mode 1)
                  (linum-mode 1))))
   "f"      #'indent-region
   ";"      #'toggle-comment-region-or-line
   "g"      #'magit-status
   "B"      #'magit-blame)

  (general-define-key "C-x <SPC>" #'find-file))

;; Hydras
(use-package hydra
  :config
  (general-define-key
   "C-x t"
   (defhydra hydra-test (:color blue :columns 1)
     "Run Tests"
     ("t"   #'single-test                "Run a single test")
     ("u"   #'run-unit-tests             "Unit")
     ("i"   #'run-integration-tests      "Integration")
     ("p"   #'run-performance-tests      "Performance")
     ("r"   #'reload-systems             "Reload Systems")
     ("q"   nil "Cancel" :exit t)))
  (progn
    ;; Window moving helpers for hydra
    (require 'windmove)

    (defun hydra-move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (shrink-window-horizontally arg)
        (enlarge-window-horizontally arg)))

    (defun hydra-move-splitter-right (arg)
      "Move window splitter right."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (enlarge-window-horizontally arg)
        (shrink-window-horizontally arg)))

    (defun hydra-move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (enlarge-window arg)
        (shrink-window arg)))

    (defun hydra-move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (shrink-window arg)
        (enlarge-window arg))))

  (defhydra hydra-tabs (:color amaranth :hint nil)
    "
TABS: %(buffer-name)
-------------------------------------------------------------
 Move:        _L_:right _H_:left
 Switch:      _l_:right _h_:left _k_:up-group _j_:down-group
 Manage:      _c_reate-group _s_witch-group
EOF"
    ("l" tabbar-forward-tab)
    ("h" tabbar-backward-tab)
    ("j" tabbar-backward-group)
    ("k" tabbar-forward-group)
    ("L" (my-tabbar-move-current-tab-one-place :right))
    ("H" (my-tabbar-move-current-tab-one-place :left))
    ("c" (my-create-tabbar-group (read-from-minibuffer "Enter new group name: ")) :exit t)
    ("s" (error "TODO") :exit t)
    ("q" nil))

  (general-define-key
   "C-x w"
   (defhydra hydra-window (:color amaranth :hint nil)
     "
WINDOW: %(buffer-name)
-------------------------------------------------------------
 Split:        _v_ert _x_:horz
 Delete:       _o_nly  _d_window
 Change:       _s_wap _b_uffer

 Tabs:         _t_ab hydra

 Move:         _h_left _j_up _k_down _l_right
 Nudge:        _H_left _J_down _K_down _L_right

 Winner:       _u_ndo  _C-r_edo
EOF"
     ("t" hydra-tabs/body :exit t)
     ("v" split-window-right)
     ("v" split-window-right)
     ("x" split-window-below)
     ("s" ace-swap-window)
     ("b" switch-to-buffer)
     ("o" delete-other-windows :exit t)
     ("d" delete-window)
     ("h" windmove-left)
     ("j" windmove-down)
     ("k" windmove-up)
     ("l" windmove-right)
     ("H" hydra-move-splitter-left)
     ("J" hydra-move-splitter-down)
     ("K" hydra-move-splitter-up)
     ("L" hydra-move-splitter-right)
     ;; winner-mode must be enabled
     ("u" winner-undo)
     ("C-r" winner-redo)
     ("q" nil)))
  (general-define-key
   "C-x p"
   (defhydra hydra-projectile (:color blue :hint nil)
     "
     PROJECTILE: %(or (ignore-errors (projectile-project-root)) \"(Not in a project)\")

 Find/Replace         Tasks                   Buffers
----------------------------------------------------------------------------------
 _f_: file find         _t_: test project         _k_: Kill all buffers
 _t_: find tag          _c_: command run
 _g_: grep all files
 _r_: replace
 _R_: replace regex

"
     ("f"   projectile-find-file)
     ("t"   projectile-find-tag)
     ("g"   projectile-grep)
     ("r"   projectile-replace)
     ("R"   projectile-replace-regexp)
     ("t"   projectile-test-project)
     ("c"   projectile-run-project)
     ("k"   projectile-kill-buffers)
     ("q"   nil "Cancel" :color red))))

(use-package smartparens
  :init
  (smartparens-global-strict-mode)
  :delight smartparens-mode
  :config
  (sp-pair "'" nil :actions nil)
  (sp-pair "`" nil :actions nil))

(use-package lispyville
  :delight lispyville-mode
  :init
  (add-hook 'lisp-mode-hook                          #'lispyville-mode)
  (add-hook 'emacs-lisp-mode-hook                    #'lispyville-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook   #'lispyville-mode)
  (add-hook 'ielm-mode-hook                          #'lispyville-mode)
  (add-hook 'lisp-mode-hook                          #'lispyville-mode)
  (add-hook 'lisp-interaction-mode-hook              #'lispyville-mode)
  (add-hook 'slime-repl-mode                         #'lispyville-mode)
  (add-hook 'scheme-mode-hook                        #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators
                              (escape insert)
                              (slurp/barf-cp normal)
                              (additional normal)
                              (additional-movement normal visual motion))))

;; ivy and friends
(use-package ivy
  :delight ivy-mode
  :init
  (ivy-mode t)
  :general
  ("C-s" #'swiper
   "M-x" #'counsel-M-x
   "C-x C-f" #'counsel-find-file)
  (:keymaps 'ivy-minibuffer-map
            "M-j"     #'ivy-next-line
            "M-k"     #'ivy-previous-line
            "M-S-j"   #'ivy-scroll-up-command
            "M-S-k"   #'ivy-scroll-down-command
            "C-j"     #'ivy-next-history-element
            "C-k"     #'ivy-previous-history-element)
  (:keymaps 'counsel-find-file-map
            "<return>" #'ivy-alt-done)
  :config
  (use-package swiper
    :init
    (setq-default swiper-action-recenter t))
  (use-package counsel)

  (setq-default ivy-wrap t)
  (setq-default ivy-use-virtual-buffers t)
  (setq-default ivy-height 10)
  (setq-default ivy-count-format "(%d/%d) ")
  (setq-default ivy-extra-directories nil)
  (setq-default ivy-re-builders-alist '((t . ivy--regex-plus)))
  ;; isearch-forward seems to get stuck in the wrong state with swiper.
  ;; Manually setting it to T seems to work around the problem.
  ;; This is probably a problem with my config
  (setq isearch-forward t))

;; yasnippet
(use-package yasnippet
  :delight yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  ;; don't turn on yas if there are no snippets
  (defun disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1)))
  (add-hook 'yas-minor-mode-hook #'disable-yas-if-no-snippets)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
  (use-package common-lisp-snippets))

;; company mode
(use-package company
  :delight company-mode
  :init
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
            "M-n" nil
            "M-p" nil
            "M-j" #'company-select-next
            "M-k" #'company-select-previous
            "M-?" #'company-show-doc-buffer
            "M-." #'company-show-location)
  :config
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode t)
    (setq-default company-idle-delay 0.25)
    (setq-default company-minimum-prefix-length 2)))

;; Projectile
(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq-default projectile-completion-system 'ivy)
  (setq-default projectile-globally-ignored-directories
                (append projectile-globally-ignored-directories
                        '(".git" ".ensime_cache.d" ".gradle"
                          ".recommenders" ".metadata" "dist")))
  (setq-default projectile-globally-ignored-files
                (append projectile-globally-ignored-files
                        '(".ensime" "*.war" "*.jar" "*.zip"
                          "*.png" "*.gif" "*.vsd" "*.svg"
                          "*.exe" "eclimd.log" "workbench.xmi"
                          ".emacs.desktop" "*.deb" "*.gz" "*.fasl")))
  (setq-default projectile-enable-caching t)

  (progn
    (defun gradle-command-alias (cmd)
      "Simplify running commands on specific gradle subprojects."
      (let ((tokens (split-string cmd)))
        ;; use wrapper
        (setcar tokens "./gradlew")
        ;; replace arg '/' with ':'
        (mapconcat (lambda (arg)
                     (if (string-equal "./gradlew" arg)
                         arg
                       (replace-regexp-in-string "/" ":" arg)))
                   tokens
                   " ")))

    (defun my-projectile-command-advice (orig-fn &rest args)
      (apply orig-fn
             (if (or (string-match "^gradle" (first args))
                     (string-match "^./gradlew" (first args)))
                 (list (gradle-command-alias (first args)))
               args)))

    ;; projectile-run-compilation ?
    (advice-add #'projectile-run-compilation :around #'my-projectile-command-advice))

  (progn ; rebuild projectile cache after magit checkouts
    (defun %run-projectile-invalidate-cache (&rest _args)
      ;; We ignore the args to `magit-checkout'.
      (projectile-invalidate-cache nil))
    (advice-add 'magit-checkout
                :after #'%run-projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout ; This is `b c'.
                :after #'%run-projectile-invalidate-cache)))

(use-package treemacs
  :defer t
  :general
  ("<f6>" #'treemacs)
  :config
  (use-package treemacs-evil)
  (use-package treemacs-projectile
    :config
    (setq-default treemacs-header-function #'treemacs-projectile-create-header))
  (setq-default treemacs-git-integration t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package tabbar
  :init
  (tabbar-mode t)
  :config
  (setq frame-title-format '("emacs <" (:eval (symbol-name tabbar-current-tabset)) "> - %b"))

  (set-face-foreground 'tabbar-selected "white")
  (set-face-background 'tabbar-selected "DarkBlue")

  ;; TODO: session tabs with hydra-tab support
  (defvar my-tabbar-groups
    "Additional tabbar groups (session-persistence only)"
    (list ))

  (defun my-create-tabbar-group (group-name)
    (interactive)
    (unless (or (ignore-errors (assoc group-name my-tabbar-groups))
                (find group-name '("emacs" "org" "other")))
      (push (cons group-name (list (current-buffer))) my-tabbar-groups)))

  (defun find-group-for-buffer (buffer)
    (cl-loop for group in my-tabbar-groups do
             (when (find buffer (cdr group))
               (cl-return (car group)))))

  (add-hook 'kill-buffer-hook
            (lambda ()
              (cl-loop for group in my-tabbar-groups do
                       (setf (cdr group) (remove (current-buffer) (cdr group))))))

  ;; tabbar-current-tabset  == current tab set

  (defun my-tabbar-buffer-groups ()
    "Show all normal files in one group"
    (list
     (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
           ((string-prefix-p "TAGS" (buffer-name)) "emacs")
           ((string-prefix-p "magit" (buffer-name)) "emacs")
           ((eq major-mode 'dired-mode) "emacs")
           ((string-match-p "Documents/org-files" default-directory) "org")
           ((find-group-for-buffer (current-buffer)) (find-group-for-buffer (current-buffer)))
           ((null (tabbar-current-tabset))
            (let ((buf (current-buffer))
                  (prev-group nil))
              (switch-to-buffer (other-buffer (current-buffer) 1))
              (setq prev-group (symbol-name (tabbar-current-tabset)))
              (switch-to-buffer buf)
              (cl-loop for group in my-tabbar-groups do
                       (when (string-equal (car group) prev-group)
                         (return (push buf (cdr group)))))
              prev-group))
           (t "other"))))
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

  (defun my-tabbar-move-current-tab-one-place (direction)
    "Move current tab one place. DIRECTION must be :LEFT or :RIGHT."
    ;; bufset == (list  (buffer . (group-list)) ...)
    (let* ((bufset (tabbar-current-tabset t))
           (old-bufs (tabbar-tabs bufset))
           (first-buf (car old-bufs))
           (last-buf (car (last old-bufs)))
           (new-bufs (list)))
      (ecase direction
        (:left
         (unless (string= (buffer-name) (format "%s" (car first-buf)))
           (setq not-yet-this-buf first-buf)
           (setq old-bufs (cdr old-bufs))
           (while (and old-bufs
                       (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
             (push not-yet-this-buf new-bufs)
             (setq not-yet-this-buf (car old-bufs))
             (setq old-bufs (cdr old-bufs)))
           (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
               (progn
                 (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
                 (push not-yet-this-buf new-bufs)
                 (setq new-bufs (reverse new-bufs))
                 (setq new-bufs (append new-bufs (cdr old-bufs))))
             (error "Error: current buffer's name was not found in Tabbar's buffer list."))))
        (:right
         (while (and
                 old-bufs
                 (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
           (push (car old-bufs) new-bufs)
           (setq old-bufs (cdr old-bufs)))
         (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
             (progn
               (setq the-buffer (car old-bufs))
               (setq old-bufs (cdr old-bufs))
               (if old-bufs ; if this is false, then the current tab is the rightmost
                   (push (car old-bufs) new-bufs))
               (push the-buffer new-bufs)) ; this is the tab that was to be moved
           (error "Error: current buffer's name was not found in Tabbar's buffer list."))
         (setq new-bufs (reverse new-bufs))
         (setq new-bufs (append new-bufs (cdr old-bufs)))))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))

  (general-def
    :states '(normal)
    "gT" #'tabbar-backward-tab
    "gt" #'tabbar-forward-tab))

;; interactive mode toggling
(progn
  (defvar interactive-perspectives '()
    "list of (major-mode interactive-mode launch-interactive-mode-function).
The first two elements must be a 1:1 unique mapping of major-modes.")
  (setq interactive-perspectives
        (list (list "lisp-mode" "slime-repl-mode" #'slime)
              (list "sh-mode" "term-mode" #'get-term)
              (list "groovye-mode" "inferior-groovy-mode" #'run-groovy)
              (list "java-mode" "inferior-groovy-mode" #'run-groovy)
              (list "sql-mode" "sql-interactive-mode" #'sql-postgres)))

  (defun toggle-or-start-interaction (interactive-mode launch-interaction-fn)
    (let ((interactive-buffers (list)))
      (dolist (buffer (buffer-list))
        (let ((buffer-major-mode (with-current-buffer buffer major-mode)))
          (when (string= interactive-mode buffer-major-mode)
            (push buffer interactive-buffers))))
      (cond
       ((= 0 (length interactive-buffers))
        (funcall launch-interaction-fn))
       ((= 1 (length interactive-buffers))
        (switch-to-buffer-other-window (first interactive-buffers)))
       (t (message "TODO: implement interactive buffer prompt")))))

  (defun toggle-interact-with-buffer ()
    (interactive)
    (let ((current-buffer-major-mode (message "%s" major-mode)))
      (unless
          (or ;; first try to launch the interaction for the current buffer
           (loop for entry in interactive-perspectives do
                 (let ((edit-mode (first entry))
                       (interact-mode (second entry))
                       (launch-interaction-fn (third entry)))
                   (cond
                    ((string= current-buffer-major-mode edit-mode)
                     (toggle-or-start-interaction interact-mode launch-interaction-fn)
                     (return t))
                    ((string= current-buffer-major-mode interact-mode)
                     (if (> (count-windows) 1)
                         (delete-window)
                       (bury-buffer))
                     (return t)))))
           ;; no interaction for buffer. Toggle first found interactive buffer
           (let ((interactive-major-modes (mapcar #'second interactive-perspectives)))
             (loop for buffer in (buffer-list) do
                   (let ((buffer-major-mode (with-current-buffer buffer
                                              (message "%s" major-mode))))
                     (when (find buffer-major-mode interactive-major-modes :test #'string=)
                       (toggle-or-start-interaction buffer-major-mode (lambda))
                       (return t))))))
        (message "No interactive mode for %s" current-buffer-major-mode))))
  (general-define-key
   "<f9>" #'toggle-interact-with-buffer))

;; IDE hydra
(progn
  (defmacro mode-case (&rest body)
    "Execute a different body depending on the active major-mode."
    `(pcase major-mode
       ,@body
       (unhandled-mode (warn (format "No handler for mode: %s" unhandled-mode)))))

  (defhydra hydra-ide (:color amaranth :columns 1)
    "IDE Actions"
    ("h" (mode-case
          ('java-mode (lsp-ui-peek-find-implementation))
          ('lisp-mode (slime-browse-classes (slime-read-symbol-name "Class Name: "))))
     "Type Hierarchy" :exit t)
    ("r" (mode-case
          ('java-mode (lsp-ui-peek-find-references)))
     "References" :exit t)
    ("g" (mode-case
          ('java-mode (lsp-ui-peek-find-definitions)))
     "Go to definition" :exit t)
    ;; ("d"  (my-ide-documentation) "Documentation" :exit t)
    ;; ("i"  (my-ide-interaction) "Interaction (repl, shell)" :exit t)
    ;; ("t"  (my-ide-test-repeat) "test re-run" :exit t)
    ;; ("T"  (my-ide-test) "Test thing at point" :exit t)
    ("q"  nil "Cancel" :color red))

  (general-def
    :states '(normal insert)
    "C-x i" #'hydra-ide/body
    "M-." (lambda ()
            (interactive)
            (mode-case
             ('emacs-lisp-mode (call-interactively #'find-function-at-point)
                               ;; delete extra window created by find-function-at-point
                               (delete-window))
             ('lisp-mode (call-interactively #'slime-edit-definition))
             ('java-mode (call-interactively #'lsp-ui-peek-find-definitions))))))

;; comint bindings
(progn
  (general-def 'insert comint-mode-map
    "C-r" #'comint-history-isearch-backward-regexp
    "C-d" (lambda ()
            (interactive)
            (let ((proc (get-buffer-process (current-buffer))))
              (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
                  (progn (comint-send-eof)
                         (sleep-for .200)
                         (kill-buffer-and-window))
                (delete-char arg))))
    "C-c" #'comint-kill-input
    "C-j" (general-simulate-keys "M-n")
    "C-k" (general-simulate-keys "M-p")))

(use-package ansi-color)

;;Slime
(use-package slime
  :delight slime
  :commands (slime slime-connect)
  :init
  (defun print-help ()
    (print "No override. Check for .custom.el?"))
  (defun single-test ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help))
  (defun run-unit-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help)
    nil)
  (defun run-integration-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help)
    nil)
  (defun run-performance-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help)
    nil)
  (defun reload-systems ()
    "Delete packages and reload asdf systems."
    (interactive)
    (print-help)
    nil)
  :config
  (progn
    ;; emit ansi colors in slime repl
    (defvar slime-colors t "If non-nil, emit ansi colors in the slime repl.")
    (defun slime-colors-on ()
      "Enable ansi colors in the slime repl."
      (interactive)
      (setq slime-colors t))
    (defun slime-colors-off ()
      "Disable ansi colors in the slime repl."
      (interactive)
      (setq slime-colors nil))
    (defadvice slime-repl-emit (around slime-repl-ansi-colorize activate compile)
      (with-current-buffer (slime-output-buffer)
        (setq ad-return-value ad-do-it)
        (when slime-colors
          (ansi-color-apply-on-region slime-output-start
                                      slime-output-end)))))
  (use-package slime-company)
  (make-directory "/tmp/slime-fasls/" t)
  (setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))


  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q -l ~/.sbclrc run")
  (slime-setup '(slime-fancy
                 slime-highlight-edits
                 slime-asdf
                 slime-xref-browser
                 slime-company))
  ;; This is a hack. slime-setup should enable slime-company
  (slime-company-init)

  (load "~/.roswell/lisp/quicklisp/clhs-use-local.el" t)

  (setq-default inhibit-splash-screen t)

  ;; stop slime from complaining about version mismatch
  (setq-default slime-protocol-version 'ignore)

  (defun my-slime-repl-kill-or-interrupt ()
    "If the user has entered text in the prompt, remove the text before and after point.
Otherwise, send an interrupt to slime."
    (interactive)
    (cond ((= (marker-position slime-repl-input-start-mark) (point))
           (slime-interrupt))
          (t (slime-repl-kill-input)
             (slime-repl-kill-input))))

  (general-def 'normal slime-repl-mode-map
    "q" (lambda ()
          (interactive)
          (end-of-buffer)
          (evil-insert-state)
          (toggle-interact-with-buffer))
    [return]  #'slime-inspect-presentation-at-point
    "C-<return>" #'slime-expand-1)

  ;; macroexpansion keys not working
  ;; defaults are C-c <ret> == exapnd, C-/ == undo
  (general-def 'normal slime-macroexpansion-minor-mode-map
    "u" #'slime-macroexpand-undo
    "C-<return>" #'slime-expand-1-inplace)

  (general-def 'insert slime-repl-mode-map
    "C-c" #'my-slime-repl-kill-or-interrupt
    "C-d" (lambda () (interactive)
            (when (y-or-n-p "Quit slime?")
              (and (slime-repl-quit) (delete-window))))
    "C-r" #'slime-repl-previous-matching-input
    "TAB" #'completion-at-point
    "C-S-l" #'slime-repl-clear-buffer
    "C-k" #'slime-repl-previous-input
    "C-j" #'slime-repl-next-input)

  (general-def 'normal lisp-mode-map
    "<f4>" #'slime-browse-classes
    "M-." 'slime-edit-definition)

  ;; slime xref browser evil bindings
  (general-def 'normal slime-browser-map
    "j" 'widget-forward
    "k" 'widget-backward
    "M-." (lambda () (interactive)
            (end-of-line)
            (slime-edit-definition (slime-symbol-at-point)))
    "q" 'bury-buffer)

  ;; sldb evil bindings
  (general-def 'normal sldb-mode-map
    "C-j" (general-simulate-keys "n" t)
    "C-k" (general-simulate-keys "p" t)
    "j" (general-simulate-keys "C-n" t)
    "k" (general-simulate-keys "C-p" t)
    "l" (general-simulate-keys "C-f" t)
    "h" (general-simulate-keys "C-b" t)
    "0" (general-simulate-keys "0" t)
    "1" (general-simulate-keys "1" t)
    "2" (general-simulate-keys "2" t)
    "3" (general-simulate-keys "3" t)
    "4" (general-simulate-keys "4" t)
    "5" (general-simulate-keys "5" t)
    "6" (general-simulate-keys "6" t)
    "7" (general-simulate-keys "7" t)
    "8" (general-simulate-keys "8" t)
    "9" (general-simulate-keys "9" t)
    "v" (general-simulate-keys "v" t))

  ;; slime xref evil bindings

  (general-def 'normal slime-xref-mode-map
    "j" #'slime-xref-next-line
    "k" #'slime-xref-prev-line
    "RET" (general-simulate-keys "RET" t))

  ;; evil keys for slime inspector
  (evil-set-initial-state 'slime-inspector-mode 'normal)
  (general-def 'normal slime-inspector-mode-map
    "q" (lambda ()
          "Reinspect the previous object or close the window if there is no previous object"
          ;; mostly copied from slime-inspector-pop
          (interactive)
          (let ((result (slime-eval `(swank:inspector-pop))))
            (if result
                (slime-open-inspector result (pop slime-inspector-mark-stack))
              (quit-window)))))

  (load-if-exists "~/.roswell/lisp/quicklisp/dists/quicklisp/software/cl-annot-20150608-git/misc/slime-annot.el"))

(use-package magit
  :general
  ("C-x g" 'magit-status)
  :config
  (setq-default magit-completing-read-function 'ivy-completing-read)
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (setq-default magit-push-always-verify nil)
  (setq-default magit-fetch-arguments '("--prune"))
  (setq-default magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (put 'magit-clean 'disabled nil)
  (use-package evil-magit)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (boundp-and-true magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package git-link
  :config
  (setq git-link-default-branch "master"))

(use-package hideshow
  :delight hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook 'hs-hide-initial-comment-block)
  :config

  (defun end-of-line-before-comment ()
    "Move the end of the line, or just before a comment character if the line ends in a comment."
    (when (comment-search-forward (line-end-position) t)
      (goto-char (match-beginning 0))
      (skip-syntax-backward " " (line-beginning-position))
      (backward-char)))

  (general-def 'normal hs-minor-mode-map
    "<tab>" (lambda ()
              (interactive)
              (save-excursion
                (end-of-line-before-comment)
                (hs-toggle-hiding)))))

(use-package multi-term
  :defer t
  :init
  (setq-default multi-term-program "/bin/zsh")
  (defun last-term-buffer (l)
    "Return most recently used term buffer from the list of buffers, \"L\"."
    (when l
      (if (eq 'term-mode (with-current-buffer (car l) major-mode))
          (car l) (last-term-buffer (cdr l)))))
  (defun get-term ()
    "Switch to the term buffer last used, or create a new one if none exists, or if the current buffer is already a term."
    (interactive)
    (let ((b (last-term-buffer (buffer-list))))
      (if (or (not b) (eq 'term-mode major-mode))
          (multi-term)
        (switch-to-buffer b))))
  ;; :general
  ;; ("C-x s" #'get-term)
  :config
  ;; might want to later set up auto term-line-mode and term-char-mode
  ;; depending on evil state
  (evil-define-key 'normal term-raw-map
    (kbd "p") 'term-paste
    (kbd "RET") 'term-send-return
    (kbd "C-x d") 'term-send-eof
    (kbd "C-d") 'term-send-eof)

  (evil-define-key 'insert term-raw-map
    (kbd "C-c") 'term-interrupt-subjob
    (kbd "C-r") 'term-send-reverse-search-history
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down
    (kbd "C-v") 'term-paste
    (kbd "C-d") 'term-send-eof
    (kbd "C-a") 'term-send-raw))

(use-package eshell
  :general
  ("C-x s" #'eshell)
  :config
  ;; esh-mode.el:303 eshell-mode-map is defined locally
  ;; must use a hook to modify it
  (defun my-eshell-hook ()
    (general-def 'insert eshell-mode-map
      "C-r" #'eshell-previous-matching-input
      "C-l" #'eshell/clear
      "C-c" #'eshell-interrupt-process
      "C-d" (lambda ()
              (interactive)
              (eshell-send-eof-to-process)
              (if (= 1 (length (window-list)))
                  (kill-buffer)
                (kill-buffer-and-window)))
      "C-j" #'eshell-next-input
      "C-k" #'eshell-previous-input)

    (if (boundp 'eshell-visual-commands)
        (add-to-list 'eshell-visual-commands "htop")
      (setq-default eshell-visual-commands '("htop")))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (eshell/clear-scrollback)
        (eshell-send-input)))
    ;; (defun eshell/gradle ()
    ;;   (find-file "./gradlew"))
    (eshell/alias 'll 'ls '-l))

  (add-hook 'eshell-mode-hook #'my-eshell-hook))

;; irc client
(use-package erc
  :ensure t
  :defer t
  :init
  (setq-default erc-nick "_ark_"
                erc-server-reconnect-timeout 5
                erc-server-reconnect-attempts 3
                erc-prompt-for-nickserv-password nil)
  :config
  (use-package erc-hl-nicks :ensure t)
  (require 'erc-services)
  (erc-services-mode 1))

;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  :delight org-indent-mode nil org-indent
  :init
  (defhydra hydra-orgmode (:color amaranth :columns 1)
    "Org Mode"
    ("a"  (org-agenda) "Agenda" :exit t)
    ("c"  (cfw:open-calendar-buffer
           :contents-sources
           (list
            (cfw:org-create-source "Green")))
     "Weekly calendar" :exit t)
    ("C"  (org-capture) "Capture" :exit t)
    ("q"  nil "Cancel" :color red))
  :general
  ("C-x c" #'hydra-orgmode/body)
  :config
  (setq-default org-startup-indented t)
  ;; fontify code in code blocks
  (setq-default org-src-fontify-natively t)
  (setq-default org-directory "~/Documents/org-files/")
  (setq-default org-agenda-files (list (concat org-directory "tasks.org")))
  (setq-default org-default-notes-file (concat org-directory "notes.org"))
  (setq-default org-log-done 'time)
  (setq-default org-enforce-todo-dependencies t)

  (setq-default org-startup-with-inline-images t)
  (setq-default org-image-actual-width nil)

  (setq-default org-refile-targets '((nil :maxlevel . 9)))
  (setq-default org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq-default org-refile-use-outline-path t) ; Show full paths for refiling
  (setq-default org-capture-templates
                `(("s" "Syn Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "syn")
                   "* TODO %?")
                  ("l" "Life Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "life")
                   "* TODO %?")
                  ("d" "Datadog Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "datadog")
                   "* TODO %?")
                  ("m" "Misc" entry (file ,(concat org-directory "orgzly/" "refile.org"))
                   "* %?")))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (use-package evil-org
    :delight
    evil-org-mode
    :general
    (general-evil-define-key 'insert evil-org-mode-map
      "M-h"     #'org-metaleft
      "M-l"     #'org-metaright)
    (general-evil-define-key 'normal evil-org-mode-map
      "S-h"     #'org-shiftleft
      "S-l"     #'org-shiftright)
    :init (add-hook 'org-mode-hook (lambda () (evil-org-mode 1))))
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-trello
  :mode ("\\.trello$" . org-mode)
  :delight org-trello-mode
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (when (string-suffix-p ".trello" (buffer-name))
                               (org-trello-mode))))
  (defun org-trello-personal-fetch-buffer ()
    "Fetch data from trello and populate the buffer"
    (interactive)
    (org-trello-sync-buffer t)))

(use-package calfw
  :config
  (use-package calfw-org)
  (use-package calfw-ical)

  ;; default calendar view to week instead of month
  (defun my--cfw:open-calendar-buffer-view (orig-func &rest args &allow-other-keys)
    (apply orig-func :view 'week :allow-other-keys t args))
  (advice-add 'cfw:open-calendar-buffer :around #'my--cfw:open-calendar-buffer-view))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :general
  (general-def 'normal markdown-mode-map
    "TAB" #'markdown-cycle)
  :init (setq-default markdown-command "multimarkdown"))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Flycheck
(use-package flycheck
  :delight flycheck-mode
  :config
  (global-flycheck-mode)
  (delight 'flyspell-mode nil 'flyspell)
  (delight 'flyspell-prog-mode nil 'flyspell)
  ;; spellcheck
  (mapc (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
        '(emacs-lisp-mode-hook
          lisp-mode-hook
          clojure-mode-hook
          java-mode-hook
          scala-mode-hook
          groovy-mode-hook))

  (mapc (lambda (mode-hook) (add-hook mode-hook 'flyspell-mode))
        '(org-mode-hook
          markdown-mode-hook
          text-mode-hook)))

;; ctags
(setq-default path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file for the project in \"DIR-NAME\"."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'lisp-mode-common-hook  'turn-on-ctags-auto-update-mode)

;;;;;;;;;;;;;;

(use-package gradle-mode
  :mode (("\\.java$" . java-mode)
         ("\\.gradle$" . groovy-mode)
         ("\\.groovy$" . groovy-mode))
  :init
  (add-hook 'java-mode-hook #'gradle-mode)
  :config
  (gradle-mode 1))

(use-package groovy-mode
  :mode
  ("\\.gradle$" . groovy-mode)
  ("\\.groovy$" . groovy-mode)
  :init
  (add-hook 'groovy-mode-hook 'hs-minor-mode)
  (add-hook 'groovy-mode-hook 'rainbow-delimiters-mode)
  :config
  (setq-default groovy-indent-offset 2)
  ;; (let ((my-groovy-args '()))
  ;;   )
  ;; (setq groovysh-args  )
  )

(use-package scala-mode
  :mode
  ("\\.scala$" . scala-mode))

(use-package jdecomp
  :mode ("\\.class$" . jdecomp-mode)
  :config
  (customize-set-variable 'jdecomp-decompiler-type 'procyon)
  (customize-set-variable 'jdecomp-decompiler-paths
                          `((procyon . ,(first-existing-file "/usr/share/java/procyon-decompiler/procyon-decompiler.jar"
                                                             "/opt/procyon/procyon-decompiler-0.5.30.jar"
                                                             "/usr/local/Cellar/procyon-decompiler/0.5.30/libexec/procyon-decompiler-0.5.30.jar"))))
  (defun my-jdecomp-bytecode ()
    "Decompile the current file into raw bytecode"
    (interactive)
    (let ((jdecomp-decompiler-options
           '((procyon "-r -ln -ss"""))))
      (jdecomp-decompile-and-view (buffer-file-name)))))

(use-package thread-dump
  :commands (thread-dump-open-file thread-dump-open-files thread-dump-open-dir))

(use-package sql
  :mode ("\\.sql$" . sql-mode)
  :general
  (general-evil-define-key 'insert sql-interactive-mode-map
    "C-S-r" #'comint-history-isearch-backward-regexp
    "C-l" #'comint-clear-buffer
    "C-k" #'comint-previous-input
    "C-j" #'comint-next-input
    "C-d" (lambda ()
            (interactive)
            (when (yes-or-no-p "Quit Sql session?")
              (comint-delchar-or-maybe-eof 0)
              (delete-window))))
  (general-evil-define-key '(insert normal) sql-mode-map
    "C-c C-c" #'sql-send-paragraph)
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq-default sql-buffer (get-buffer "*SQL*"))
              (toggle-truncate-lines t)))

  (use-package sql-indent
    :init (add-hook 'sql-mode-hook 'sql-indent))

  (use-package sqlup-mode
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    :config
    (add-to-list 'sqlup-blacklist "name"))

  (sql-set-sqli-buffer-generally)

  (make-directory "~/.emacs.d/sql" :parents)
  (assert (file-exists-p "~/.emacs.d/sql")
          T
          "Unable to create or find sql history dir.")
  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook))

(use-package aggressive-indent
  :delight aggressive-indent-mode
  :config
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (let ((modes-to-indent '(lisp-mode-hook emacs-lisp-mode-hook)))
    (mapc (lambda (hook)
            (add-hook hook #'aggressive-indent-mode))
          modes-to-indent))

  ;; For cc-modes don't indent line below until ending ';' is entered
  (defvar semicolon-delimited-modes (list 'java-mode 'c-mode))

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (remove-if-not (lambda (mode-name)
                          (derived-mode-p mode-name))
                        semicolon-delimited-modes)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)(\(.*\)\))?\\b\\)"
                             (thing-at-point 'line)))))
  (add-to-list
   'aggressive-indent-dont-indent-if
   'company-candidates))

(use-package lsp-mode
  :config
  (use-package lsp-ui
    :general
    (:keymaps 'lsp-ui-peek-mode-map
              "M-j" #'lsp-ui-peek--select-next
              "M-k" #'lsp-ui-peek--select-prev)
    :config
    (setq-default lsp-ui-sideline-enable nil
                  lsp-ui-peek-list-width 130)
    (lsp-ui-sideline-mode -1))
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)

  (use-package company-lsp
    :after company
    :config
    (setq company-lsp-enable-snippet t
          company-lsp-cache-candidates t)
    ;; (push 'java-mode company-global-modes)
    (push 'company-lsp company-backends))

  (use-package lsp-intellij)
  (add-hook 'java-mode-hook
            (lambda ()
              (lsp-intellij-enable))))

;; Finally, apply host and project custom settings

(let ((project-customizations nil))
  (ignore-errors
    ;; projectile-project-root will throw an error outside of projectile
    (load-if-exists (load-if-exists (concat (projectile-project-root) "./.custom.el")))))

(server-start)

;;; init.el ends here
