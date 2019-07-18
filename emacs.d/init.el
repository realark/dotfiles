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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-initial-delay 0)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      backup-by-copying t)

;; automatic reload from disk
(global-auto-revert-mode t)

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

  (defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "all buffers refreshed"))

  (defun select-current-line ()
    "Select the current line."
    (end-of-line)                       ; move to end of line
    (set-mark (line-beginning-position)))

  (defun toggle-comment-region-or-line ()
    "Comment or uncomment the selected region.  If no region is selected use the current line."
    (interactive)
    (if (not mark-active)
        (select-current-line))
    (comment-or-uncomment-region (region-beginning) (region-end)))

(defun current-line-starts-with (char)
  (let (line-starts-with-char)
    (save-excursion
      (beginning-of-line)
      (setq line-starts-with-char (string-equal (thing-at-point 'char) char)))
    line-starts-with-char)))

(require 'cl)

;; Change startup messages
(progn
  (defun display-startup-echo-area-message ()
    "Change the startup message."
    (message "(oo)"))

  (setq initial-scratch-message
        (concat
         (replace-regexp-in-string
          "^" ";; "                     ; comment each line
          (replace-regexp-in-string
           "\n$" ""                     ; remove trailing linebreak
           (get-string-from-file "/etc/motd")))
         "\n\n")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use mepla and marmalade for package
(eval-when-compile
  (defmacro require-install (PCK)
    "Require package PCK, install via \"package-install\" if missing."
    `(unless (require ,PCK nil t)
       (package-install ,PCK)
       (require ,PCK)))
  (require-install 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; a fresh setup will fail here. Run this block manually and at this point run: M-x package-refresh-contents RET
  (package-initialize)
  (require-install 'use-package)
  (setq-default use-package-always-ensure t)
  (setq-default use-package-always-defer t)

  (use-package try
    :commands try))

(progn ; settings for osx
  (setq-default mac-command-modifier 'meta)

  (use-package exec-path-from-shell
    :defer nil
    :if (memq window-system '(ns))
    :config
    (exec-path-from-shell-initialize)))

;; Themes and UI tweaks
(progn
  (use-package powerline
    :defer nil)

  (use-package delight
    :defer nil
    :config
    (delight 'eldoc-mode nil 'eldoc)
    (delight 'undo-tree-mode nil 'undo-tree)
    (delight 'auto-revert-mode nil 'autorevert))

  ;; Theme
  (defadvice load-theme (before theme-dont-propagate activate)
    "Reset to standard theme before switching to a new one"
    (mapc #'disable-theme custom-enabled-themes))

  (use-package moe-theme
    :ensure :defer
    :if window-system
    :load-path "themes"
    :config
    (moe-theme-set-color 'cyan)
    (powerline-moe-theme))

  (use-package cyberpunk-theme
    :ensure :defer
    :if window-system
    :load-path "themes")

  (use-package farmhouse-theme
    :ensure :defer
    :if window-system
    :load-path "themes")

  (use-package circadian
    :ensure t
    :if window-system)

  (setq-default circadian-themes '(("07:30" . deeper-blue)
                                   ("16:00" . cyberpunk)))
  (circadian-setup)

  ;; Maximize emacs window
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; winner mode allows easy undo/redo of window changes
  (winner-mode 1)

  ;; use ace-window
  (use-package ace-window
    :defer nil))

;;Evil (extensible vi layer for Emacs)
(use-package evil
  :defer nil
  :init
  (setq-default ;; Default to normal mode most of the time
                evil-insert-state-modes '(nrepl-mode shell-mode git-commit-mode term-mode eshell-mode)
                evil-emacs-state-modes '(magit-mode)
                evil-motion-state-modes '())
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :defer nil
  :config
  (with-eval-after-load 'ediff
    (require 'evil-collection-ediff)
    (evil-collection-ediff-setup)))

;; general for keybindings
(use-package general
  :defer nil
  :init
  (general-evil-setup t)
  :config
  (general-def
    "<escape>" (general-simulate-key "C-g")
    "C-+" #'text-scale-adjust)

  (defun widen-or-quit-window ()
    (interactive)
    (if (buffer-narrowed-p)
        (call-interactively #'widen)
      (call-interactively #'quit-window)))

  (general-def :states '(normal)
    "q" #'widen-or-quit-window
    "RET" (general-simulate-key "RET" :state 'emacs))

  (general-def
    :states '(normal visual)
    "/" (general-simulate-key "C-s")
    "<SPC>"  (general-simulate-key "C-x"))

  (general-def
    :prefix "C-x"
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
    ";"      #'toggle-comment-region-or-line))

;; Hydras
(use-package hydra
  :defer nil
  :config
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

  (general-def
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
     ("q" nil))))

(use-package smartparens
  :init
  (smartparens-global-strict-mode)
  :delight smartparens-mode
  :config
  (sp-pair "'" nil :actions nil)
  (sp-pair "`" nil :actions nil))

(use-package lispyville
  :delight lispyville-mode
  :hook ((lisp-mode . lispyville-mode)
         (emacs-lisp-mode . lispyville-mode)
         (eval-expression-minibuffer-setup . lispyville-mode)
         (ielm-mode . lispyville-mode)
         (lisp-interaction-mode . lispyville-mode)
         (sly-mrepl-mode . lispyville-mode)
         (scheme-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators
                              (escape insert)
                              (slurp/barf-cp normal)
                              (additional normal)
                              (additional-movement normal visual motion))))

;; ivy and friends
(progn
  (use-package ivy
    :demand t
    :delight ivy-mode
    :init
    (ivy-mode t)
    :general
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
    (setq-default ivy-wrap t
                  ivy-use-virtual-buffers t
                  ivy-height 10
                  ivy-count-format "(%d/%d) "
                  ivy-extra-directories nil
                  ivy-re-builders-alist '((t . ivy--regex-plus))))

  (use-package swiper
    :demand t
    :general
    ("C-s" #'swiper)
    :init
    (setq-default swiper-action-recenter t)
    :config
    ;; isearch-forward seems to get stuck in the wrong state with swiper.
    ;; Manually setting it to T seems to work around the problem.
    ;; https://github.com/emacs-evil/evil/issues/712
    ;; This is probably a problem with my config
    (setq isearch-forward t))

  (use-package counsel
    :demand t
    :general
    ("M-x" #'counsel-M-x
     "C-x <SPC>" #'counsel-find-file
     "C-x C-f" #'counsel-find-file)))

;; company mode
(use-package company
  :demand t
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
    (setq-default company-idle-delay 1.00)
    (setq-default company-minimum-prefix-length 3)))


;; Options for M-x rgrep
(use-package grep
  :commands (grep)
  :config
  (when (boundp 'grep-find-ignored-files)
    (mapc (lambda (file-regex)
            (add-to-list 'grep-find-ignored-files file-regex))
          '("*.fasl"
            "*.class")))
  (when (boundp 'grep-find-ignored-directories)
    (mapc (lambda (dir-regex)
            (add-to-list 'grep-find-ignored-directories dir-regex))
          '("target"
            "build"
            "bin"))))

;; Projectile
(use-package projectile
  :demand t
  :init
  (projectile-mode)
  :config
  (setq-default projectile-completion-system 'ivy
                projectile-globally-ignored-directories (append projectile-globally-ignored-directories
                                                                '(".git" ".ensime_cache.d" ".gradle"
                                                                  ".recommenders" ".metadata" "dist"))
                projectile-globally-ignored-files (append projectile-globally-ignored-files
                                                          '(".ensime" "*.war" "*.jar" "*.zip"
                                                            "*.png" "*.gif" "*.vsd" "*.svg"
                                                            "*.exe" "eclimd.log" "workbench.xmi"
                                                            ".emacs.desktop" "*.deb" "*.gz" "*.fasl"))
                projectile-enable-caching t)

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
                :after #'%run-projectile-invalidate-cache))

  (general-def
   "C-x p"
   (defhydra hydra-projectile (:color blue :hint nil)
     "
     PROJECTILE: %(or (ignore-errors (projectile-project-root)) \"(Not in a project)\")

 Find/Replace         Tasks                   Buffers
----------------------------------------------------------------------------------
 _f_: file find         _t_: test project     _k_: Kill all buffers
 _T_: find tag          _c_: command run
 _g_: grep all files
 _r_: replace
 _R_: replace regex

"
     ("f"   projectile-find-file)
     ("T"   projectile-find-tag)
     ("g"   projectile-grep)
     ("r"   projectile-replace)
     ("R"   projectile-replace-regexp)
     ("t"   projectile-test-project)
     ("c"   projectile-run-project)
     ("k"   projectile-kill-buffers)
     ("q"   nil "Cancel" :color red))))

;; Flycheck
(use-package flycheck
  :demand t
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

;; yasnippet
(use-package yasnippet
  :demand t
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


(use-package dired-sidebar
  :general ("<f8>" #'dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (setq-default dired-sidebar-should-follow-file t))


(progn
  ;; TODO: Once on melpa
  ;; use-package torus
  ;; :config
  (load-file "~/tmp/torus/torus.el")

  ;; torus groups buffers.
  ;; location = (filename . position)
  ;; circle = group of locations (i.e. buffers)
  ;; torus = group of all circles
  :config
  ;; Created if non existent
  (setq-default torus-dirname "~/.emacs.d/torus/"
                torus-save-on-exit t
                torus-history-maximum-elements 30
                torus-maximum-horizontal-split 3
                torus-maximum-vertical-split 3)

  (torus-init))

(use-package tabbar
  :demand t
  :init
  (tabbar-mode t)
  :general
  ("C-x t" #'hydra-tabs/body)
  :config
  (defhydra hydra-tabs (:color amaranth :hint nil)
    "
TABS: %(buffer-name) -- %(car (car torus-torus))
-------------------------------------------------------------
 Move:        _L_:right _H_:left
 Switch:      _l_:right _h_:left _k_:up-group _j_:down-group
 Manage:      _c_reate-torus-circle _C_:create-location _t_:switch-torus-circle
EOF"
    ("l" tabbar-forward-tab)
    ("h" tabbar-backward-tab)
    ("j" tabbar-backward-group)
    ("k" tabbar-forward-group)
    ("L" (my-tabbar-move-current-tab-one-place :right))
    ("H" (my-tabbar-move-current-tab-one-place :left))
    ("c" torus-add-circle :exit t)
    ("C" torus-add-location :exit t)
    ("t" torus-switch-circle :exit t)
    ("s" (error "TODO") :exit t)
    ("q" nil))

  (setq frame-title-format '("emacs <" (:eval (symbol-name tabbar-current-tabset)) "> - %b"))

  (set-face-foreground 'tabbar-selected "white")
  (set-face-background 'tabbar-selected "DarkBlue")

  ;; tabbar-current-tabset  == current tab set

  (defun my-torus-circle-for-buffer (file-path)
    (cl-loop for circle-list in torus-torus do
             (let ((circle-for-buffer (cl-loop for location in (rest circle-list) do
                                               (when (string-equal file-path (car location))
                                                 (return (car circle-list))))))
               (when circle-for-buffer (return circle-for-buffer)))))

  (defun my-tabbar-buffer-groups ()
    "Show all normal files in one group"
    (list
     (cond ((and (buffer-file-name)
                 (my-torus-circle-for-buffer (file-truename (buffer-file-name))))
            (my-torus-circle-for-buffer (file-truename (buffer-file-name))))
           ((eq major-mode 'erc-mode) "IRC")
           ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
           ((string-prefix-p "TAGS" (buffer-name)) "emacs")
           ((string-prefix-p "magit" (buffer-name)) "emacs")
           ((eq major-mode 'dired-mode) "emacs")
           ((eq major-mode 'dired-sidebar-mode) "emacs")
           ((eq major-mode 'org-mode) "org")
           (t "no-group"))))
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
        (list (list "lisp-mode" "sly-mrepl-mode" #'sly)
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
  (general-def
   "<f9>" #'toggle-interact-with-buffer))

;; IDE hydra
(progn
  (defmacro mode-case (&rest body)
    "Execute a different body depending on the active major-mode."
    `(pcase major-mode
       ,@body
       (unhandled-mode (warn (format "No handler for mode: %s" unhandled-mode)))))

  (defun single-test ()
    "Run or rerun a single test."
    (interactive)
    (message "Not implemented for current mode. Override with dir local."))

  (defun %my-go-to-definition ()
    (interactive)
    (mode-case
     ('emacs-lisp-mode (call-interactively #'find-function-at-point)
                       ;; delete extra window created by find-function-at-point
                       (delete-window))
     ('lisp-mode (call-interactively #'sly-edit-definition))
     ('sly-mrepl-mode (call-interactively #'sly-edit-definition))
     ('java-mode (call-interactively #'lsp-ui-peek-find-definitions))))

  (defhydra hydra-ide (:color amaranth :columns 1)
    "IDE Actions"
    ("h" (mode-case
          ('java-mode (lsp-ui-peek-find-implementation))
          ;; ('lisp-mode (slime-browse-classes (slime-read-symbol-name "Class Name: ")))
          )
     "Type Hierarchy" :exit t)
    ("r" (mode-case
          ('java-mode (lsp-ui-peek-find-references))
          ('lisp-mode (call-interactively #'sly-who-calls)))
     "References" :exit t)
    ("g" (%my-go-to-definition)
     "Go to definition" :exit t)
    ;; ("d"  (my-ide-documentation) "Documentation" :exit t)
    ;; ("i"  (my-ide-interaction) "Interaction (repl, shell)" :exit t)
    ;; ("t"  (my-ide-test-repeat) "test re-run" :exit t)
    ("i"  (single-test) "Test thing at point" :exit t)
    ("q"  nil "Cancel" :color red))

  (general-def
    :states '(normal insert)
    "C-x i" #'hydra-ide/body
    "M-." #'%my-go-to-definition))

(use-package ansi-color
  :demand t)

(progn
  (use-package sly
    :hook ((sly-mrepl-mode . my-sly-post-connect))
    :general
    (:states 'insert :keymaps 'sly-mrepl-mode-map
             ;; insert a newline instead of evaluating the expression
             "S-<return>" #'newline-and-indent
             "C-c"     (lambda ()
                         (interactive)
                         (end-of-buffer)
                         (comint-kill-input))
             "C-r"     #'comint-history-isearch-backward-regexp
             "C-d"     #'sly-quit-lisp
             "C-S-l"   #'comint-clear-buffer
             "C-k"     #'comint-previous-input
             "C-j"     #'comint-next-input)
    (:states 'normal :keymaps 'sly-db-mode-map
             "i" #'evil-insert
             "0" (general-simulate-key "0" :state 'emacs)
             "1" (general-simulate-key "1" :state 'emacs)
             "2" (general-simulate-key "2" :state 'emacs)
             "3" (general-simulate-key "3" :state 'emacs)
             "4" (general-simulate-key "4" :state 'emacs)
             "5" (general-simulate-key "5" :state 'emacs)
             "6" (general-simulate-key "6" :state 'emacs)
             "7" (general-simulate-key "7" :state 'emacs)
             "v" (general-simulate-key "v" :state 'emacs)
             ":" #'sly-db-pprint-eval-in-frame)
    (:states 'normal :keymaps 'sly-inspector-mode-map
             "Q" #'sly-inspector-quit
             "q" #'sly-inspector-pop
             ":" #'sly-inspector-eval)
    ;; (general-def :states 'normal :keymaps 'slime-xref-mode-map
    ;;   "j" #'slime-xref-next-line
    ;;   "k" #'slime-xref-prev-line)
    :config
    (load (expand-file-name "~/.roswell/helper.el"))
    ;; (load "~/.roswell/lisp/quicklisp/clhs-use-local.el" t)
    (setq inferior-lisp-program "ros -Q -l ~/.sbclrc run")

    (setq sly-compile-file-options '(:fasl-directory "/tmp/sly-fasls/"))

    (defun my-sly-post-connect ()
      (when (file-exists-p "~/.emacs.d/slime-init.lisp")
        (sly-eval-async (car (read-from-string (get-string-from-file "~/.emacs.d/slime-init.lisp")))))
      (when (file-exists-p (concat default-directory ".custom-slime-init.lisp"))
        (sly-eval-async (car (read-from-string (get-string-from-file (concat default-directory ".custom-slime-init.lisp"))))
                          (lambda (&rest args)
                            (when (file-exists-p (concat default-directory ".slime-initial-package"))
                              (sly-mrepl-sync-package-and-default-directory (get-string-from-file (concat default-directory ".slime-initial-package")))
                              (message "setting sly package"))
                            (message "custom sly init complete"))))))

  (use-package sly-quicklisp
    :demand t
    :after sly))

(progn ; magit and other git utils
  (use-package magit
    :general
    ("C-x g" #'magit-status
     "C-x G" #'magit-file-popup
     "C-x B" #'magit-blame)
    :config
    (setq-default magit-completing-read-function 'ivy-completing-read)
    (setq-default magit-last-seen-setup-instructions "1.4.0")
    (setq-default magit-push-always-verify nil)
    (setq-default magit-fetch-arguments '("--prune"))
    (setq-default magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
    (put 'magit-clean 'disabled nil)

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

  (use-package evil-magit
    :demand t
    :after magit)

  ;; no longer works due to lack of magit-popup in latest magit release
  ;; (use-package magithub
  ;;   :demand t
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t)
  ;;   (defun my-magithub-refresh ()
  ;;     (interactive)
  ;;     (magithub--refresh)))

  (use-package git-link
    :config
    (setq git-link-default-branch "master")))

(use-package hideshow
  :delight hs-minor-mode
  :hook ((prog-mode . hs-minor-mode)
         (hs-minor-mode . hs-hide-initial-comment-block))
  :init
  (defun end-of-line-before-comment ()
    "Move the end of the line, or just before a comment character if the line ends in a comment."
    (when (comment-search-forward (line-end-position) t)
      (goto-char (match-beginning 0))
      (skip-syntax-backward " " (line-beginning-position))
      (backward-char)))
  :general
  (:states '(normal) :keymaps 'hs-minor-mode-map
            "<tab>"   (lambda ()
                        (interactive)
                        (save-excursion
                          (end-of-line-before-comment)
                          (hs-toggle-hiding)))
            "<backtab>"     #'hs-toggle-hiding))

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
      "C-k" #'eshell-previous-input
      "S-<return>" #'company-complete)

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

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; irc client
(use-package erc
  :ensure t
  :defer t
  :init
  (setq-default erc-server-reconnect-timeout 5
                erc-server-reconnect-attempts 3
                erc-prompt-for-nickserv-password nil)

  :config
  (use-package erc-hl-nicks :ensure t)
  (require 'erc-services)
  (erc-services-mode 1))

(use-package znc
  :defer t
  :commands (znc-erc))

;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  :general
  ("C-x c" #'hydra-orgmode/body
   "C-x n" #'org-narrow-to-subtree)
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
  :config
  (setq-default org-todo-keywords '("TODO" "DOING" "DONE"))
  (setq-default org-startup-indented t)
  ;; fontify code in code blocks
  (setq-default org-src-fontify-natively t)
  (setq-default org-directory "~/Documents/org-files/")
  (setq-default org-agenda-files (list (concat org-directory "tasks.org")))
  (setq-default org-default-notes-file (concat org-directory "notes.org"))
  (setq-default org-log-done 'time)
  (setq-default org-enforce-todo-dependencies t)
  (setq-default org-catch-invisible-edits 'show-and-error)

  (setq-default org-startup-with-inline-images t)
  (setq-default org-image-actual-width nil)

  (setq-default org-log-repeat nil)

  (setq-default org-refile-targets '((nil :maxlevel . 9)))
  (setq-default org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq-default org-refile-use-outline-path t) ; Show full paths for refiling
  (setq-default org-capture-templates
                `(("s" "Syn Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "syn")
                   "* TODO %?")
                  ("l" "Life Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "life")
                   "* TODO %?")
                  ("m" "Misc" entry (file ,(concat org-directory "orgzly/" "refile.org"))
                   "* %?")))
  (setq-default org-duration-format (quote h:mm))
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package evil-org
  :hook ((org-mode . evil-org-mode)
         (org-trello-mode . evil-org-mode))
  :delight
  evil-org-mode
  :general
  (:states 'insert :keymaps 'evil-org-mode-map
           "M-h"     #'org-metaleft
           "M-l"     #'org-metaright)
  (:states 'normal :keymaps 'evil-org-mode-map
    "H" #'org-shiftleft
    "L" #'org-shiftright
    "K" #'org-shiftup
    "J" (lambda ()
            (interactive)
            (if (current-line-starts-with "*")
                (call-interactively #'org-shiftdown)
              (call-interactively #'evil-join)))
    "D" #'org-cut-subtree))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(use-package org-super-agenda
  :hook ((org-agenda-mode . org-super-agenda-mode))
  :general
  (general-def :keymaps 'org-super-agenda-header-map
    "j" #'evil-next-line
    "k" #'evil-previous-line)
  :config
  (setq-default org-super-agenda-header-separator "  --->"
                org-agenda-skip-scheduled-if-done t
                org-agenda-skip-deadline-if-done t
                org-super-agenda-groups
                '((:name "#### ACTIVE TASKS ####"
                   :todo ("DOING")
                   :face (:underline t))
                  (:name "Syn"
                   :category "syn")
                  (:auto-category t))))

(use-package org-trello
  :mode ("\\.trello$" . org-mode)
  :delight org-trello-mode
  :general
  (:keymaps 'org-trello-mode-map
            "C-x C-s"
            (lambda ()
              (interactive)
              (when my-org-trello-online
                (call-interactively #'org-trello-sync-buffer ))
              (call-interactively #'save-buffer)))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (when (string-suffix-p ".trello" (buffer-name))
                               (org-trello-mode))))
  (defun org-trello-personal-fetch-buffer ()
    "Fetch data from trello and populate the buffer"
    (interactive)
    (org-trello-sync-buffer t))
  (defvar my-org-trello-online t
    "Automatically sync org trello files when non-nil.")
  (add-hook 'org-trello-mode-hook
            (lambda ()
              (when my-org-trello-online
                (org-trello-sync-buffer t)))))

(progn ; calfw calendar
  (use-package calfw
    :commands (cfw:open-calendar-buffer cfw:org-create-source)
    :general
    (:states 'normal :keymaps 'cfw:calendar-mode-map
             ">"     (general-simulate-key ">" :state 'emacs)
             "<"     (general-simulate-key "<" :state 'emacs))
    :config

    ;; default calendar view to week instead of month
    (defun my--cfw:open-calendar-buffer-view (orig-func &rest args &allow-other-keys)
      (apply orig-func :view 'week :allow-other-keys t args))
    (advice-add 'cfw:open-calendar-buffer :around #'my--cfw:open-calendar-buffer-view))

  (use-package calfw-org
    :after calfw
    :demand t)

  (use-package calfw-ical
    :after calfw
    :demand t))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :general
  (general-def 'normal markdown-mode-map
    "TAB" #'markdown-cycle)
  :init
  ;; coleslaw blogging
  (add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))
  (setq-default markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package graphviz-dot-mode
  :mode ("\\.dot$" . graphviz-dot-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(progn
  ;; ctags
  (setq-default path-to-ctags "/usr/bin/ctags")

  (defun create-tags (dir-name)
    "Create tags file for the project in \"DIR-NAME\"."
    (interactive "DDirectory: ")
    (shell-command
     (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

  (setq-default tags-add-tables nil)
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
  (add-hook 'lisp-mode-common-hook  'turn-on-ctags-auto-update-mode))

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

(progn ; sql and db
  (use-package sql
    :mode ("\\.sql$" . sql-mode)
    :general
    (:states 'insert :keymaps 'sql-interactive-mode-map
             "C-S-r" #'comint-history-isearch-backward-regexp
             "C-l" #'comint-clear-buffer
             "C-k" #'comint-previous-input
             "C-j" #'comint-next-input
             "C-d" (lambda ()
                     (interactive)
                     (when (yes-or-no-p "Quit Sql session?")
                       (comint-delchar-or-maybe-eof 0)
                       (delete-window))))
    (:state '(insert normal) :keymaps 'sql-mode-map
            "C-c C-c" #'sql-send-paragraph)
    :config
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (setq-default sql-buffer (get-buffer "*SQL*"))
                (toggle-truncate-lines t)))

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

  (use-package sql-indent
    :defer nil
    :after sql
    :init (add-hook 'sql-mode-hook 'sql-indent))

  (use-package sqlup-mode
    :defer nil
    :after sql
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    :config
    (add-to-list 'sqlup-blacklist "name")))

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

(progn ; settings for emacs-anywhere
  (defun popup-handler (app-name window-title x y w h)
    (lisp-mode))

  (add-hook 'ea-popup-hook 'popup-handler))

;; start the emacs daemon
(server-start)

;;; init.el ends here
