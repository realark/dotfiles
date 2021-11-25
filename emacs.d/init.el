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
(setq frame-title-format "%b -- %m")
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
;; Don't tell me what I can't do!
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; highlight the current line
(global-hl-line-mode t)
(setq large-file-warning-threshold 200000000) ;200mb
(setq create-lockfiles nil)
(blink-cursor-mode -1)
(setq help-window-select t)
(setq echo-keystrokes 0.1)

(mouse-avoidance-mode 'exile)

(setq completion-ignore-case t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(progn ; line numbers
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

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

;; handle long lines
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))


;; Misc elisp utils
(progn
  (defun touch-file (file)
    "Open FILE and write to it without changing to a new buffer."
    (save-excursion
      (find-file file)
      (save-buffer)
      (kill-buffer)))

  (defun load-if-exists (file)
    "If FILE exists load it and return t. nil if file does not exist."
    (when (file-exists-p file)
      (load file)
      t))

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
      line-starts-with-char))

  (defun toggle-camelcase-underscores ()
    "Toggle between camelcase and underscore notation for the symbol at point."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds)))
      (let ((currently-using-underscores-p (save-excursion (goto-char start)
                                                           (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (progn
              (upcase-initials-region start end)
              (replace-string "_" "" nil start end)
              (downcase-region start (1+ start)))
          (progn
            (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
            (downcase-region start (cdr (bounds-of-thing-at-point 'symbol))))))
      (goto-char start)))

  ;; https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
  (defun aj-toggle-fold ()
    "Toggle fold all lines larger than indentation on current line"
    (interactive)
    (let ((col 1))
      (save-excursion
        (back-to-indentation)
        (setq col (+ 1 (current-column)))
        (set-selective-display
         (if selective-display nil (or col 1)))))))

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
           (cond
            ((file-exists-p "/etc/motd")
             (get-string-from-file "/etc/motd"))
            (t "I am but a dream--your dream, a creature of your imagination.\nIn a moment you will have realized this,\nthen you will banish me from your visions and I shall dissolve into the nothingness out of which you made me."))))
         "\n\n")))

(progn ; set up custom file
  (unless (file-exists-p "~/.emacs.d/custom.el")
    (touch-file "~/.emacs.d/custom.el"))

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

;; use mepla and marmalade for package
(eval-when-compile
  (defmacro require-install (PCK)
    "Require package PCK, install via \"package-install\" if missing."
    `(unless (require ,PCK nil t)
       (package-install ,PCK)
       (require ,PCK)))
  (require-install 'package)
  (when (and (equal emacs-version "27.2")
             (eql system-type 'darwin))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless package-archive-contents
    (package-refresh-contents))
  (require-install 'use-package)
  (setq-default use-package-always-ensure nil)
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

  ;; Theme. For ideas: https://emacsthemes.com/popular/index.html
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

  (use-package zenburn-theme
    :ensure :defer
    :if window-system
    :load-path "themes")

  (use-package spacemacs-theme
    :ensure :defer
    :if window-system
    :load-path "themes")

  (use-package darktooth-theme
    :ensure t
    :if window-system)

  (use-package circadian
    :ensure t
    :if window-system)

  (load-theme 'darktooth)

  ;; (setq-default circadian-themes '(("07:30" . zenburn)
  ;;                                  ("17:00" . spacemacs-dark)
  ;;                                  ("22:00" . cyberpunk)))
  ;; (circadian-setup)

  ;; Maximize emacs window
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; winner mode allows easy undo/redo of window changes
  (winner-mode 1)

  ;; use ace-window
  (use-package ace-window
    :defer nil))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;;Evil (extensible vi layer for Emacs)
(use-package evil
  :defer nil
  :init
  (setq-default ;; Default to normal mode most of the time
                evil-want-keybinding nil
                evil-insert-state-modes '(nrepl-mode shell-mode git-commit-mode term-mode eshell-mode)
                evil-emacs-state-modes '(magit-mode)
                evil-motion-state-modes '())
  (evil-mode 1)
  :config
  (evil-set-undo-system 'undo-tree))

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
    :states '(normal insert)
    :keymaps 'compilation-mode-map
    "C-j" #'compilation-next-error
    "C-k" #'compilation-previous-error)

  (general-def
    :prefix "C-x"
    "l"      (lambda ()
               (interactive)
               (if (bound-and-true-p whitespace-mode)
                   (progn
                     (whitespace-mode 0))
                 (progn
                   (message "nil branch")
                   (whitespace-mode 1))))
    "f"      #'indent-region
    ";"      #'toggle-comment-region-or-line
    "x"      (general-simulate-key "M-x")))

(block narrow-utils
  ;; https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (defun narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (sticky-window-delete-other-windows nil))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  (general-def :states '(normal)
    "C-x n" #'narrow-or-widen-dwim)
  (general-def :states '(normal) :keymaps '(org-src-mode-map)
    "q" #'org-edit-src-exit))

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
     ("o" sticky-window-delete-other-windows :exit t)
     ("d" sticky-window-delete-window)
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

(use-package smerge-mode
  :after hydra
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body))))
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_j_next      _K_: upper           _<_: upper/base        _r_esolve manually
_k_prev      _J_: lower           _>_: base/lower
"

    ("j" (lambda ()
           (interactive)
           ;; smerge throws an error if there's no next
           (ignore-errors
             (smerge-next 1))))
    ("k" smerge-prev)
    ("K" smerge-keep-upper)
    ("J" smerge-keep-lower)
    ("<" smerge-diff-base-upper)
    (">" smerge-diff-base-lower)
    ("r" nil :color blue)
    ("q" nil "cancel" :color blue)))

(use-package smartparens
  :init
  (smartparens-global-strict-mode)
  :delight smartparens-mode
  :config
  (sp-pair "'" nil :actions nil)
  (sp-pair "`" nil :actions nil))

;; (use-package cl-font-lock
;;   ;; :hook ((lisp-mode . cl-font-lock))
;;   :demand t)

(use-package lispyville
  :delight lispyville-mode
  :hook ((lisp-mode . lispyville-mode)
         (emacs-lisp-mode . lispyville-mode)
         (eval-expression-minibuffer-setup . lispyville-mode)
         (ielm-mode . lispyville-mode)
         (lisp-interaction-mode . lispyville-mode)
         (slime-repl-mode . lispyville-mode)
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
    ("C-s" (lambda ()
             (interactive)
             (setq isearch-forward t)
             (swiper)))
    :init
    (setq-default swiper-action-recenter t))

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

(use-package noflet
  :commands (noflet))

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

  (defun my/bypass-confirmation (function &rest args)
    "Call FUNCTION with ARGS, bypassing all `y-or-n-p' prompts."
    (require 'noflet)
    (noflet
     ((y-or-n-p (prompt) t))
     (apply function args)))


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
     ("T"   (lambda ()
              (interactive)
              ;; NOTE: if you get some error about `-e` not being supported
              ;; you may have to explicitly install ctags (apt-get install ctags)
              ;; https://stackoverflow.com/questions/2580313/ctags-doesnt-understand-e-option-no-exuberant-tags-option
              (my/bypass-confirmation #'projectile-regenerate-tags)
              (projectile-find-tag)))
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
  :general
  (:states 'normal :keymaps 'flycheck-error-list-mode-map
           "j" #'flycheck-error-list-next-error
           "k" #'flycheck-error-list-previous-error)
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

(use-package flycheck-pos-tip
  :after flycheck
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flyspell-correct
  :after flyspell)

(use-package frog-menu
  :after flyspell-correct
  :config
  (require 'flyspell-correct)
  (defun frog-menu-flyspell-correct (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (let* ((corrects (if flyspell-sort-corrections
                         (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word"         (save    . ,word))
                      ("C-a" "Accept (session)"  (session . ,word))
                      ("C-b" "Accept (buffer)"   (buffer  . ,word))
                      ("C-c" "Skip"              (skip    . ,word))))
           (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                     ispell-dictionary
                                                     "default")))
           (res      (frog-menu-read prompt corrects actions)))
      (unless res
        (error "Quit"))
      res))
  (setq flyspell-correct-interface #'frog-menu-flyspell-correct))

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

(use-package all-the-icons
  ;; note: must run M-x all-the-icons-install-fonts
  :demand t)

(use-package centaur-tabs
  :demand t
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (magit-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (slime-repl-mode . centaur-tabs-local-mode)
  :general
  (:states 'normal :keymaps 'centaur-tabs-mode-map
   "gT" #'centaur-tabs-backward-tab
   "gt" #'centaur-tabs-forward-tab)
  :init
  (centaur-tabs-mode 1)
  :config
  (setq centaur-tabs-set-icons t
        entaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar t
        centaur-tabs-background-color (face-background 'default)
        centaur-tabs-set-bar 'over
        centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker t)
  (defvar my-cached-buffer-groups nil
    "Cached value for custom buffer groups")
  (defun centaur-tabs-buffer-groups ()
    (if my-cached-buffer-groups
        (symbol-value 'my-cached-buffer-groups)
      (set (make-local-variable 'my-cached-buffer-groups)
           (list
            (cond
             ((memq major-mode '(erc-mode)) "IRC")
             ((or (memq major-mode '(magit-process-mode
			                               magit-status-mode
			                               magit-diff-mode
			                               magit-log-mode
			                               magit-file-mode
			                               magit-blob-mode
			                               magit-blame-mode))
                  (and (not (null (buffer-name)))
                       (>= (length (buffer-name)) 1)
                       (string-equal "*" (substring (buffer-name) 0 1)))
                  (and (not (null (buffer-name)))
                       (>= (length (buffer-name)) 4)
                       (string-equal "TAGS" (substring (buffer-name) 0 4))))
              "Emacs")
             ((derived-mode-p 'eshell-mode)
              "EShell")
             ((derived-mode-p 'emacs-lisp-mode)
              "Elisp")
             ((derived-mode-p 'dired-mode)
              "Dired")
             ((memq major-mode '(org-agenda-mode diary-mode))
              "OrgMode")
             (t
              (centaur-tabs-project-name)))))
      (symbol-value 'my-cached-buffer-groups)))

  (defhydra hydra-tabs (:color amaranth :hint nil)
    "
TABS: %(buffer-name) -- %(centaur-tabs-current-tabset)
-------------------------------------------------------------
 Move:        _L_:right _H_:left
 Switch:      _l_:right _h_:left _k_:forward-group _j_:backward-group
 Kill:        _x_:current-buffer _X_:all-buffers-in-group
EOF"
    ("l" centaur-tabs-forward-tab)
    ("h" centaur-tabs-backward-tab)
    ("j" #'centaur-tabs-forward-group)
    ("k" #'centaur-tabs-backward-group)
    ("L" #'centaur-tabs-move-current-tab-to-right)
    ("H" #'centaur-tabs-move-current-tab-to-left)
    ("x" (lambda ()
           (interactive)
           (kill-buffer (current-buffer))))
    ("X" #'centaur-tabs-kill-all-buffers-in-current-group)
    ("q" nil))
  ;; (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-group-buffer-groups))

(use-package keycast
  :demand t
  :init
  (keycast-mode))

;; sticky windows
(when (load-if-exists "~/.emacs.d/sticky-windows.el")
  (general-def
    "C-x 0" #'sticky-window-delete-window
    "C-x 1" #'sticky-window-delete-other-windows))

;; interactive mode toggling
(progn
  (defvar interactive-perspectives '()
    "list of (major-mode interactive-mode launch-interactive-mode-function).
The first two elements must be a 1:1 unique mapping of major-modes.")
  (setq interactive-perspectives
        (list (list "lisp-mode" "slime-repl-mode" #'slime)
              (list "sh-mode" "term-mode" #'eshell)
              (list "groovye-mode" "inferior-groovy-mode" #'run-groovy)
              (list "java-mode" "inferior-groovy-mode" #'run-groovy)
              (list "sql-mode" "sql-interactive-mode" (lambda() (call-interactively #'sql-connect)))))

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
                         (sticky-window-delete-window nil)
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
                       (sticky-window-delete-window nil))
     ('lisp-mode (call-interactively #'slime-edit-definition))
     ('slime-repl-mode (call-interactively #'slime-edit-definition))
     ('java-mode (call-interactively #'lsp-ui-peek-find-definitions))
     ('protobuf-mode (call-interactively #'dumb-jump-go ))))

  (defhydra hydra-ide (:color amaranth :columns 1)
    "IDE Actions"
    ("h" (mode-case
          ('java-mode (lsp-ui-peek-find-implementation))
          ('lisp-mode (slime-browse-classes (slime-read-symbol-name "Class Name: "))))
     "Type Hierarchy" :exit t)
    ("r" (mode-case
          ('java-mode (lsp-ui-peek-find-references))
          ('lisp-mode (call-interactively #'slime-who-calls)))
     "References" :exit t)
    ("R" (mode-case
          ('java-mode (call-interactively #'lsp-rename)))
     "Rename" :exit t)
    ("d" (mode-case
          ((or 'lisp-mode 'slime-repl-mode)
           (if (file-exists-p (concat (getenv "HOME") "/prog/slime-doc-contribs"))
               (call-interactively #'slime-help-symbol)
             (call-interactively #'slime-documentation)))
          ('java-mode (call-interactively #'lsp-describe-thing-at-point)))
     "Documentation" :exit t)
    ("e" (mode-case
          ('lisp-mode (call-interactively #'slime-list-compiler-notes))
          ('java-mode (call-interactively #'list-flycheck-errors))
          ('emacs-lisp-mode (call-interactively #'list-flycheck-errors)))
     "Error List" :exit t)
    ("g" (%my-go-to-definition)
     "Go to definition" :exit t)
    ;; ("d"  (my-ide-documentation) "Documentation" :exit t)
    ;; ("i"  (my-ide-interaction) "Interaction (repl, shell)" :exit t)
    ;; ("t"  (my-ide-test-repeat) "test re-run" :exit t)
    ("i"  (single-test) "Test thing at point" :exit t)
    ("?" (display-local-help) "Display help at point" :exit t)
    ("q"  nil "Cancel" :color red))

  (general-def
    :states '(normal insert)
    "C-x i" #'hydra-ide/body
    "<f4>" #'%my-go-to-definition
    "M-." #'%my-go-to-definition))

(use-package ansi-color
  :demand t
  :config
  (defun my-ansi-color-region ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun my-ansi-color-buffer ()
    (interactive)
    (let ((inhibit-read-only t))
      (save-excursion
        (mark-beginning-of-buffer)
        (my-ansi-color-region)))))

(progn ; slime stuff
  (use-package slime
    :delight slime
    :commands (slime slime-connect)
    :general
    (:states 'normal :keymaps 'slime-repl-mode-map
             "q" (lambda ()
                   (interactive)
                   (end-of-buffer)
                   (evil-insert-state)
                   (toggle-interact-with-buffer))
             [return]  #'slime-inspect-presentation-at-point
             "C-<return>" #'slime-expand-1-inplace)
    (:states 'insert :keymaps 'slime-repl-mode-map
             ;; insert a newline instead of evaluating the expression
             "S-<return>" #'newline-and-indent
             "C-c" #'my-slime-repl-kill-or-interrupt
             "C-d" (lambda () (interactive)
                     (when (y-or-n-p "Quit slime?")
                       (and (slime-repl-quit) (sticky-window-delete-window nil))))
             "C-r" #'slime-repl-previous-matching-input
             "TAB" #'completion-at-point
             "C-S-l" #'slime-repl-clear-buffer
             "C-k" #'slime-repl-previous-input
             "C-j" #'slime-repl-next-input)
    (:states 'normal :keymaps 'sldb-mode-map
             "C-j" (general-simulate-key "n" :state 'emacs)
             "C-k" (general-simulate-key "p" :state 'emacs)
             "j" (general-simulate-key "C-n" :state 'emacs)
             "k" (general-simulate-key "C-p" :state 'emacs)
             "l" (general-simulate-key "C-f" :state 'emacs)
             "h" (general-simulate-key "C-b" :state 'emacs)
             "0" (general-simulate-key "0" :state 'emacs)
             "1" (general-simulate-key "1" :state 'emacs)
             "2" (general-simulate-key "2" :state 'emacs)
             "3" (general-simulate-key "3" :state 'emacs)
             "4" (general-simulate-key "4" :state 'emacs)
             "5" (general-simulate-key "5" :state 'emacs)
             "6" (general-simulate-key "6" :state 'emacs)
             "7" (general-simulate-key "7" :state 'emacs)
             "8" (general-simulate-key "8" :state 'emacs)
             "9" (general-simulate-key "9" :state 'emacs)
             ":" #'sldb-pprint-eval-in-frame
             "v" (general-simulate-key "v" :state 'emacs))
    (:states 'normal :keymaps 'slime-xref-mode-map
             "j" #'slime-xref-next-line
             "k" #'slime-xref-prev-line)
    (:states 'normal :keymaps 'lisp-mode-map
             "C-c C-c" (lambda ()
                       (interactive)
                       (slime-compile-defun)
                       ;; (when my-slime-defun
                       ;;   (slime-eval-async
                       ;;       ;; DONTCOMMIT make project custom
                       ;;       my-slime-defun
                       ;;     nil
                       ;;     #'cl-user))
                       ))
    (:states 'normal :keymaps 'slime-inspector-mode-map
             ":" #'slime-inspector-eval
             "q" (lambda ()
                   "Reinspect the previous object or close the window if there is no previous object"
                   ;; mostly copied from slime-inspector-pop
                   (interactive)
                   (let ((result (slime-eval `(swank:inspector-pop))))
                     (if result
                         (slime-open-inspector result (pop slime-inspector-mark-stack))
                       (quit-window)))))
    (:states 'normal :keymaps 'slime-sprof-browser-mode
             "v" (general-simulate-key "v" :state 'emacs))
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
    (defvar my-slime-defun nil)
    :config
    (progn ; emit ansi colors in slime repl
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
    (make-directory "/tmp/slime-fasls/" t)
    (setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))

    (load (expand-file-name "~/.roswell/helper.el"))
    (setq inferior-lisp-program "ros -Q -l ~/.sbclrc run --lose-on-corruption")

    (if (file-exists-p (concat (getenv "HOME") "/prog/slime-doc-contribs"))
        (progn
          (push (concat (getenv "HOME") "/prog/slime-doc-contribs") load-path)
          (slime-setup '(slime-fancy
                         slime-highlight-edits
                         slime-asdf
                         slime-sprof
                         slime-xref-browser
                         slime-help
                         slime-info)))
      (slime-setup '(slime-fancy
                     slime-highlight-edits
                     slime-asdf
                     slime-sprof)))

    (slime-require :swank-listener-hooks)
    (setq-default slime-sprof-exclude-swank t)
    ;; https://emacs.stackexchange.com/questions/9600/how-can-i-override-a-pre-defined-face-for-light-and-dark-backgrounds
    (set-face-attribute 'slime-highlight-edits-face nil
                        ;; :background "dimgray"
                        ;; :background "lightgray"
                        :background "purple4")

    ;; keymap must go here after contrib loads
    (general-def :states 'normal :keymaps 'slime-browser-map
      "j" 'widget-forward
      "k" 'widget-backward
      "M-." (lambda () (interactive)
              (end-of-line)
              (slime-edit-definition (slime-symbol-at-point)))
      "q" 'bury-buffer)

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

    (defun slime-post-connect ()
      (when (file-exists-p "~/.emacs.d/slime-init.lisp")
        (slime-eval (car (read-from-string (get-string-from-file "~/.emacs.d/slime-init.lisp")))))
      (when (file-exists-p (concat default-directory ".custom-slime-init.lisp"))
        (slime-eval (car (read-from-string (get-string-from-file (concat default-directory ".custom-slime-init.lisp"))))))
      (when (file-exists-p (concat default-directory ".slime-initial-package"))
        (slime-repl-set-package (get-string-from-file (concat default-directory ".slime-initial-package"))))
      (message "custom slime init complete"))

    (load-if-exists "~/.roswell/lisp/quicklisp/dists/quicklisp/software/cl-annot-20150608-git/misc/slime-annot.el")
    ;; to install log4slime:
    ;; (ql:quickload :log4slime)
    ;; (log4slime:install)
    (when (file-exists-p "~/.roswell/lisp/quicklisp/log4slime-setup.el")
      (load "~/.roswell/lisp/quicklisp/log4slime-setup.el")
      (global-log4slime-mode 1)
      (general-def :keymaps 'log4slime-mode-map
        "C-c C-g" #'log4slime-level-selection))

    ;; allow slime doc to resize the window
    (setq-default eldoc-echo-area-use-multiline-p t)

    (add-hook 'slime-repl-mode-hook #'slime-post-connect))

  (use-package slime-company
    :demand t
    :after slime
    :config
    (slime-setup '(slime-company))
    (slime-company-init)))

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

  (use-package magit-todos
    :after magit
    :init
    ;; To set it locally, run add-dir-local-variable, choose magit-status-mode,
    ;; choose magit-todos-exclude-globs, add patterns you would like to exclude as a list.
    (setq-default magit-todos-exclude-globs '("*.map" "*.html")))

  (use-package evil-collection
    :after evil
    :ensure t
    :init
    (add-hook 'magit-todos-mode-hook
              (lambda ()
                (evil-collection-init 'magit-todos))))

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
    (setq-default git-link-default-branch "master")))

(use-package hideshow
  :delight hs-minor-mode
  :hook ((prog-mode . hs-minor-mode)
         (hs-minor-mode . hs-hide-initial-comment-block))
  :config
  (defun end-of-line-before-comment ()
    "Move the end of the line, or just before a comment character if the line ends in a comment."
    (when (comment-search-forward (line-end-position) t)
      (goto-char (match-beginning 0))
      (skip-syntax-backward " " (line-beginning-position))
      (backward-char)))

  ;; hideshow org-mode like folding
  ;; see: https://github.com/logangrado/hideshow-orgmode/blob/master/hideshow-orgmode.el
  (setq hs-allow-nesting t)
  (defun hs-cycle ()
    "Progressively shows more blocks under current block, then hide all blocks"
    (interactive)
    (hs-life-goes-on
     (save-excursion
       ;; (move-beginning-of-line 1)
       (when (hs-find-block-beginning)
         (let (minp maxp)
	         (setq minp (point))		;Set minp to beg+1
	         (funcall hs-forward-sexp-func 1)	;Goes to end of current block
	         (setq maxp (point))		;Set maxp to end-1s
	         (if (hs-contains-hidden minp maxp)
	             (hs-discard-overlays minp maxp)
	           (hs-hide-recursive minp maxp)))))))
  (defun hs-contains-hidden (minp maxp)
    "Returns nil if there is no overlay between minp and maxp"
    (goto-char minp)
    (let ((contains_hidden nil))
      (while (progn
	             (forward-comment (buffer-size)) ;forward-comment moves forward across count complete comments
	             (and(and (< (point) maxp) ;Ensure we're not past maxp
		                    (re-search-forward hs-block-start-regexp maxp t))
		               (not contains_hidden))) ;Searches forward for next blockstart
        (setq contains_hidden (hs-already-hidden-p)))
      contains_hidden))
  (defun hs-hide-recursive (minp maxp)
    "Hide all blocks between minp,maxp recursively (deepest level up)"
    (hs-life-goes-on
     (save-excursion
       (goto-char minp)
       (save-excursion
         (let ((maxd 3));(hs-max-depth minp maxp)))
	         (while (> maxd 0)
	           (goto-char minp)
	           (hs-hide-level-recursive maxd minp maxp)
	           (setq maxd (1- maxd)))))
       (hs-hide-block))))
  (defun hs-cycle-all()
    "Progressive show more blocks all are shown, then hide all blocks"
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (if (hs-contains-hidden (point-min) (point-max))
	         (hs-discard-overlays (point-min) (point-max))
         (hs-hide-recursive (point-min) (point-max))))))

  (defun hs-fold-all()
    "Hides all blocks. Differs from 'hs-hide-all' in that it also folds all child blocks"
    (interactive)
    (hs-life-goes-on
     (hs-find-block-beginning) ;go to beginning of block
     (save-excursion
       (goto-char (point-min))
       (hs-hide-recursive (point-min) (point-max)))))

  (defun hs-fold-block()
    "Hides current block recursively"
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (move-beginning-of-line 1)
       (let ((minp nil) (maxp nil))
         (when (hs-find-block-beginning)
	         (setq minp (point))
	         (funcall hs-forward-sexp-func 1)
	         (setq maxp (1- (point)))
	         (goto-char minp)
	         (hs-hide-recursive minp maxp))))))

  :general
  (:states '(normal) :keymaps 'hs-minor-mode-map
            "<tab>" #'hs-toggle-hiding
            "<backtab>" #'hs-show-all))

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

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)))

;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  :general
  ("C-x c" #'hydra-orgmode/body
   "C-x a" #'org-agenda-list)
  (:states 'normal :keymaps 'org-agenda-mode-map
           "M-k"     #'org-agenda-drag-line-backward
           "M-j"     #'org-agenda-drag-line-forward
           "L"     #'org-agenda-do-date-later
           "H"     #'org-agenda-do-date-earlier
           "D"     #'org-agenda-kill)
  :delight org-indent-mode nil org-indent
  :init
  (defhydra hydra-orgmode (:color amaranth :columns 1)
    "Org Mode"
    ("a"  (org-agenda) "Agenda" :exit t)
    ("c"  (progn
            (cfw:open-calendar-buffer
             :contents-sources
             (list
              (cfw:org-create-source "Green")))
            (hydra-calfw/body))
     "Weekly calendar" :exit t)
    ("C"  (org-capture) "Capture" :exit t)
    ("q"  nil "Cancel" :color red))
  :config
  (setq-default org-agenda-show-future-repeats t) ; will want to turn this off if I stop using org-habits
  (setq-default org-todo-keywords '("TODO(t)" "DOING(o)" "|" "DONE(d!)" "SKIP(s!)"))
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

  ;; (setq-default org-log-repeat nil)
  (add-to-list 'org-modules 'org-habit t)
  ;; show all habits in agenda, even if done
  (setq-default org-habit-show-all-today nil)
  ;; I don't like seeing red in the future
  (setq-default org-habit-following-days 0)

  (setq-default org-log-into-drawer t)

  (setq-default org-refile-targets '((nil :maxlevel . 9)))
  (setq-default org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq-default org-refile-use-outline-path t) ; Show full paths for refiling
  (setq-default org-capture-templates
                `(("s" "Syn Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "syn")
                   "* TODO %?\nSCHEDULED: <%<%Y-%m-%d>>")
                  ("l" "Life Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "life")
                   "* TODO %?\nSCHEDULED: <%<%Y-%m-%d>>")
                  ("p" "Peak6 Task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "peak6")
                   "* TODO %?\nSCHEDULED: <%<%Y-%m-%d>>")
                  ("b" "Backlog task" entry (file+olp ,(concat org-directory "tasks.org") "Agenda" "backlog")
                   "* TODO %?\nSCHEDULED: <%<%Y-%m-%d>>")))
  (setq-default org-duration-format (quote h:mm))
  (setq-default org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package evil-org
  :hook ((org-mode . evil-org-mode)
         (org-trello-mode . evil-org-mode))
  :delight
  evil-org-mode
  :general
  (:states '(insert) :keymaps '(org-mode-map)
    "M-h"     #'org-metaleft
    "M-l"     #'org-metaright)
  (:states '(normal) :keymaps '(org-mode-map)
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
                org-agenda-skip-scheduled-if-done nil
                org-agenda-skip-deadline-if-done nil
                org-super-agenda-groups
                '((:name "CALENDAR"
                         :time-grid t
                         :file-path ".*gcal-.*.org")
                  (:name "#### ACTIVE TASKS ####"
                   :todo ("DOING")
                   :time-grid t
                   :face (:underline t))
                  (:name "SYN"
                   :time-grid t
                   :category "SYN")
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
  (setq-default calendar-location-name "Portland, OR")
  (setq-default calendar-latitude 45.5)
  (setq-default calendar-longitude -122.7)

  (use-package calfw
    :commands (cfw:open-calendar-buffer cfw:org-create-source)
    :general
    (:states 'normal :keymaps 'cfw:calendar-mode-map
             ;; ">"     (general-simulate-key ">" :state 'emacs)
             ;; "<"     (general-simulate-key "<" :state 'emacs)
             "q"     #'cfw:org-clean-exit)
    :config
    (defhydra hydra-calfw (:color amareth :columns 1)
      "Calfw"
      ("w" (cfw:change-view-week)
       "week view")
      ("m" (cfw:change-view-month)
       "month view")
      (">"
       (progn
         (if (eq 'week (cfw:cp-get-view (cfw:cp-get-component)))
           (cfw:navi-next-week-command)
           (cfw:navi-next-month-command)))
       "Next")
      ("<"
       (progn
         (if (eq 'week (cfw:cp-get-view (cfw:cp-get-component)))
           (cfw:navi-previous-week-command)
           (cfw:navi-previous-month-command)))
       "Previous")
      ("q"
       ;; (progn
       ;;   (cfw:org-clean-exit)
       ;;   (hydra--body-exit))
       nil
       "quit hydra"
       :color red
       :exit t))

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

(use-package org-gcal
  :ensure t
  :config
  (setq-default
   org-gcal-client-id my-gcal-client-id
   org-gcal-client-secret my-gcal-client-secret
   org-gcal-file-alist my-gcal-file-alist))

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
  :init
  :general
  (general-def 'normal yaml-mode-map
    "TAB" #'aj-toggle-fold)
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package highlight-indentation
  :hook ((yaml-mode . highlight-indentation-current-column-mode)))

(use-package graphviz-dot-mode
  :mode ("\\.dot$" . graphviz-dot-mode))

;; NOTE: mmdc command must be in path
(use-package mermaid-mode
  :mode ("\\.mmd$" . mermaid-mode)
  :general
  (:states 'normal :keymaps 'mermaid-mode-map
           "C-c C-c" #'mermaid-compile-buffer))

(use-package restclient
  :general
  (:states 'normal :keymaps 'restclient-mode-map
           "TAB" #'restclient-toggle-body-visibility))

(use-package protobuf-mode
  ;; In a dir local, you might want to add something like this to get flycheck imports working
  ;; ((protobuf-mode . ((flycheck-protoc-import-path . ("/path/to/proto/root")))))
  :mode ("\\.proto$" . protobuf-mode)
  :hook ((protobuf-mode . hs-minor-mode)
         (protobuf-mode . flyspell-prog-mode)
         (protobuf-mode . display-line-numbers-mode)))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(progn ; show-paren-mode
  :init
  (show-paren-mode 1)
  :config
  (setq-default show-paren-style 'expression)
  (set-face-background 'show-paren-match-expression "RoyalBlue4")
  (progn
    ;; favor highlighting the previous expression (closing paren)
    (defun show-paren--locate-near-paren-ad ()
      "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
      (let* ((current (show-paren--categorize-paren (point)))
             (before (show-paren--categorize-paren (- (point) 1))))
        (cond ((or (eq (car before) 1)
                   (eq (car before) -1))
               before)
              ((or (eq (car current) 1)
                   (eq (car current) -1))
               current))))
    (advice-add 'show-paren--locate-near-paren :override #'show-paren--locate-near-paren-ad)))

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

(use-package kotlin-mode
  :mode
  ("\\.kt$" . kotlin-mode))

(use-package scala-mode
  :mode
  ("\\.scala$" . scala-mode))

(use-package jdecomp
  ;; :mode ("\\.class$" . jdecomp-mode)
  :config
  (customize-set-variable 'jdecomp-decompiler-type 'procyon)
  (customize-set-variable 'jdecomp-decompiler-paths
                          `((procyon . ,(first-existing-file "/opt/procyon/procyon-decompiler-0.6-prerelease.jar"
                                                             "/usr/share/java/procyon-decompiler/procyon-decompiler.jar"
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

(use-package feature-mode
  :mode (("\\.feature$" . feature-mode)))

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
                       (sticky-window-delete-window nil))))
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
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)))

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

(progn                                  ; lsp
  (use-package lsp-mode
    :demand t
    :init
    (setq-default lsp-prefer-flymake nil)
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode))

  (use-package company-lsp
    :after lsp-mode
    :config
    (setq company-lsp-enable-snippet t
          company-lsp-cache-candidates t)
    ;; (push 'java-mode company-global-modes)
    (push 'company-lsp company-backends))

  (use-package lsp-ui
    :after lsp-mode
    :hook ((lsp-mode . lsp-ui-mode))
    :general
    (:keymaps 'lsp-ui-peek-mode-map
              "M-j" #'lsp-ui-peek--select-next
              "M-k" #'lsp-ui-peek--select-prev)
    :config
    (setq-default lsp-ui-sideline-enable nil
                  lsp-ui-peek-list-width 130)
    (lsp-ui-sideline-mode -1))

  (use-package dap-mode
    :after lsp-mode
    :config
    (dap-auto-configure-mode)))

(use-package lsp-java
  :demand t
  ;; :hook ((java-mode . lsp))
  :after lsp-mode
  ;; :init
  ;; (defun jmi/java-mode-config ()
  ;;   (setq-local tab-width 4
  ;;               c-basic-offset 4)
  ;;   (toggle-truncate-lines 1)
  ;;   (setq-local tab-width 4)
  ;;   (setq-local c-basic-offset 4)
  ;;   (lsp))
  ;; :hook (java-mode   . jmi/java-mode-config)
  :config
  ;; Enable dap-java
  (require 'dap-java)

  ;; (setenv "JAVA_HOME"  "/usr/lib/jvm/default-java")

  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:"
                      (getenv "HOME")
                      "/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.18/481f5bfed3ae29f656eedfe9e98c8365b8ba5c57/lombok-1.18.18.jar")
              (concat "-Xbootclasspath/a:"
                      (getenv "HOME")
                      "/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.18/481f5bfed3ae29f656eedfe9e98c8365b8ba5c57/lombok-1.18.18.jar")
              ;; (concat "-javaagent:" jmi/lombok-jar)
              ;; (concat "-Xbootclasspath/a:" jmi/lombok-jar)
              )
        ;; lsp-java-java-path "/usr/lib/jvm/default-java/bin/java"
        ;; lsp-java-import-gradle-java-home "/usr/lib/jvm/default-java/bin/java"
        ;; lsp-java-configuration-runtimes '[(:name "JavaSE-11"
        ;;                                          :path "/usr/lib/jvm/default-java"
        ;;                                          :default t)]
        lsp-file-watch-ignored
        (remove-duplicates (concatenate 'list
                                        (mapcar (lambda (ignore)
                                                  (concatenate 'string "**/*" ignore "$"))
                                                '("public" "build" "bin" ".gradle"))

                                        ;; keep lsp-mode defaults
                                        '(     ; SCM tools
                                          "[/\\\\]\\.git$"
                                          "[/\\\\]\\.hg$"
                                          "[/\\\\]\\.bzr$"
                                          "[/\\\\]_darcs$"
                                          "[/\\\\]\\.svn$"
                                          "[/\\\\]_FOSSIL_$"
                                          ;; IDE tools
                                          "[/\\\\]\\.idea$"
                                          "[/\\\\]\\.ensime_cache$"
                                          "[/\\\\]\\.eunit$"
                                          "[/\\\\]node_modules$"
                                          "[/\\\\]\\.fslckout$"
                                          "[/\\\\]\\.tox$"
                                          "[/\\\\]\\.stack-work$"
                                          "[/\\\\]\\.bloop$"
                                          "[/\\\\]\\.metals$"
                                          "[/\\\\]target$"
                                          "[/\\\\]\\.ccls-cache$"
                                          ;; Autotools output
                                          "[/\\\\]\\.deps$"
                                          "[/\\\\]build-aux$"
                                          "[/\\\\]autom4te.cache$"
                                          "[/\\\\]\\.reference$"))
                           :test #'equal)
        ;; Don't organize imports on save
        ;; Formatter profile
        ;; lsp-java-format-settings-url
        ;; (concat "file://" jmi/java-format-settings-file)
        lsp-java-import-gradle-enabled t
        lsp-java-import-gradle-offline-enabled t
        lsp-java-save-action-organize-imports nil)

  (cl-defun my-lsp-gradle-offline-toggle  (&optional (offline-p nil supplied-p))
    "Set gradle offline mode to OFFLINE-P. If OFFLINE-P is not provided, toggle the current offline value"
    (interactive)
    (if supplied-p
        (setq lsp-java-import-gradle-offline-enabled offline-p)
      (setq lsp-java-import-gradle-offline-enabled (not lsp-java-import-gradle-offline-enabled )))
    (if lsp-java-import-gradle-offline-enabled
        (message "...Gradle Offline...")
        (message "!!!Gradle Online!!!"))
    lsp-java-import-gradle-offline-enabled))

(progn ; glsl-mode
  (use-package glsl-mode)

  (use-package company-glsl
    :after glsl-mode))

(progn ; settings for emacs-anywhere
  (defun popup-handler (app-name window-title x y w h)
    (lisp-mode))

  (add-hook 'ea-popup-hook 'popup-handler))

(when (file-exists-p "~/.emacs.d/vtl.el")
  (load-file "~/.emacs.d/vtl.el")
  (add-to-list 'auto-mode-alist '("\\.vm\\'" . turn-on-vtl-mode)))

(when (file-exists-p "~/.emacs.d/go-template-mode.el")
  (load-file  "~/.emacs.d/go-template-mode.el"))

(progn ; k8
  ;; (setq kubel-namespace "some-namespace")
  (use-package kubel
    :ensure t
    :commands (kubel))

  (use-package kubel-evil
    :ensure t
    :after kubel))

(use-package elpy
  :commands (elpy-enable)
  :config
  (setq-default
   elpy-rpc-python-command "python3"
   elpy-rpc-backend "jedi"))

(use-package go-mode
  :mode ("\\.go$" . go-mode))

(use-package graphql-mode
  :mode ("\\.graphql$" . graphql-mode)
  ;; :config
  ;; (when (file-exists-p "~/.emacs.d/company-graphql/company-graphql.el")
  ;;   (load "~/.emacs.d/company-graphql/company-graphql.el")
  ;;   (add-to-list 'company-backends 'company-graphql))
    )

;; (use-package company-graphql
;;   :after graphql-mode
;;   :config
;;   (add-to-list 'company-backends 'company-graphql))

(use-package json-mode
  :mode ("\\.json$" . json-mode))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq-default xref-show-definitions-function #'ivy-completing-read)
  (setq-default dumb-jump-selector 'ivy)
  :config
  (setq-default dumb-jump-git-grep-search-untracked nil))

(use-package phps-mode
  :mode ("\\.php$" . phps-mode))

(defun my/reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

;; start the emacs daemon
(server-start)

;;; init.el ends here
