;;; init.el --- emacs init file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; Just an Emacs init.  Nothing special here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;; Tweak Standard options:
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
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;;Show line numbers
(global-linum-mode t)
;; Insert closing bracket
(electric-pair-mode 1)
(show-paren-mode 1) ; turn on paren match highlighting
(setq-default show-paren-style 'expression) ; highlight entire bracket expression
;; Enable narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; highlight the current line
(global-hl-line-mode t)
(setq large-file-warning-threshold 100000000) ;100mb
(setq create-lockfiles nil)
(blink-cursor-mode -1)
(setq help-window-select t)
(setq echo-keystrokes 0.1)

(mouse-avoidance-mode 'banish)

(when (file-exists-p "~/.sec.el")
  (load "~/.sec.el"))

(defun my-minibuffer-setup-hook ()
  "Disable GC in the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  "Re-enable GC after minibuffer exit."
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

; change the minbuffer startup message
(defun display-startup-echo-area-message ()
  "Change the startup message."
  (message "Welcome"))
;; (setq initial-scratch-message "")

;; Maximize emacs window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; winner mode allows easy undo/redo of window changes
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Require-install Macro
(defmacro require-install (PCK)
  "Require package PCK, install via \"package-install\" if missing."
  `(unless (require ,PCK nil t)
     (package-install ,PCK)
     (require ,PCK)))

;; use mepla and marmalade for package
(require-install 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require-install 'use-package)
(setq-default use-package-always-ensure t)

;; try runs emacs packages without installing them
(use-package try)

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

(use-package tsdh
  :if window-system
  :ensure nil)
(use-package zenburn-theme
  :if window-system
  :ensure nil)
(use-package doom-themes
  :if window-system
  :ensure nil)
(use-package moe-theme
  :if window-system
  :ensure t
  :config
  (load-theme 'moe-dark t)
  (moe-theme-set-color 'cyan)
  (powerline-moe-theme))

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

;; use ace-window
(use-package ace-window)

(defun select-current-line ()
  "Select the current line."
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun toggle-comment-region-or-line ()
  "Comment or uncomment the selected region.  If no region is selected use the current line."
  (interactive)
  (if (not mark-active)
      (select-current-line))
  (comment-or-uncomment-region (region-beginning) (region-end)))

;; Options for M-x rgrep
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
             "bin"))))

;;Evil (extensible vi layer for Emacs)
(use-package evil
  :config
  (evil-mode 1)
  ;; Default to normal mode most of the time
  (setq-default evil-insert-state-modes '(nrepl-mode shell-mode git-commit-mode term-mode))
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
   "l"      #'whitespace-mode
   "f"      #'indent-region
   ";"      #'toggle-comment-region-or-line
   "<tab>"  #'hs-toggle-hiding
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
  (progn ;; Window moving helpers for hydra
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
  (general-define-key
   "C-x w"
   (defhydra hydra-window (:color amaranth :hint nil)
     "
WINDOW: %(buffer-name)
-------------------------------------------------------------
 Split:        _v_ert _x_:horz
 Delete:       _o_nly  _d_window
 Change:       _s_wap _b_uffer

 Move:         _h_left _j_up _k_down _l_right
 Nudge:        _H_left _J_down _K_down _L_right

 Winner:       _u_ndo  _C-r_edo"
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
     PROJECTILE: %(projectile-project-root)

 Find/Replace                   Buffers
------------------------------------------------------------------------------------------
 _f_: file find                 _k_: Kill all buffers
 _g_: grep all files
 _r_: replace
 _R_: replace regex

"
     ("f"   projectile-find-file)
     ("g"   projectile-grep)
     ("r"   projectile-replace)
     ("R"   projectile-replace-regexp)
     ("k"   projectile-kill-buffers)
     ("q"   nil "Cancel" :color red))))

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
  (use-package swiper)
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
  ;; make company work with org-mode
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
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

  (progn ; rebuild projectile cache after magit checkouts
    (defun %run-projectile-invalidate-cache (&rest _args)
      ;; We ignore the args to `magit-checkout'.
      (projectile-invalidate-cache nil))
    (advice-add 'magit-checkout
                :after #'%run-projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout ; This is `b c'.
                :after #'%run-projectile-invalidate-cache)))

(use-package ansi-color)

;;Slime
(use-package slime
  :delight slime
  :init
  (defun toggle-or-start-slime ()
    (interactive)
    (let ((slime-buffer (get-buffer "*slime-repl sbcl*")))
      (if slime-buffer
          (if (eq slime-buffer (current-buffer))
              (delete-window)
            (switch-to-buffer-other-window "*slime-repl sbcl*"))
        (slime))))
  (defun print-help ()
    (print "No override. Check for .custom.el?"))
  (defun single-test ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help))
  (defun run-unit-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help))
  (defun run-integration-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help))
  (defun run-performance-tests ()
    "Function for running unit test(s).  This should be overridden by a directory local definition."
    (interactive)
    (print-help))
  (defun reload-systems ()
    "Delete packages and reload asdf systems."
    (interactive)
    (print-help))
  :general
  ("<f9>" #'toggle-or-start-slime)
  :config
  (progn ;; emit ansi colors in slime repl
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
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
  (slime-setup '(slime-fancy
                 slime-highlight-edits
                 slime-asdf
                 slime-company
                 slime-xref-browser))
  ;; This is a hack. The hook should have already been established.
  (add-hook 'slime-repl-mode-hook #'slime-company-maybe-enable)
  (load "~/quicklisp/clhs-use-local.el" t)
  (setq inhibit-splash-screen t)

  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")
  (setq-default slime-protocol-version 'ignore)

  (defun my-slime-repl-kill-or-interrupt ()
    "If the user has entered text in the prompt, remove the text before and after point.
Otherwise, send an interrupt to slime."
    (interactive)
    (cond ((= (marker-position slime-repl-input-start-mark) (point))
           (slime-interrupt))
          (t (slime-repl-kill-input)
             (slime-repl-kill-input))))

  (general-evil-define-key 'normal slime-repl-mode-map
    "q" (lambda () (interactive) (end-of-buffer) (evil-insert-state) (toggle-or-start-slime))
    [return]  #'slime-inspect-presentation-at-point)

  (general-evil-define-key 'insert slime-repl-mode-map
    "C-c" 'my-slime-repl-kill-or-interrupt
    "C-d" (lambda () (interactive)
            (when (y-or-n-p "Quit slime?")
              (and (slime-repl-quit) (delete-window))))
    "C-r" 'slime-repl-previous-matching-input
    "TAB" 'completion-at-point
    "C-S-l" 'slime-repl-clear-buffer
    "C-k" 'slime-repl-previous-input
    "C-j" 'slime-repl-next-input)

  (evil-define-key 'normal lisp-mode-map
    (kbd "<f4>") #'slime-browse-classes
    (kbd "M-.") 'slime-edit-definition)

  ;; slime xref browser evil bindings
  (evil-define-key 'normal slime-browser-map
    (kbd "j") 'widget-forward
    (kbd "k") 'widget-backward
    (kbd "M-.") (lambda () (interactive)
                  (end-of-line)
                  (slime-edit-definition (slime-symbol-at-point)))
    (kbd "q") 'bury-buffer)

  ;; slime xref evil bindings
  (define-key slime-xref-mode-map (kbd "j") 'slime-xref-next-line)
  (define-key slime-xref-mode-map (kbd "k") 'slime-xref-prev-line)

  ;; sldb evil bindings
  (general-evil-define-key 'normal sldb-mode-map
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

  (general-evil-define-key 'normal slime-xref-mode-map
    "RET" (general-simulate-keys "RET" t))

  (evil-define-key 'normal slime-popup-buffer-mode-map
    (kbd "q") #'quit-window)

  ;; evil keys for slime inspector
  (evil-set-initial-state 'slime-inspector-mode 'normal)
  (evil-define-key 'normal slime-inspector-mode-map
    (kbd "q") (lambda ()
                "Reinspect the previous object or close the window if there is no previous object"
                ;; mostly copied from slime-inspector-pop
                (interactive)
                (let ((result (slime-eval `(swank:inspector-pop))))
                  (if result
                      (slime-open-inspector result (pop slime-inspector-mark-stack))
                    (quit-window))))))

;; Magit -- The best git interface I've ever used.
(use-package magit
  :general
  ("C-x g" 'magit-status)
  :config
  (setq-default magit-completing-read-function 'ivy-completing-read)
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (setq-default magit-push-always-verify nil)
  (setq-default magit-fetch-arguments '("--prune"))
  (setq-default magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (use-package evil-magit)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))

  ;; taken from: https://github.com/jordonbiondo/.emacs.d/blob/92bdf8cbedd61040ae85cb2a6b84cd3f3a9be4f6/jordon/jordon-magit.el
  (defun jordon-magit-line-region-of-section-at-point ()
    "If point is in a hunk return a list of info about the hunk.
The info is like (expanded-file-name starting-line number-of-lines-show)"
    (let* ((section (magit-current-section))
           (context-type (magit-section-type section)))
      (when (and (equal 'hunk context-type))
        (let* ((info
                (mapcar 'string-to-number
                        (split-string
                         (cl-third
                          (magit-section-value
                           (magit-current-section)))
                         ",")))
               (start-line (car info))
               (line-count (or (and (cdr info) (cadr info)) 1)))
          (let ((parent (magit-section-parent section)))
            (while (and parent
                        (not (equal (magit-section-type parent)
                                    'file)))
              (setq magit-section-parent parent))
            (list (expand-file-name (magit-section-value parent))
                  start-line
                  line-count))))))

  (defun jordon-magit-section-child-of-unstaged-p (section)
    (when section
      (or (equal 'unstaged (magit-section-type section))
          (jordon-magit-section-child-of-unstaged-p (magit-section-parent section)))))

  (defmacro define-magit-unstaged-hunk-action (name args &optional docstring &rest body)
    "NAME will be command that executes BODY in a way that has access to the beginning
and end of the region shown by the unstaged magit hunk at point.

The function will automatically open the hunks file, evaluated the body, and then
save the file and refresh the magit status buffer.

Args needs to be in the form (BEG END) where BEG and END are symbols that will be bound
to the regions beginning and end respectively.

In this example, the function `cleanup-this-hunk' is defined as a function
that deletes the trailing whitespace in the current unstaged magit hunk:

  (define-magit-unstaged-hunk-action cleanup-this-hunk (beg end)
    \"Delete trailing whitespace in the current unstaged magit hunk.\"
    (delete-trailing-whitespace beg end))

\(fn NAME (BEG END) &optional DOCSTRING &rest BODY)"
    (declare (indent defun) (doc-string 3))
    (unless (and (= (length args) 2)
                 (symbolp (car args))
                 (symbolp (cadr args)))
      (error "Invalid args format to `define-magit-unstaged-hunk-action', see doc."))
    (let ((docstring (if (car-safe docstring) "" docstring))
          (body (append (and (car-safe docstring) (list docstring)) body))
          (file-sym (make-symbol "file-name"))
          (start-line-sym (make-symbol "starting-line"))
          (total-lines-sym (make-symbol "total-lines-shown"))
          (area-sym (make-symbol "hunk-data")))
      `(defun ,name ()
         ,docstring
         (interactive)
         (let ((,area-sym (jordon-magit-line-region-of-section-at-point)))
           (if (and ,area-sym (jordon-magit-section-child-of-unstaged-p (magit-current-section)))
               (cl-destructuring-bind (,file-sym ,start-line-sym ,total-lines-sym) ,area-sym
                 (save-some-buffers)
                 (with-current-buffer (find-file-noselect ,file-sym)
                   (save-window-excursion
                     ;; (save-mark-and-excursion
                     (save-excursion
                       (let ((,(car args) (progn (goto-char (point-min))
                                                 (forward-line (1- ,start-line-sym))
                                                 (point-at-bol)))
                             (,(cadr args) (progn (forward-line (1- ,total-lines-sym))
                                                  (point-at-eol))))
                         ,@body)))
                   (save-buffer))
                 (magit-refresh))
             (message "Cannot perform. Point is not on an unstaged hunk."))))))

  (define-magit-unstaged-hunk-action custom-magit-clean-hunk (beg end)
    "Clean whitespace in the given hunk."
    (delete-trailing-whitespace beg end)))

(use-package git-link)

;; Code folding
(use-package hideshow
  :delight hs-minor-mode
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;multi-term
(use-package multi-term
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
  :general
  ("C-x s" #'get-term)
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

;;Turn off tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Adapt to the whitespace style of the file we're editing
(use-package fuzzy-format
  :delight fuzzy-format-mode
  :config
  (fuzzy-format-mode t)
  (setq show-trailing-whitespace t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;; Buffer navigation
; necessary support function for buffer burial
(defun crs-delete-these (delete-these from-this-list)
  "Delete DELETE-THESE FROM-THIS-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-this-list)
        (crs-delete-these (cdr delete-these) (delete (car delete-these)
                                                 from-this-list))
      (crs-delete-these (cdr delete-these) from-this-list)))
   (t from-this-list)))
; buffer regexes to skip while cycling
(defvar crs-hated-buffers
  '("^\*.*\*$"
    "help"
    "KILL"
    "TAGS"
    "^ .*"))
(defun crs-hated-buffers ()
  "List of buffers I never want to see, converted from names to buffers."
  (delete nil
          (append
           (mapcar (lambda (this-buffer)
                     (let (badbuffer)
                       (dolist (hated-buffer-regex crs-hated-buffers badbuffer)
                         (if (string-match hated-buffer-regex (buffer-name this-buffer))
                             (setq badbuffer this-buffer)))
                       badbuffer))
                   (buffer-list)))))

(defun crs-bury-buffer (&optional n)
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
                                          (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length my-buffer-list) n)
              my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))
;; evil buffer nav keys
(general-define-key
 :states '(normal)
 "gT" #'crs-bury-buffer
 "gt" (lambda () (interactive) (crs-bury-buffer -1)))

;; tags
(use-package etags-select
  :init
  (setq-default tags-revert-without-query 1)
  :general
  ("<f3>" #'etags-select-find-tag-at-point)
  (general-evil-define-key 'emacs etags-select-mode-map
    "j"       'etags-select-next-tag
    "k"       'etags-select-previous-tag
    "q"       'etags-select-quit
    "/"       'evil-search-forward
    "n"       'evil-search-next
    "N"       'evil-search-previous)
  :config
  (setq-default etags-select-go-if-unambiguous t)
  (setq-default etags-select-highlight-delay 5.0)
  (setq-default etags-select-use-short-name-completion t))

;; Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-by-copying t)

;; Org mode
(use-package org
  :mode ("\\.org$" . org-mode)
  :delight org-indent-mode nil org-indent
  :config
  (setq-default org-startup-indented t)
  ;; fontify code in code blocks
  (setq-default org-src-fontify-natively t)
  (setq-default org-agenda-files (list "~/Documents/org-files/tasks.org"
                                       "~/Documents/org-files/schedule.org"))
  (setq-default org-log-done 'time)
  (setq-default org-enforce-todo-dependencies t)

  (setq-default org-refile-targets '((nil :maxlevel . 9)))
  (setq-default org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq-default org-refile-use-outline-path t) ; Show full paths for refiling
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

;; elfeed
(use-package elfeed
  :defer t
  :config
  (elfeed-load-opml "~/.admin/arks_feeds.opml"))

;; Dired options
(use-package dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package rainbow-delimiters
  :config
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

(use-package cc-mode
  :mode ("\\.java$" . java-mode)
  :config
  (use-package eclim ; emacs frontend for eclipse
    ;; don't autostart eclim mode
    :mode ("\\.java$" . java-mode)
    :general
    (:keymaps 'eclim-mode-map
              "C-S-g"  #'eclim-java-find-references
              "<f4>"   #'eclim-personal-find-implementors
              "C-M-h"  #'eclim-java-call-hierarchy)
    (:keymaps 'eclim-problems-mode-map
              "e" 'eclim-problems-show-errors
              "w" 'eclim-problems-show-warnings
              "a" 'eclim-problems-show-all
              "g" 'eclim-problems-buffer-refresh
              "q" 'eclim-quit-window
              "RET" 'eclim-problems-open-current)
    :config
    (require 'eclim)
    (require 'eclimd)
    (global-eclim-mode)
    (defun eclim-personal-switch-to-junit-buffer-and-run ()
      "Re-run the last eclim junit test.  If there is not last test then test the current buffer."
      (interactive)
      (if (get-buffer "*compilation*")
          (progn
            (switch-to-buffer "*compilation*")
            (recompile)
            (end-of-buffer))
        (progn
          (end-of-buffer)
          (eclim-run-junit (eclim-project-name)
                           (eclim--project-current-file)
                           (eclim--byte-offset)
                           (eclim--current-encoding))
          (pop-to-mark-command)
          (pop-to-mark-command)
          (other-window 1)
          (delete-other-windows)
          (end-of-buffer))))

    (defun eclim-personal-find-implementors ()
      "Find implementors of the symbol under the point."
      (interactive)
      (eclim-java-find-generic "all" "implementors" "all" (thing-at-point 'symbol)))

    (setq-default eclim-auto-save t
                  eclim-executable "/usr/lib/eclipse/eclim"
                  eclimd-executable "/usr/lib/eclipse/eclimd"
                  eclimd-wait-for-process nil
                  eclimd-default-workspace "~/workspace"
                  eclim-use-yasnippet nil
                  help-at-pt-display-when-idle t
                  help-at-pt-timer-delay 0.01)
    ;; make help-at-pt-* settings take effect
    (help-at-pt-set-timer)
    (use-package company-emacs-eclim
      :config
      (setq company-emacs-eclim-ignore-case t)
      (company-emacs-eclim-setup))))


;; SQL
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
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  (use-package sql-indent
    :init (add-hook 'sql-mode-hook 'sql-indent))

  (use-package sqlup-mode
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    :config
    (add-to-list 'sqlup-blacklist "name"))

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

;; agressive indent mode to re-indent after changes are made
(use-package aggressive-indent
  :delight aggressive-indent-mode
  :config
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (global-aggressive-indent-mode 1))

;; custom vars
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(setq-default custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(let ((project-customizations nil))
  (ignore-errors
    (setf project-customizations (concat (projectile-project-root) "./.custom.el")))
  (when (and project-customizations (file-exists-p project-customizations))
    (load project-customizations)))

;;; init.el ends here
