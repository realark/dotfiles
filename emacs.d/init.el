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
(setq-default linum-format "%d")
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
  (message "This too shall pass."))

;Maximize emacs window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; text size
(global-set-key (kbd "C-+") 'text-scale-adjust)

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

;; try runs emacs packages without installing them
(use-package try
  :ensure t)

(use-package powerline
  :ensure t)

;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  "Change the theme."
  (mapc #'disable-theme custom-enabled-themes))
(when window-system
  (use-package tsdh
    :ensure nil)
  (use-package zenburn-theme
    :ensure nil)
  (use-package doom-themes
    :ensure nil)
  (use-package moe-theme
    :ensure t
    :config
    (load-theme 'moe-dark t)
    (moe-theme-set-color 'cyan)
    (powerline-moe-theme)))

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
(use-package ace-window
  :ensure t)

(defun select-current-line ()
  "Select the current line."
  (interactive)
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
     (add-to-list 'grep-find-ignored-files "*.fasl")
     (add-to-list 'grep-find-ignored-files "*.class")))
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-directories)
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "build")
     (add-to-list 'grep-find-ignored-directories "bin")))

;;Evil (extensible vi layer for Emacs)
(use-package evil
  :ensure t
  :init (evil-mode 1))
;;; esc quits
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Default to normal mode most of the time
(setq-default evil-insert-state-modes '(nrepl-mode shell-mode git-commit-mode term-mode))
(setq-default evil-emacs-state-modes '(magit-mode magit-popup-mode))
(setq-default evil-motion-state-modes '())

;; keybindings for eclim
(evil-define-key 'normal eclim-problems-mode-map
  (kbd "e") 'eclim-problems-show-errors
  (kbd "w") 'eclim-problems-show-warnings
  (kbd "a") 'eclim-problems-show-all
  (kbd "g") 'eclim-problems-buffer-refresh
  (kbd "q") 'eclim-quit-window
  (kbd "RET") 'eclim-problems-open-current)

;; general for keybindings
(use-package general
  :ensure t
  :init (general-evil-setup t))
;; bind a key globally in normal state; keymaps must be quoted

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

(general-define-key "C-x <SPC>" #'find-file)

;; Hydras
(use-package hydra
  :ensure t
  :config
  (general-define-key
   "C-x t"
   (defhydra hydra-test (:color blue :columns 1)
     "Run Tests"
     ("t"   #'run-unit-tests             "Unit")
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
 Split: _v_ert _x_:horz
Delete: _o_nly  _a_ce  _d_window
  Move: _s_wap _b_uffer
  Misc:  _a_ce  _u_ndo  _r_edo"
     ("h" windmove-left)
     ("j" windmove-down)
     ("k" windmove-up)
     ("l" windmove-right)
     ("H" hydra-move-splitter-left)
     ("J" hydra-move-splitter-down)
     ("K" hydra-move-splitter-up)
     ("L" hydra-move-splitter-right)
     ("|" (lambda ()
            (interactive)
            (split-window-right)
            (windmove-right)))
     ("_" (lambda ()
            (interactive)
            (split-window-below)
            (windmove-down)))
     ("v" split-window-right)
     ("x" split-window-below)
     ;; winner-mode must be enabled
     ("u" winner-undo)
     ("r" winner-redo)
     ("o" delete-other-windows :exit t)
     ("a" ace-window :exit t)
     ("s" ace-swap-window)
     ("b" switch-to-buffer)
     ("d" delete-window)
     ("a" ace-delete-window :exit t)
     ("q" nil)))
  (general-define-key
   "C-x p"
   (defhydra hydra-projectile (:color blue :columns 4)
     "Projectile"
     ("f"   projectile-find-file                "Find File")
     ("r"   projectile-recentf                  "Recent Files")
     ("t"   projectile-regenerate-tags          "Recent Files")

     ("x"   projectile-remove-known-project     "Remove Known Project")
     ("d"   projectile-find-dir                 "Find Directory")
     ("c"   projectile-invalidate-cache         "Clear Cache")
     ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

     ("o"   projectile-multi-occur              "Multi Occur")
     ("g"   projectile-grep                     "Grep all files")
     ("s"   projectile-switch-project           "Switch Project")
     ("k"   projectile-kill-buffers             "Kill Buffers")
     ("q"   nil "Cancel" :color red))))

;; Paredit
(use-package paredit
  :ensure t)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (and (require 'slime-xref-browser) ;; from slime-fancy
                                                (paredit-mode +1))))
(defun override-slime-repl-bindings-with-paredit ()
  "Stop SLIME's REPL from grabbing DEL."
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; evil bindings for paredit
(evil-define-key 'insert paredit-mode-map
  (kbd "C-l") 'paredit-forward-slurp-sexp
  (kbd "C-S-h") 'paredit-backward-slurp-sexp
  (kbd "C-h") 'paredit-forward-barf-sexp
  (kbd "C-S-l") 'paredit-backward-barf-sexp)
(evil-define-key 'normal paredit-mode-map
  (kbd "C-l") 'paredit-forward-slurp-sexp
  (kbd "C-S-h") 'paredit-backward-slurp-sexp
  (kbd "C-h") 'paredit-forward-barf-sexp
  (kbd "C-S-l") 'paredit-backward-barf-sexp)

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

;;Slime
(use-package slime
  :ensure t)
(use-package slime-company
  :ensure t)
(make-directory "/tmp/slime-fasls/" t)
(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(slime-setup '(slime-fancy slime-asdf slime-company))
(load "~/quicklisp/clhs-use-local.el" t)
(setq inhibit-splash-screen t)

(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")

(defun toggle-or-start-slime ()
  (interactive)
  (let ((slime-buffer (get-buffer "*slime-repl sbcl*")))
    (if slime-buffer
        (if (eq slime-buffer (current-buffer))
            (delete-window)
          (switch-to-buffer-other-window "*slime-repl sbcl*"))
      (slime))))

;; Toggle slime buffer
(global-set-key [f9] #'toggle-or-start-slime)

(defun my-slime-repl-kill-or-interrupt ()
  "If the user has entered text in the prompt, remove the text before and after point.
Otherwise, send an interrupt to slime."
  (interactive)
  (cond ((= (marker-position slime-repl-input-start-mark) (point))
         (slime-interrupt))
        (t (slime-repl-kill-input)
           (slime-repl-kill-input))))

;; browse local hyper spec
(load "~/quicklisp/clhs-use-local.el" t)

(evil-define-key 'normal slime-repl-mode-map
  (kbd "q") (lambda () (interactive) (end-of-buffer) (evil-insert-state) (toggle-or-start-slime)))

(evil-define-key 'insert slime-repl-mode-map
  (kbd "C-c") 'my-slime-repl-kill-or-interrupt
  (kbd "C-d") (lambda () (interactive)
                (when (y-or-n-p "Quit slime?")
                  (and (slime-repl-quit) (delete-window))))
  (kbd "C-r") 'slime-repl-previous-matching-input
  (kbd "TAB") 'completion-at-point
  (kbd "C-S-l") 'slime-repl-clear-buffer
  (kbd "C-k") 'slime-repl-previous-input
  (kbd "C-j") 'slime-repl-next-input)

(defun print-help ()
  (print "No override. Check for .custom.el?"))
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
  "4" (general-simulate-keys "5" t)
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
              (slime-eval-async
                  `(swank:inspector-pop)
                (lambda (result)
                  (cond (result
                         (slime-open-inspector result (pop slime-inspector-mark-stack)))
                        (t (quit-window)))))))

;; Magit -- The best git interface I've ever used.
(use-package magit
  :ensure t)
(setq-default magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)
(setq-default magit-push-always-verify nil)
(setq-default magit-fetch-arguments '("--prune"))
(setq-default magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))

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
  (delete-trailing-whitespace beg end))

(use-package evil-magit
  :ensure t)

;; Code folding
(use-package hideshow
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;multi-term
(use-package multi-term
  :ensure t
  :config
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

  (general-define-key "C-x s" #'get-term)

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
  :ensure t
  :config
  (fuzzy-format-mode t)
  (setq show-trailing-whitespace t))

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
(define-key evil-normal-state-map "gT" 'crs-bury-buffer)
(define-key evil-normal-state-map "gt" (lambda () (interactive) (crs-bury-buffer -1)))

;; tags
(use-package etags-select
  :ensure t)
(setq-default etags-select-go-if-unambiguous t)
(setq-default etags-select-highlight-delay 5.0)
(setq-default etags-select-use-short-name-completion t)
(setq-default tags-revert-without-query 1)
(global-set-key (kbd "<f3>") #'etags-select-find-tag-at-point)

(evil-define-key 'emacs etags-select-mode-map
  (kbd "j")       'etags-select-next-tag
  (kbd "k")       'etags-select-previous-tag
  (kbd "q")       'etags-select-quit
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous)

;; ivy and friends
(use-package ivy
  :ensure t)
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t)

(ivy-mode t)
(setq-default ivy-wrap t)
(setq-default ivy-use-virtual-buffers t)
(setq-default ivy-height 10)
(setq-default ivy-count-format "(%d/%d) ")
(setq-default ivy-extra-directories nil)
(setq-default ivy-re-builders-alist '((t . ivy--regex-plus)))

;; ivy global keys
(global-set-key (kbd "C-s") 'swiper)
;; isearch-forward seems to get stuck in the wrong state with swiper.
;; Manually setting it to T seems to work around the problem.
;; This is probably a problem with my config.
(setq isearch-forward t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; ivy minibuffer keys
(define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "M-S-j") 'ivy-scroll-up-command)
(define-key ivy-minibuffer-map (kbd "M-S-k") 'ivy-scroll-down-command)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-history-element)

(define-key counsel-find-file-map (kbd "<return>") 'ivy-alt-done)

(setq-default magit-completing-read-function 'ivy-completing-read)
(setq-default projectile-completion-system 'ivy)


;Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-by-copying t)
;; Desktop save files
;; Automatically save and restore sessions from the current dir
(setq-default desktop-dirname             "./"
              desktop-base-file-name      ".emacs.desktop"
              desktop-base-lock-name      ".lock"
              desktop-path                (list desktop-dirname)
              ;; desktop-save                t
              desktop-files-not-to-save   "^\*.*\*$"
              desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;; Projectile
(use-package projectile
  :ensure t)
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
(projectile-global-mode)
(global-set-key (kbd "C-S-F") #'projectile-find-file)
(global-set-key (kbd "C-S-T") #'projectile-find-tag)

;Org mode
(use-package org
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq-default org-startup-indented t)
;; fontify code in code blocks
(setq-default org-src-fontify-natively t)
(use-package evil-org
  :ensure t)
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq-default org-agenda-files (list "~/Documents/org-files/tasks.org"
                                     "~/Documents/org-files/schedule.org"))
(setq-default org-log-done 'time)
(setq-default org-enforce-todo-dependencies t)

(setq-default org-refile-targets '((nil :maxlevel . 9)))
(setq-default org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq-default org-refile-use-outline-path t)                  ; Show full paths for refiling

; Neotree
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(setq-default neo-smart-open t)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-change-root)))

;; elfeed
(use-package elfeed
  :ensure t)
(elfeed-load-opml "~/.admin/arks_feeds.opml")

;; Dired options
(use-package dired+
  :ensure t)
(diredp-toggle-find-file-reuse-dir 1)

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;Flycheck
(use-package flycheck
  :ensure t)
(global-flycheck-mode)

; spellcheck
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
        text-mode-hook))

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

(setq-default mumamo-background-colors nil)

;; Interface to eclipse via eclim
(use-package eclim
  :ensure t
  :init
  (require 'eclim)
  (require 'eclimd)
  (global-eclim-mode))

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
(add-hook 'eclim-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-g") #'eclim-java-find-references)
            (local-set-key (kbd "<f4>") #'eclim-personal-find-implementors)
            (local-set-key (kbd "C-M-h") #'eclim-java-call-hierarchy)))

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; yasnippet
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)
;; don't turn on yas if there are no snippets
(defun disable-yas-if-no-snippets ()
  (when (and yas-minor-mode (null (yas--get-snippet-tables)))
    (yas-minor-mode -1)))
(add-hook 'yas-minor-mode-hook #'disable-yas-if-no-snippets)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
(use-package common-lisp-snippets
  :ensure t)

;; company mode
(use-package company
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-quickhelp
  :ensure t)
(company-quickhelp-mode t)
(setq-default company-idle-delay 0.25)
(setq-default company-minimum-prefix-length 2)

(use-package company-emacs-eclim
  :ensure t)
(setq company-emacs-eclim-ignore-case t)
(company-emacs-eclim-setup)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "M-j") #'company-select-next)
(define-key company-active-map (kbd "M-k") #'company-select-previous)

;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq-default company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; make company work with org-mode
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;; Groovy and Gradle
(use-package groovy-mode
  :ensure t
  :init (require 'inf-groovy))
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)))
(setq-default inferior-groovy-mode-hook
              '(lambda()
                 (setq groovy-home "/usr/share/groovy")))

;; Clojure
(use-package ac-cider
  :ensure t)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

;; Scala
; (add-to-list 'auto-mode-alist '("\.scala$" . scala-mode))
(use-package ensime
  :ensure t)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; SQL
(use-package sql-indent
  :ensure t
  :init (add-hook 'sql-mode-hook 'sql-indent))

(use-package sqlup-mode
  :ensure t
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
(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

;; agressive indent mode to re-indent after changes are made
(use-package aggressive-indent
  :ensure t)
(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

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
