;; -*- emacs-lisp -*-
; Text and the such
;; Use colors to highlight commands, etc.
(global-font-lock-mode t) 
;; Disable the welcome message
(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
;; Always end a file with a newline
(setq require-final-newline nil)
;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)
;; Flash instead of that annoying bell
(setq visible-bell t)
;; Remove icons toolbar
(tool-bar-mode -1)
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;;Show line numbers
(global-linum-mode t)
(column-number-mode t)
(line-number-mode t)
(setq linum-format "%d")
;Standard copy+pate keys
(cua-mode 1)
;Insert closing bracket
(electric-pair-mode 1)
(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-style 'expression) ; highlight entire bracket expression
(setq large-file-warning-threshold 100000000) ;100mb
(setq create-lockfiles nil) ; Don't create # lock files
(blink-cursor-mode -1)

(if window-system
 (load-theme 'tango-dark t))

; change the minbuffer startup message
(defun display-startup-echo-area-message ()
  (message "This too shall pass."))

;Maximize emacs window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; text size
(global-set-key (kbd "C-+") 'text-scale-adjust)

;; Require-install Macro
(defmacro require-install (PCK)
  "Require package PCK, install via package-install if missing"
  `(unless (require ,PCK nil t)
     (package-install ,PCK)
     (require ,PCK)))

;;Marmalade Package Archive
(require-install 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; toggle two-window split between horizontal and vertical
(defun toggle-window-split ()
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

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun toggle-comment-region-or-line ()
  "Comment or uncomment the selected region. If no region is selected use the current line."
  (interactive)
  (if (not mark-active)
      (select-current-line))
  (comment-or-uncomment-region (region-beginning) (region-end)))

;; Options for M-x rgrep
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-directories)
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "build")
     (add-to-list 'grep-find-ignored-directories "bin")))

;;Evil (extensible vi layer for Emacs)
(global-evil-leader-mode) ; evil-leader must load first
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>"  'execute-extended-command
  "3"      'toggle-window-split
  "l"      'whitespace-mode
  "f"      'indent-region
  ";"      'toggle-comment-region-or-line
  "<tab>"  'hs-toggle-hiding
  "g"      'magit-status
  "B"      'magit-blame
  "b"      'magit-blame-toggle)

(require-install 'evil)
(evil-mode 1)
(require-install 'evil-magit)
;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                          (nrepl-mode . insert)
                          (comint-mode . normal)
                          (shell-mode . insert)
                          (git-commit-mode . insert)
                          (term-mode . insert)
                          (grep-mode . emacs))
  do (evil-set-initial-state mode state))

;;Slime
;(setq inferior-lisp-program "/usr/bin/rlwrap -c -H ~/.sbcl_history /usr/bin/sbcl --noinform")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require-install 'slime)
;(global-set-key (kbd "C-c C-c") 'slime-compile-defun)
;(global-set-key (kbd "C-c C-k") 'slime-compile-and-load-file)
(load "~/quicklisp/clhs-use-local.el" t)
(setq inhibit-splash-screen t)

;;Git
(require-install 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-push-always-verify nil)

(defun magit-blame-toggle ()
  "Toggle magit-blame-mode on and off interactively."
  (interactive)
  (if (and (boundp 'magit-blame-mode) magit-blame-mode)
      (magit-blame-quit)
    (call-interactively 'magit-blame)))

;; Code folding
(load-library "hideshow")
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'scala-mode-hook      'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;;multi-term
(require-install 'multi-term)
(setq multi-term-program "/bin/zsh")
(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))
(defun get-term ()
        "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
        (interactive)
        (let ((b (last-term-buffer (buffer-list))))
          (if (or (not b) (eq 'term-mode major-mode))
              (multi-term)
                (switch-to-buffer b))))
(global-set-key (kbd "C-x t") 'get-term)

(evil-define-key 'normal term-raw-map (kbd "RET") 'term-send-return)
(evil-define-key 'normal term-raw-map (kbd "p") 'term-paste)
(evil-define-key 'insert term-raw-map (kbd "C-v") 'term-paste)

(evil-define-key 'normal term-raw-map (kbd "C-d") 'term-send-eof)
(evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-eof)

;(add-hook 'term-mode-hook
;          (lambda ()
;            (add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))
;            (add-to-list 'term-bind-key-alist '("C-d" . term-send-eof))))


;;Turn off tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Adapt to the whitespace style of the file we're editing
(require-install 'fuzzy-format)
(fuzzy-format-mode t)

;;Emacs Code Browser
(require-install 'ecb)
(global-set-key (kbd "C-c . t") 'ecb-toggle-ecb-windows)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(desktop-enable t nil (desktop))
 '(ecb-auto-activate nil)
 '(ecb-compile-window-height 6)
 '(ecb-layout-window-sizes
   (quote
    (("left-analyse"
      (ecb-directories-buffer-name 0.1694915254237288 . 0.25862068965517243)
      (ecb-sources-buffer-name 0.1694915254237288 . 0.4827586206896552)
      (ecb-methods-buffer-name 0.1694915254237288 . 0.10344827586206896)
      (ecb-analyse-buffer-name 0.1694915254237288 . 0.13793103448275862))
     ("left8"
      (ecb-directories-buffer-name 0.225531914893617 . 0.3103448275862069)
      (ecb-sources-buffer-name 0.225531914893617 . 0.22413793103448276)
      (ecb-methods-buffer-name 0.225531914893617 . 0.27586206896551724)
      (ecb-history-buffer-name 0.225531914893617 . 0.1724137931034483)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(inhibit-startup-screen t)
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(safe-local-variable-values (quote ((Base . 10) (Syntax . ANSI-Common-Lisp))))
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
                             (setq badbuffer this-buffer)))))
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
;Buffer nav keys
(define-key evil-normal-state-map "gT" 'crs-bury-buffer)
(define-key evil-normal-state-map "gt" (lambda () (interactive) (crs-bury-buffer -1)))

;(require 'icicle)
;(icy-mode 1)
;(setq icicle-show-Completions-initially-flag t)
;(setq icicle-top-level-when-sole-completion-flag t)

;(require 'helm)
;(require 'helm-config)
;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;(helm-mode 1)

(require-install 'etags-select)
(setq etags-select-go-if-unambiguous t)
(setq etags-select-highlight-delay 5.0)
(setq etags-select-use-short-name-completion t)
(setq tags-revert-without-query 1)
(require-install 'ido)
(ido-mode t)
(ido-everywhere)
(require-install 'ido-ubiquitous)
(ido-ubiquitous-mode)
(require-install 'ido-vertical-mode)
(ido-vertical-mode)
(setq magit-completing-read-function #'magit-ido-completing-read)
;(defun my-ido-find-tag ()
;  "Find a tag using ido"
;  (interactive)
;  (let (tag-names)
;    (map (lambda (x)
;           (push (prin1-to-string x t) tag-names))
;         (tags-completion-at-point-function)) ; not working
;    (find-tag (ido-completing-read "Tag: " tag-names))))

;(defun find-tag-no-prompt ()
;  "Jump to the tag at point without prompting"
;  (interactive)
;  (find-tag (find-tag-default)))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key (kbd "<f3>") #'etags-select-find-tag-at-point)
(global-set-key (kbd "C-<f3>") #'tags-search)
(global-set-key (kbd "C-S-T") #'ido-find-file-in-tag-files)

;Backup files
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)
;Desktop save files
;; Automatically save and restore sessions from the current dir
(setq desktop-dirname             "./"
      desktop-base-file-name      ".emacs.desktop"
      desktop-base-lock-name      ".lock"
      desktop-path                (list desktop-dirname)
      ;desktop-save                t
      desktop-files-not-to-save   "^\*.*\*$"
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;Org mode
(require-install 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; fontify code in code blocks
(setq org-src-fontify-natively t)

;Dired options
(diredp-toggle-find-file-reuse-dir 1)

(require-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;(global-auto-complete-mode t)
;Flymake: on the fly code checking
(require-install 'flymake-easy)
(require-install 'flymake-cursor)
;(add-to-list 'auto-mode-alist '("\\.lisp$" . common-lisp-mode))
;(add-hook 'lisp-mode-hook 'flymake-lisp-load)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

; spellcheck
(add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))

;; ctags
(setq path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'lisp-mode-common-hook  'turn-on-ctags-auto-update-mode)

;;;;;;;;;;;;;;

(require-install 'flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)

(setq mumamo-background-colors nil) 

;; Hook up auto complete mode
(require-install 'auto-complete-config)
(ac-config-default)

;; Groovy and Gradle

(require-install 'groovy-mode)
(require-install 'inf-groovy)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)))
(setq inferior-groovy-mode-hook
      '(lambda()
         (setq groovy-home "/usr/share/groovy")))

;; Clojure
(require-install 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Scala
; (add-to-list 'auto-mode-alist '("\.scala$" . scala-mode))
(require-install 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)
