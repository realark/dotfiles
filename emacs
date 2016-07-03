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
(put 'narrow-to-region 'disabled nil)
(setq help-window-select t)
(setq echo-keystrokes 0.1)

;; Disable GC in the minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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

;; use mepla and marmalade for package
(require-install 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; try runs emacs packages without installing them
(require-install 'try)

;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))
(if window-system
 (load-theme 'tsdh-dark t))

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
(require-install 'evil-leader)
(global-evil-leader-mode) ; evil-leader must load first
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>"  'execute-extended-command
  "3"      'toggle-window-split
  "l"      'whitespace-mode
  "f"      'indent-region
  ";"      'toggle-comment-region-or-line
  "<tab>"  'hs-toggle-hiding
  "e b"    'eclim-project-build
  "e p"    'eclim-problems
  "e c"    'eclim-problems-correct
  "e j"    'eclim-personal-switch-to-junit-buffer-and-run
  "g"      'magit-status
  "B"      'magit-blame
  "b"      'magit-blame-toggle)

(require-install 'evil)
(evil-mode 1)
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
                          (grep-mode . insert))
  do (evil-set-initial-state mode state))

;; grep-mode bindings
(evil-define-key 'insert grep-mode-map
  (kbd "q") 'quit-window
  (kbd "j") 'next-error-no-select
  (kbd "k") 'previous-error-no-select)

;; keybindings for eclim
(evil-define-key 'normal eclim-problems-mode-map
  (kbd "e") 'eclim-problems-show-errors
  (kbd "w") 'eclim-problems-show-warnings
  (kbd "a") 'eclim-problems-show-all
  (kbd "g") 'eclim-problems-buffer-refresh
  (kbd "q") 'eclim-quit-window
  (kbd "RET") 'eclim-problems-open-current)

;;Slime
;(setq inferior-lisp-program "/usr/bin/rlwrap -c -H ~/.sbcl_history /usr/bin/sbcl --noinform")
(require-install 'slime)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(slime-setup '(slime-fancy slime-asdf))
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

(setq magit-fetch-arguments '("--prune"))
(setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))


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

(require-install 'evil-magit)

;; Code folding
(load-library "hideshow")
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'scala-mode-hook      'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'json-mode-hook       'hs-minor-mode)

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

(evil-define-key 'normal term-raw-map
  (kbd "p") 'term-paste
  (kbd "RET") 'term-send-return
  (kbd "C-d") 'term-send-eof)

(evil-define-key 'insert term-raw-map
  (kbd "C-v") 'term-paste
  (kbd "C-d") 'term-send-eof
  (kbd "C-a") 'term-send-raw)

;;Turn off tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Adapt to the whitespace style of the file we're editing
(require-install 'fuzzy-format)
(fuzzy-format-mode t)
(setq show-trailing-whitespace t)

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
;; evil buffer nav keys
(define-key evil-normal-state-map "gT" 'crs-bury-buffer)
(define-key evil-normal-state-map "gt" (lambda () (interactive) (crs-bury-buffer -1)))

;; tags
(require-install 'etags-select)
(setq etags-select-go-if-unambiguous t)
(setq etags-select-highlight-delay 5.0)
(setq etags-select-use-short-name-completion t)
(setq tags-revert-without-query 1)
(global-set-key (kbd "<f3>") #'etags-select-find-tag-at-point)

(evil-define-key 'emacs etags-select-mode-map
  (kbd "j")       'etags-select-next-tag
  (kbd "k")       'etags-select-previous-tag
  (kbd "q")       'etags-select-quit
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous)

;; ido
(require-install 'ido)
(ido-mode t)
(ido-everywhere)
(require-install 'ido-ubiquitous)
(ido-ubiquitous-mode)
(require-install 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-create-new-buffer 'always)
(setq magit-completing-read-function #'magit-ido-completing-read)

;Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
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

;Projectile
(require-install 'projectile)
(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories
              '(".git" ".ensime_cache.d" ".gradle"
                ".recommenders" ".metadata" "dist")))
(setq projectile-globally-ignored-files
      (append projectile-globally-ignored-files
              '(".ensime" "*.war" "*.jar" "*.zip"
                "*.png" "*.gif" "*.vsd" "*.svg"
                "*.exe" "eclimd.log" "workbench.xmi"
                ".emacs.desktop" "*.deb" "*.gz")))
(setq projectile-enable-caching t)
(projectile-global-mode)
(global-set-key (kbd "C-S-F") #'projectile-find-file)
(global-set-key (kbd "C-S-T") #'projectile-find-tag)

;Org mode
(require-install 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-inhibit-startup-visibility-stuff t)
(setq org-startup-indented t)
;; fontify code in code blocks
(setq org-src-fontify-natively t)
(require-install 'evil-org)
(require-install 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Neotree
(require-install 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-change-root)))

;Dired options
(require-install 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(require-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;Flycheck
(require-install 'flycheck)
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
(setq path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'lisp-mode-common-hook  'turn-on-ctags-auto-update-mode)

;;;;;;;;;;;;;;

(setq mumamo-background-colors nil)

;; Interface to eclipse via eclim
(require-install 'eclimd)
(require-install 'eclim)
(global-eclim-mode)

(defun eclim-personal-switch-to-junit-buffer-and-run ()
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

(setq eclim-auto-save t
      eclim-executable "/usr/lib/eclipse/eclim"
      eclimd-executable "/usr/lib/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/workspace"
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)
(add-hook 'eclim-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-g") #'eclim-java-find-references)
            (local-set-key (kbd "<f4>") #'eclim-personal-find-implementors)
            (local-set-key (kbd "C-M-h") #'eclim-java-call-hierarchy)))

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require-install 'auto-complete-config)
(ac-config-default)
(require-install 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

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
(require-install 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Scala
; (add-to-list 'auto-mode-alist '("\.scala$" . scala-mode))
(require-install 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
