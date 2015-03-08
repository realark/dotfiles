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
(if (> emacs-major-version 20)
(tool-bar-mode -1))
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;;Show line numbers
(global-linum-mode t)
(column-number-mode t)
(line-number-mode t)
(setq linum-format "%d ")
;Standard copy+pate keys
(cua-mode 1)
;Insert closing bracket
(electric-pair-mode 1)
(show-paren-mode 1) ; turn on paren match highlighting
;(setq show-paren-style 'expression) ; highlight entire bracket expression



;;Marmalade Package Archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;Slime
;(setq inferior-lisp-program "/usr/bin/rlwrap -c -H ~/.sbcl_history /usr/bin/sbcl --noinform")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'slime)
;(global-set-key (kbd "C-c C-c") 'slime-compile-defun)
;(global-set-key (kbd "C-c C-k") 'slime-compile-and-load-file)
(load "~/quicklisp/clhs-use-local.el" t)
(setq inhibit-splash-screen t)

;;Evil (extensible vi layer for Emacs)
(require 'evil)
(evil-mode 1)

;(defun slime-repl-bol-insert ()
;  (interactive)
;  (slime-repl-bol)
;  (evil-insert 1))
;(define-key evil-normal-state-map "I" 'slime-repl-bol-insert)

;;Git
;;(add-to-list 'load-path "~/.emacs.d/elpa/git-emacs")
;;(require 'git-emacs)
(require 'magit)

(if window-system
 (load-theme 'deeper-blue t))

;;Turn off tabs. I don't really give a shit about tabs vs spaces, but most people I meet seem to think tabs are the spawn of Satan.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;;Emacs Code Browser
(require 'ecb)
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
 '(ecb-layout-window-sizes (quote (("left-analyse" (ecb-directories-buffer-name 0.1694915254237288 . 0.25862068965517243) (ecb-sources-buffer-name 0.1694915254237288 . 0.4827586206896552) (ecb-methods-buffer-name 0.1694915254237288 . 0.10344827586206896) (ecb-analyse-buffer-name 0.1694915254237288 . 0.13793103448275862)) ("left8" (ecb-directories-buffer-name 0.225531914893617 . 0.3103448275862069) (ecb-sources-buffer-name 0.225531914893617 . 0.22413793103448276) (ecb-methods-buffer-name 0.225531914893617 . 0.27586206896551724) (ecb-history-buffer-name 0.225531914893617 . 0.1724137931034483)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((Base . 10) (Syntax . ANSI-Common-Lisp))))
 '(tool-bar-mode nil))
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
; this is the list of buffers I never want to see
(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*" "*Completions*" "*inferior-lisp*" "*slime-repl sbcl*"))
; might as well use this for both
(setq iswitchb-buffer-ignore (append '("^ " "*Buffer") crs-hated-buffers))
(defun crs-hated-buffers ()
  "List of buffers I never want to see, converted from names to buffers."
  (delete nil
          (append
           (mapcar 'get-buffer crs-hated-buffers)
           (mapcar (lambda (this-buffer)
                     (if (string-match "^ " (buffer-name this-buffer))
                         this-buffer))
                   (buffer-list)))))
; I'm sick of switching buffers only to find KILL right in front of me
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
(global-set-key [(control tab)] 'crs-bury-buffer)
(global-set-key [(control meta tab)] (lambda ()
                                       (interactive)
                                       (crs-bury-buffer -1)))
(add-to-list 'crs-hated-buffers "*Messages*")
(add-to-list 'crs-hated-buffers "*scratch*")
(add-to-list 'crs-hated-buffers "*slime-events*")
(add-to-list 'crs-hated-buffers "*inferior-lisp*")
(add-to-list 'crs-hated-buffers "help")

;Buffer nav keys
(global-set-key (kbd "C-x l") 'next-buffer)
(global-set-key (kbd "C-x h") 'previous-buffer)
(global-set-key [C-tab] 'crs-bury-buffer)
(global-set-key [C-S-iso-lefttab] (lambda () (interactive) (crs-bury-buffer -1)))

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
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;(global-auto-complete-mode t)
;Flymake: on the fly code checking
(require 'flymake-easy)
(require 'flymake-cursor)
;(add-to-list 'auto-mode-alist '("\\.lisp$" . common-lisp-mode))
;(add-hook 'lisp-mode-hook 'flymake-lisp-load)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;(add-hook 'php-mode-hook 'flymake-phpcs-load)
(add-hook 'php-mode-hook 'flymake-php-load)

(require 'flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)

(setq mumamo-background-colors nil) 

;nXhtml for web stuff
(load "~/.emacs.d/nxhtml/autostart")
(autoload 'nxhtml-mode "nxhtml-mode" "Major mode for editing html and templates." t)
(add-to-list 'auto-mode-alist '("\\.jsp$" . nxhtml-mode))


;(load "~/.emacs.d/tkj-java.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to eclipse via eclim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eclim)
(global-eclim-mode)

;; Variables
(setq eclim-auto-save t
;;      eclim-executable "/opt/eclipse/eclim"
;;      eclimd-executable "/opt/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/src/workspace-eclim"
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;; Groovy and Gradle
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;; Clojure
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
