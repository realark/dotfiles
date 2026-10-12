;;; my-java-lsp.el --- NetBeans Java language server -*- lexical-binding: t; -*-

;;; Commentary:
;; Opening the first Java buffer automatically downloads the pinned server if
;; missing, then starts LSP.  Later buffers reuse the installation.  A JDK 21+
;; must already be installed.  M-x lsp-install-server RET my-java-netbeans RET
;; remains available for manual reinstall; M-x lsp-workspace-restart restarts LSP.
;; Gradle import executes the project's build scripts: trust only projects you
;; would build locally.  Annotation processors (including Lombok) come from
;; Gradle's source-set dependencies, not a global Lombok agent.  The server JVM
;; is independent of the project's Java toolchains and source/target levels.
;; NetBeans owns its userdir lock.  Never delete a live server's lock or share
;; this root's userdir with a second Emacs; stop the first workspace instead.
;; NetBeans supplies completion and hover for Lombok-generated methods, but
;; does not return a declaration location for those synthetic accessors.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'lsp-mode)
(require 'my-java-gradle)

(defgroup my-java-lsp nil
  "Java editing with the Apache NetBeans language server."
  :group 'lsp-mode)

(defcustom my-java-lsp-version "28.0.1"
  "ASF Marketplace extension version to install.
This pins the bundled javac as well as NetBeans.  28.0.1 works with the
project's Lombok 1.18.42; 31's bundled javac 27 does not.  Upgrading the
server runtime JDK alone does not change that bundled compiler."
  :type 'string :group 'my-java-lsp)

(defcustom my-java-lsp-java-home nil
  "Full JDK 21+ home for the server, or nil to discover one.
Discovery tries JAVA_HOME, macOS java_home -v 21, then PATH's java.
This does not override Gradle project toolchains."
  :type '(choice (const :tag "Discover" nil) directory)
  :group 'my-java-lsp)

(defcustom my-java-lsp-install-directory nil
  "Existing extracted extension directory, or nil for managed installation.
An override must contain package.json and nbcode/.  Installation always
uses the managed version directory, never overwrites this override."
  :type '(choice (const :tag "Managed installation" nil) directory)
  :group 'my-java-lsp)

(defcustom my-java-lsp-settings
  '(("netbeans.java.onSave.organizeImports" . :json-false))
  "Dotted NetBeans settings overriding bundled package.json defaults.
Use :json-false for JSON false, nil for JSON null, vectors for arrays,
and hash tables for objects.  Restart after editing this variable.
The server's user-initiated config/update requests save this variable
through Customize and notify the requesting workspace immediately."
  :type '(alist :key-type string :value-type sexp)
  :group 'my-java-lsp)

(defun my-java-lsp--managed-directory ()
  (expand-file-name (concat "netbeans/" my-java-lsp-version "/extension")
                    lsp-server-install-dir))

(defun my-java-lsp--directory ()
  (expand-file-name (or my-java-lsp-install-directory
                        (my-java-lsp--managed-directory))))

(defun my-java-lsp--launcher ()
  (expand-file-name "nbcode/bin/nbcode" (my-java-lsp--directory)))

(defun my-java-lsp--installed-p ()
  ;; Do not evaluate the command here: that would run Gradle during discovery.
  (and (file-executable-p (my-java-lsp--launcher))
       (file-executable-p
        (expand-file-name "nbcode/platform/lib/nbexec" (my-java-lsp--directory)))
       (file-readable-p (expand-file-name "package.json" (my-java-lsp--directory)))))

(defvar my-java-lsp--configuration-cache nil)

(defun my-java-lsp--install (_client callback error-callback _update)
  (if my-java-lsp-install-directory
      (if (my-java-lsp--installed-p)
          (funcall callback)
        (funcall error-callback
                 "Invalid my-java-lsp-install-directory; unset it to install NetBeans"))
    (let* ((directory (my-java-lsp--managed-directory))
           (parent (file-name-directory directory)))
      (make-directory parent t)
      ;; The lsp-mode downloader cannot replace nonempty directories.  Stage
      ;; extraction first so a failed download also preserves the working server.
      (let* ((stage (make-temp-file (expand-file-name ".install-" parent) t))
             (extension (expand-file-name "extension" stage))
             (fail (lambda (err)
                     (when (file-directory-p stage) (delete-directory stage t))
                     (funcall error-callback err))))
        (lsp-download-install
         (lambda ()
           (condition-case err
               (progn
                 (dolist (relative '("nbcode/bin/nbcode" "nbcode/platform/lib/nbexec"))
                   (let ((file (expand-file-name relative extension)))
                     (set-file-modes file (logior (file-modes file) #o111))))
                 (when (file-directory-p directory) (delete-directory directory t))
                 (rename-file extension directory)
                 (delete-directory stage t)
                 (setq my-java-lsp--configuration-cache nil)
                 (funcall callback))
             (error (funcall fail err))))
         fail
         :url (format (concat "https://marketplace.visualstudio.com/_apis/public/"
                              "gallery/publishers/asf/vsextensions/"
                              "apache-netbeans-java/%s/vspackage")
                      my-java-lsp-version)
         :decompress :zip :store-path extension)))))

(defun my-java-lsp--jdk-p (home)
  "Whether HOME contains a full JDK whose release is at least 21."
  (and home (not (string-empty-p home))
       (file-executable-p (expand-file-name "bin/java" home))
       (file-executable-p (expand-file-name "bin/javac" home))
       (file-readable-p (expand-file-name "release" home))
       (with-temp-buffer
         (insert-file-contents (expand-file-name "release" home))
         (and (re-search-forward "^JAVA_VERSION=\"\\([0-9]+\\)" nil t)
              (>= (string-to-number (match-string 1)) 21)))))

(defun my-java-lsp--java-home ()
  (if my-java-lsp-java-home
      (if (my-java-lsp--jdk-p my-java-lsp-java-home)
          (file-truename my-java-lsp-java-home)
        (user-error "my-java-lsp-java-home must name a full JDK 21+ (java, javac and release)"))
    (let ((home (getenv "JAVA_HOME")))
      (unless (my-java-lsp--jdk-p home)
        (setq home
              (when (and (eq system-type 'darwin)
                         (file-executable-p "/usr/libexec/java_home"))
                (with-temp-buffer
                  (when (eq 0 (call-process "/usr/libexec/java_home" nil '(t nil) nil "-v" "21"))
                    (string-trim (buffer-string)))))))
      (unless (my-java-lsp--jdk-p home)
        (setq home
              (when-let* ((java (executable-find "java")))
                (file-name-directory
                 (directory-file-name (file-name-directory (file-truename java)))))))
      (unless (my-java-lsp--jdk-p home)
        (user-error "NetBeans needs a full JDK 21+; set my-java-lsp-java-home or JAVA_HOME"))
      (file-truename home))))

(defun my-java-lsp--cache-directory (root)
  (expand-file-name
   (concat "netbeans/" my-java-lsp-version "/"
           (secure-hash 'sha256 (file-name-as-directory (file-truename root))))
   (expand-file-name ".cache/" user-emacs-directory)))

(defun my-java-lsp--command ()
  "Build the command once at startup, with the workspace root as cwd."
  (let* ((root (file-name-as-directory (file-truename default-directory)))
         (cache (my-java-lsp--cache-directory root))
         (userdir (expand-file-name "user" cache))
         (java (my-java-lsp--java-home)))
    ;; Let NetBeans acquire its native lock, including recovery of stale locks.
    (make-directory userdir t)
    (make-directory (expand-file-name "cache" cache) t)
    (append (list (my-java-lsp--launcher)
                  "--jdkhome" java
                  "--userdir" userdir
                  "--cachedir" (expand-file-name "cache" cache)
                  ;; Never forward a second launch into another Emacs' JVM.
                  ;; NetBeans then reports ALREADY_RUNNING on lock contention.
                  "-J-Dorg.netbeans.CLIHandler.server=false"
                  "--modules" "--direct-disable"
                  "org.netbeans.modules.nbcode.integration.java"
                  "--start-java-language-server=stdio")
            ;; Import failures are actionable; never start with guessed levels.
            (my-java-gradle-launch-arguments root))))

(defun my-java-lsp--put-setting (tree path value)
  "Set dotted PATH to VALUE in TREE, creating intermediate objects."
  (let ((parts (split-string path "\\.")))
    (dolist (part (butlast parts))
      (unless (hash-table-p (gethash part tree))
        (puthash part (make-hash-table :test 'equal) tree))
      (setq tree (gethash part tree)))
    (puthash (car (last parts)) value tree)))

(defun my-java-lsp--configuration ()
  "Return the cached settings tree, preserving JSON false and null."
  (let ((file (expand-file-name "package.json" (my-java-lsp--directory))))
    (unless (and (equal file (nth 0 my-java-lsp--configuration-cache))
                 (equal my-java-lsp-settings (nth 1 my-java-lsp--configuration-cache)))
      (let* ((package (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'hash-table :array-type 'array
                                           :null-object nil :false-object :json-false)))
             (configuration (gethash "configuration" (gethash "contributes" package)))
             (tree (make-hash-table :test 'equal)))
        (mapc (lambda (group)
                (maphash (lambda (key spec)
                           (my-java-lsp--put-setting tree key (gethash "default" spec)))
                         (gethash "properties" group)))
              (if (vectorp configuration) configuration (list configuration)))
        (dolist (entry my-java-lsp-settings)
          (my-java-lsp--put-setting tree (car entry) (cdr entry)))
        (setq my-java-lsp--configuration-cache
              (list file (copy-tree my-java-lsp-settings) tree))))
    (nth 2 my-java-lsp--configuration-cache)))

(defun my-java-lsp--setting (section)
  (let ((value (my-java-lsp--configuration)))
    (dolist (part (and section (split-string section "\\." t)))
      (setq value (and (hash-table-p value) (gethash part value))))
    value))

(defun my-java-lsp--workspace-configuration (_workspace params)
  (vconcat (mapcar (lambda (item) (my-java-lsp--setting (lsp-get item :section)))
                  (append (lsp-get params :items) nil))))

(defun my-java-lsp--update-configuration (workspace params)
  (let* ((section (lsp-get params :section))
         (key (lsp-get params :key))
         (path (if (or (null section) (string-empty-p section)) key
                 (concat section "." key)))
         (value (lsp-get params :value))
         (previous (my-java-lsp--setting path)))
    ;; lsp-mode decodes incoming JSON false as nil.  Recover boolean settings
    ;; from their schema-backed current value; other nil values remain null.
    (when (and (null value) (memq previous '(t :json-false)))
      (setq value :json-false))
    (customize-save-variable
     'my-java-lsp-settings
     (cons (cons path value) (assoc-delete-all path (copy-tree my-java-lsp-settings))))
    (with-lsp-workspace workspace
      (lsp-notify "workspace/didChangeConfiguration"
                  (list :settings (my-java-lsp--configuration))))
    nil))

(defun my-java-lsp--prompt (params field)
  (concat (string-join (delq nil (list (lsp-get params :title)
                                      (lsp-get params field))) " — ") ": "))

(defun my-java-lsp--pick (params)
  "Prompt with PARAMS and return original items, including opaque userData."
  (let ((index 0) choices selected)
    (mapc (lambda (item)
            (let ((label (format "%d: %s%s%s" (cl-incf index)
                                 (or (lsp-get item :label) "")
                                 (if-let* ((description (lsp-get item :description)))
                                     (concat " — " description) "")
                                 (if-let* ((detail (lsp-get item :detail)))
                                     (concat " — " detail) ""))))
              (push (cons label item) choices)
              (when (eq t (lsp-get item :picked)) (push label selected))))
          (lsp-get params :items))
    (setq choices (nreverse choices) selected (nreverse selected))
    (let* ((prompt (my-java-lsp--prompt params :placeHolder))
           (many (eq t (lsp-get params :canPickMany)))
           (answers (if many
                        (completing-read-multiple prompt choices nil t nil nil selected)
                      (list (completing-read prompt choices nil t nil nil (car selected))))))
      (vconcat (delq nil (mapcar (lambda (answer) (cdr (assoc answer choices))) answers))))))

(defun my-java-lsp--quick-pick (_workspace params)
  (condition-case nil (my-java-lsp--pick params) (quit nil)))

(defun my-java-lsp--input (params)
  (let ((prompt (my-java-lsp--prompt params :prompt))
        (value (or (lsp-get params :value) "")))
    (if (eq t (lsp-get params :password))
        (read-passwd prompt nil value)
      (read-string prompt value))))

(defun my-java-lsp--input-box (_workspace params)
  (condition-case nil (my-java-lsp--input params) (quit nil)))

(defun my-java-lsp--multi-step (workspace params callback)
  "Run NetBeans' input/step and input/validate protocol asynchronously.
C-g cancels the entire wizard with JSON null; no partial success is sent."
  (let ((data (make-hash-table :test 'equal))
        (id (lsp-get params :id)))
    (cl-labels
        ((failed (err)
           (message "NetBeans input failed: %s" err)
           (funcall callback nil))
         (request (method step receive)
           (with-lsp-workspace workspace
             (lsp-request-async method (list :inputId id :step step :data data)
                                receive :mode 'detached :error-handler #'failed)))
         (input (step spec)
           (condition-case err
               (progn
                 (puthash (lsp-get spec :stepId) (my-java-lsp--input spec) data)
                 (request "input/validate" step
                          (lambda (problem)
                            (if (and problem (not (equal problem "")))
                                (progn (message "%s" problem) (input step spec))
                              (next (1+ step))))))
             (quit (funcall callback nil))
             (error (failed (error-message-string err)))))
         (next (step)
           (request "input/step" step
                    (lambda (spec)
                      (condition-case err
                          (cond
                           ((null spec) (funcall callback data))
                           ((lsp-member? spec :items)
                            (puthash (lsp-get spec :stepId) (my-java-lsp--pick spec) data)
                            (next (1+ step)))
                           ((lsp-member? spec :value) (input step spec))
                           (t (failed "Unrecognized input step")))
                        (quit (funcall callback nil))
                        (error (failed (error-message-string err))))))))
      (next 1))))

(defun my-java-lsp--save-documents (_workspace params)
  "Save modified visited buffers named by PARAMS; report actual success."
  (let ((ok t))
    (mapc
     (lambda (uri)
       (condition-case err
           (let* ((path (lsp--uri-to-path uri))
                  (buffers (cl-remove-if-not
                            (lambda (buffer)
                              (with-current-buffer buffer
                                (and buffer-file-name
                                     (equal (file-truename buffer-file-name)
                                            (file-truename path)))))
                            (buffer-list))))
             (unless buffers (setq ok nil))
             (dolist (buffer buffers)
               (with-current-buffer buffer
                 (when (buffer-modified-p) (save-buffer))
                 (when (buffer-modified-p) (setq ok nil)))))
         (error (setq ok nil)
                (message "NetBeans document save failed: %s" (error-message-string err)))))
     (lsp-get params :documents))
    (if ok t :json-false)))

(defun my-java-lsp--output-buffer (workspace name)
  (get-buffer-create
   (format "*NetBeans %s: %s*" (lsp--workspace-root workspace) name)))

(defun my-java-lsp--output-write (workspace params)
  (with-current-buffer (my-java-lsp--output-buffer workspace (lsp-get params :outputName))
    (unless (derived-mode-p 'special-mode) (special-mode))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (lsp-get params :message))))
  nil)

(defun my-java-lsp--output-show (workspace name)
  (display-buffer (my-java-lsp--output-buffer workspace name))
  nil)

(defun my-java-lsp--output-close (workspace name)
  (kill-buffer (my-java-lsp--output-buffer workspace name))
  nil)

(defun my-java-lsp--output-reset (workspace name)
  (with-current-buffer (my-java-lsp--output-buffer workspace name)
    (let ((inhibit-read-only t)) (erase-buffer)))
  nil)

(defun my-java-lsp--library-directory (workspace)
  (expand-file-name "sources" (my-java-lsp--cache-directory (lsp--workspace-root workspace))))

(defun my-java-lsp--archive-source (uri)
  "Resolve a dependency URI to source, preserving its server identity."
  (let* ((workspace (or (lsp-find-workspace 'my-java-netbeans nil)
                        (user-error "No NetBeans workspace for %s" uri)))
         (directory (expand-file-name (secure-hash 'sha256 uri)
                                      (my-java-lsp--library-directory workspace)))
         (file (expand-file-name
                (concat (file-name-base (url-unhex-string uri)) ".java") directory)))
    (unless (file-readable-p file)
      (let ((content (with-lsp-workspace workspace
                       (lsp-request "workspace/executeCommand"
                                    (list :command "nbls.get.archive.file.content"
                                          :arguments (vector uri))))))
        (unless (stringp content)
          (user-error "NetBeans could not read source for %s" uri))
        (make-directory directory t)
        (with-temp-file file (insert content))
        (with-temp-file (concat file ".uri") (insert uri))))
    file))

(defun my-java-lsp--before-open (workspace)
  "Keep library navigation on its original URI, not the local cache path."
  (when (file-in-directory-p buffer-file-name (my-java-lsp--library-directory workspace))
    (let ((metadata (concat buffer-file-name ".uri")))
      (setq-local lsp-buffer-uri
                  (with-temp-buffer
                    (insert-file-contents metadata)
                    (buffer-string))))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'my-java-lsp--command #'my-java-lsp--installed-p)
  :major-modes '(java-mode java-ts-mode)
  :server-id 'my-java-netbeans
  :priority 1
  :multi-root nil
  :download-server-fn #'my-java-lsp--install
  :uri-handlers (ht ("jar" #'my-java-lsp--archive-source)
                    ("nbjrt" #'my-java-lsp--archive-source))
  :library-folders-fn
  (lambda (workspace)
    (list (my-java-lsp--library-directory workspace)
          ;; NetBeans returns file: URIs for decompiled classes in its index.
          (expand-file-name "cache" (my-java-lsp--cache-directory (lsp--workspace-root workspace)))))
  :before-file-open-fn #'my-java-lsp--before-open
  :initialization-options
  (lambda ()
    (list :nbcodeCapabilities
          (list :wantsJavaSupport t :wantsGroovySupport :json-false
                :statusBarMessageSupport :json-false :testResultsSupport :json-false
                :showHtmlPageSupport :json-false)))
  :request-handlers
  (ht ("workspace/configuration" #'my-java-lsp--workspace-configuration)
      ("window/showQuickPick" #'my-java-lsp--quick-pick)
      ("window/showInputBox" #'my-java-lsp--input-box)
      ("window/documentSave" #'my-java-lsp--save-documents)
      ("config/update" #'my-java-lsp--update-configuration)
      ("output/write" #'my-java-lsp--output-write)
      ("output/show" #'my-java-lsp--output-show)
      ("output/close" #'my-java-lsp--output-close)
      ("output/reset" #'my-java-lsp--output-reset))
  :async-request-handlers
  (ht ("window/showMultiStepInput" #'my-java-lsp--multi-step))))

;;;###autoload
(defun my-java-lsp-start ()
  "Register the Gradle root and start Java LSP, installing its server if absent."
  ;; Navigating into a generated source set must not register its emitted
  ;; settings.gradle as a new workspace inside the already-open real build.
  (unless (cl-some
           (lambda (workspace)
             (and (eq 'my-java-netbeans (lsp--client-server-id (lsp--workspace-client workspace)))
                  (file-in-directory-p default-directory (lsp--workspace-root workspace))))
           (lsp--session-workspaces (lsp-session)))
    (when-let* ((root (my-java-gradle-root))
                (canonical (lsp-f-canonical root)))
      (unless (member canonical (lsp-session-folders (lsp-session)))
        (lsp-workspace-folders-add canonical))))
  (if (my-java-lsp--installed-p)
      (lsp-deferred)
    (let ((client (gethash 'my-java-netbeans lsp-clients)))
      ;; lsp-mode resumes these buffers after installation and reports failures.
      ;; Share its in-flight state when several Java buffers open together.
      (cl-pushnew (current-buffer) (lsp--client-buffers client))
      (unless (lsp--client-download-in-progress? client)
        (lsp-install-server nil 'my-java-netbeans)))))

(provide 'my-java-lsp)
;;; my-java-lsp.el ends here
