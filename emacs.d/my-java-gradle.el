;;; my-java-gradle.el --- Preserve Gradle source ownership -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst my-java-gradle--init-script
  (expand-file-name "java-source-roots.gradle"
                    (file-name-directory (file-truename load-file-name))))

(defun my-java-gradle-root ()
  "Find the enclosing Gradle build, including builds outside version control."
  (or (locate-dominating-file
       default-directory
       (lambda (dir)
         (or (file-exists-p (expand-file-name "settings.gradle" dir))
             (file-exists-p (expand-file-name "settings.gradle.kts" dir)))))
      (locate-dominating-file default-directory "gradlew")
      (locate-dominating-file
       default-directory
       (lambda (dir)
         (or (file-exists-p (expand-file-name "build.gradle" dir))
             (file-exists-p (expand-file-name "build.gradle.kts" dir)))))))

(defun my-java-gradle--model (root)
  "Ask ROOT's Gradle for declared source directories, without compiling.
Like ordinary IDE import, this executes the project's build scripts.  Use
its wrapper and environment, not the language server's separate runtime JDK."
  (let* ((default-directory root)
         (wrapper (expand-file-name "gradlew" root))
         (command (cond ((file-executable-p wrapper) (list wrapper))
                        ((file-exists-p wrapper) (list "sh" wrapper))
                        ((executable-find "gradle") (list "gradle"))
                        (t (user-error "Gradle import needs gradlew or Gradle on PATH"))))
         (buffer (get-buffer-create "*Java Gradle import*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (message "Java: reading Gradle source sets in %s" root)
    (unless (zerop (apply #'process-file (car command) nil buffer nil
                         (append (cdr command)
                                 (list "--quiet" "--console=plain"
                                       "-Dorg.gradle.configuration-cache=false"
                                       "--init-script" my-java-gradle--init-script
                                       ":help"))))
      (display-buffer buffer)
      (user-error "Gradle source-set import failed; see *Java Gradle import*"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (let (projects found)
        (while (re-search-forward "^EMACS_JAVA_SOURCE_ROOTS=\\(.*\\)$" nil t)
          (setq found t
                projects (append projects
                                 (json-parse-string (match-string 1)
                                                    :array-type 'list))))
        (unless found
          (display-buffer buffer)
          (user-error "Gradle did not return source-set metadata"))
        projects))))

(defun my-java-gradle--nested-projects (projects)
  "Find descriptor directories shadowing PROJECTS' declared source roots.
NetBeans normally gives a file to its closest enclosing project.  Generators
can emit a whole project inside a real source set, so that rule loses the
real compiler classpath.  Skip recognition only at those intermediate
directories, never at actual Gradle project roots or at the Java files."
  (let ((project-dirs (mapcar (lambda (project) (gethash "directory" project)) projects))
        forbidden)
    (dolist (project projects)
      (let ((owner (gethash "directory" project)))
        (dolist (source (gethash "sources" project))
          (when (and (file-directory-p source) (file-in-directory-p source owner))
            (let ((dir (directory-file-name source)))
              (while (not (equal dir owner))
                (when (and (not (member dir project-dirs))
                           (cl-some (lambda (marker)
                                      (file-exists-p (expand-file-name marker dir)))
                                    '("settings.gradle" "settings.gradle.kts"
                                      "build.gradle" "build.gradle.kts" "pom.xml"
                                      "nbproject/project.xml")))
                  (cl-pushnew dir forbidden :test #'equal))
                (setq dir (directory-file-name (file-name-directory dir)))))))))
    (sort forbidden #'string<)))

(defun my-java-gradle-launch-arguments (root)
  "Return NetBeans ownership arguments for the Gradle build at ROOT.
No project source or build scripts are rewritten.  The property skips project
recognition, not source indexing.  NetBeans reads it only at startup: restart
LSP after running a generator that creates new nested project descriptors."
  (let ((default-directory root))
    (when (my-java-gradle-root)
      (let ((directories (my-java-gradle--nested-projects
                          (my-java-gradle--model root))))
        (when directories
          (when (cl-some (lambda (dir) (string-match-p ";" dir)) directories)
            (user-error "NetBeans project.forbiddenFolders cannot encode semicolons in paths"))
          (list (concat "-J-Dproject.forbiddenFolders="
                        (string-join directories ";"))))))))

(provide 'my-java-gradle)
;;; my-java-gradle.el ends here
