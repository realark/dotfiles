;;; my-java-gradle-tests.el --- Source ownership regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-java-gradle)

(ert-deftest my-java-gradle-generated-descriptors-do-not-hide-real-projects ()
  (let* ((root (directory-file-name (file-truename (make-temp-file "java source roots " t))))
         (generated (expand-file-name "build/generated/client" root))
         (library (expand-file-name "lib" root))
         (normal-src (expand-file-name "src/main/java" root))
         (generated-src (expand-file-name "src/main/java" generated))
         (library-src (expand-file-name "src/main/java" library)))
    (unwind-protect
        (progn
          (dolist (dir (list normal-src generated-src library-src))
            (make-directory dir t))
          (dolist (dir (list root generated library))
            (with-temp-file (expand-file-name "settings.gradle.kts" dir)
              (insert "rootProject.name = \"fixture\"\n")))
          (let ((projects
                 (list (let ((model (make-hash-table :test 'equal)))
                         (puthash "directory" root model)
                         ;; Shared sources and duplicates must not accidentally
                         ;; hide a real modeled subproject or duplicate flags.
                         (puthash "sources" (list normal-src generated-src library-src generated-src) model)
                         model)
                       (let ((model (make-hash-table :test 'equal)))
                         (puthash "directory" library model)
                         (puthash "sources" (list library-src) model)
                         model))))
            (should (equal (my-java-gradle--nested-projects projects) (list generated)))
            ;; Once the nested descriptor belongs to a real Gradle project,
            ;; its ownership must win instead of being hidden as generated.
            (let ((model (make-hash-table :test 'equal)))
              (puthash "directory" generated model)
              (puthash "sources" (list generated-src) model)
              (should-not (my-java-gradle--nested-projects (cons model projects))))))
      (delete-directory root t))))

(provide 'my-java-gradle-tests)
;;; my-java-gradle-tests.el ends here
