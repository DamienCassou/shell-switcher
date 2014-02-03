;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(And "^I delete other windows$"
  (lambda ()
    (delete-other-windows)))

(And "^I setup y-or-n-p to always answer \\(t\\|nil\\)$"
     (lambda (answer)
       (lexical-let ((answer answer))
         (fset 'y-or-n-p (lambda (prompt)
                           (string= answer "t"))))))

(And "^I setup a mock shell function$"
  (lambda ()
    (setq shell-switcher-new-shell-function
          (lambda ()
            (generate-new-buffer "ecukes-shell")))))

(Given "^I setup shell-switcher-mode$"
  (lambda ()
    (shell-switcher-mode)))

(And "^I kill all shell-switcher buffers$"
     (lambda ()
       (shell-switcher-kill-all-shells)))
