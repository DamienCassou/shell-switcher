(require 'f)

(defvar shell-switcher-support-path
  (f-dirname load-file-name))

(defvar shell-switcher-features-path
  (f-parent shell-switcher-support-path))

(defvar shell-switcher-root-path
  (f-parent shell-switcher-features-path))

(add-to-list 'load-path shell-switcher-root-path)

(require 'cl)
(require 'shell-switcher)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
