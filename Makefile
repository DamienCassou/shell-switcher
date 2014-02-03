CWD          = $(shell pwd)
SCRIPT       = $(CWD)/script
GIT_DIR      = $(CWD)/.git
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
CASK         = cask
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_INIT_EL = $(USER_EMACS_D)/init.el
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out flycheck_%, $(filter-out %-pkg.el, $(wildcard *.el)))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
OBJECTS      = $(SRCS:.el=.elc)
PACKAGE_SRCS = $(SRCS) shell-switcher-pkg.el
PACKAGE_TAR  = shell-switcher-$(VERSION).tar

PRECOMMIT_SRC  = $(SCRIPT)/pre-commit.sh
PRECOMMIT_HOOK = $(GIT_DIR)/hooks/pre-commit

.PHONY: all
all : env compile dist

# Configure tooling and environment.
.PHONY: env
env : packages $(PRECOMMIT_HOOK)

# Run tests before committing.
$(PRECOMMIT_HOOK) :
	ln -s $(PRECOMMIT_SRC) $(PRECOMMIT_HOOK)
	chmod +x $(PRECOMMIT_HOOK)

# Byte-compile elisp files.
.PHONY: compile
compile : $(OBJECTS)

# Run ert tests.
.PHONY: unit-tests ecukes-tests check
unit-tests : compile
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

ecukes-tests : compile
	$(CASK) exec ecukes --script

check : unit-tests ecukes-tests

# Install packages with Cask.
$(PKG_DIR) : Cask
	$(CASK)
	$(CASK) install
	touch $(PKG_DIR)

# Create a tar that can be installed by package.el
.PHONY: dist
dist : $(PACKAGE_TAR)
$(PACKAGE_TAR) : $(PACKAGE_SRCS)
	rm -rf shell-switcher-$(VERSION)
	mkdir -p shell-switcher-$(VERSION)
	cp -f $(PACKAGE_SRCS) shell-switcher-$(VERSION)
	tar cf $(PACKAGE_TAR) shell-switcher-$(VERSION)
	rm -rf shell-switcher-$(VERSION)

# Install elisp packages with cask.
.PHONY: packages
packages : $(PKG_DIR)

# Install the package to the user's Emacs dir.
.PHONY: install
install : dist
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(CWD)/$(PACKAGE_TAR)")'

# Uninstall the package.
.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/shell-switcher-*

# Restore to pristine state.
.PHONY: clean-all
clean-all : clean clean-pkgdir

# Clean generated files.
.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf shell-switcher-*.tar shell-switcher-pkg.el

# Remove packages installed by Cask.
.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKG_DIR)

# Generate files.

shell-switcher-pkg.el : Cask
	$(CASK) package

%.elc : %.el $(PKG_DIR)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize -L . \
	-f batch-byte-compile $<
