# package.el multi-file package install

# These are the variables that are specific to the package
NAME=shell-switcher
VERSION=0.1.1
DOC="Easily switch between shell buffers, like with alt+tab."
REQUIREMENTS=requirements.txt
package_parts = rswitcher.el shell-switcher.el README.md COPYING AUTHORS

# Everything beyond here should be generic
PACKAGE=$(NAME)-$(VERSION)
TARBALL=$(PACKAGE).tar

all: tarball

# Install the tarball in a test package store
test: tarball
	emacs -Q --batch -l ./packagedir.el -- $(TARBALL)

# Install the tarball in the user's emacs
install: tarball
	emacs --batch -l ~/.emacs.d/init.el -l ./build.el -- $(TARBALL)

clean:
	rm -rf .elpa
	rm -rf $(TARBALL)
	rm -rf $(PACKAGE)
	rm -rf $(NAME)-pkg.el

tarball: $(TARBALL)

$(TARBALL): $(PACKAGE) $(PACKAGE)/$(NAME)-pkg.el
	tar cf $@ $<

$(PACKAGE): $(package_parts)
	mkdir $@
	cp $(package_parts) $@

$(PACKAGE)/$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DOC)\" `cat $(REQUIREMENTS)`)" > $@

run-tests:
	emacs -batch -l ert -l rswitcher.el -l rswitcher-test.el -f ert-run-tests-batch-and-exit


# End
