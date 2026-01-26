.PHONY: help
help:
	@echo "Available targets:"
	@echo "  make config    - Tangle config.org into init.el and early-init.el"
	@echo "  make test      - Test that config loads without errors"
	@echo "  make check     - Run config and test targets"

.PHONY: config
config:
	emacs --batch -l org -l ob-emacs-lisp --eval "(progn (advice-add 'hack-local-variables :override #'ignore) (org-babel-tangle-file \"config.org\"))"

.PHONY: test
test:
	emacs --batch -l init.el -eval '(message "Config loaded successfully")'

.PHONY: check
check: config test
	@echo "All checks passed!"
