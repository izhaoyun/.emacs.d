emacs ?= emacs

pull:
	echo "test. nothing at the moment"

run:
	$(emacs) -Q -l init.el
