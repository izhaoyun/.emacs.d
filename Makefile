EMACS := emacs

FILES := init.el \
	custom.el \
	lisp/init-editor.el \
	lisp/init-develop.el \
	lisp/init-writing.el

all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'

clean:
	find . -maxdepth 2 -name "*elc" -exec rm {} \;
