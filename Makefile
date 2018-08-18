EMACS=emacs

FILES=init.el \
	custom.el \
	lisp/setup-packages.el \
	lisp/setup-editor.el \
	lisp/init-prog.el \
	lisp/init-cc.el \
	lisp/init-org.el \
	lisp/init-go.el \
	lisp/init-python.el \
	lisp/init-web.el


all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'


clean:
	find . -maxdepth 2 -name "*elc" -exec rm {} \;
