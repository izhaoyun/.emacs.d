EMACS=emacs

FILES=init.el \
	custom.el \
	modules/install-packages.el \
	modules/setup-editor.el \
	modules/setup-core.el \
	modules/init-c++.el \
	modules/init-lisp.el \
	modules/init-org.el \
	modules/init-latex.el \
	modules/init-python.el \
	modules/init-web.el \
	modules/init-go.el


all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'


clean:
	find . -maxdepth 2 -name "*elc" -exec rm {} \;
