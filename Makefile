EMACS=emacs

FILES=init.el \
      modules/install-packages.el \
      modules/setup-editor.el \
      modules/setup-core.el \
      modules/init-c++.el \
      modules/init-lisp.el \
      modules/init-org.el \
      modules/init-latex.el \
      modules/init-python.el \
      modules/init-web.el


all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'


clean:
	rm *elc
	cd modules && rm *elc
