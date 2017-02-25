EMACS=emacs

FILES=init.el \
      custom.el \
      modules/install-packages.el \
      modules/setup-editor.el \
      modules/setup-core.el \
      modules/init-lisp.el \
      modules/init-python.el \
      modules/init-latex.el \
      modules/init-org.el


all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'


clean: 
	rm *elc
	cd modules && rm *elc
