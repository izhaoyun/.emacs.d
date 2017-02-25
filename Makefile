EMACS=emacs

FILES=init.el \
      modules/install-packages.el \
      modules/setup-editor.el \
      modules/setup-core.el \
      modules/init-lisp.el


all: $(FILES)
	$(EMACS) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILES))))'


clean: 
	rm *elc
	cd modules && rm *elc
