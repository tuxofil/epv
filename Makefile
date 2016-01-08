APP = epv

VERSION = $(shell cat version)

.PHONY: all compile doc clean test eunit dialyze all-tests \
	install install-doc install-html

all: compile html

COPTS = {outdir, ebin}, {i, \"include\"}, warn_unused_function, \
 warn_bif_clash, warn_deprecated_function, warn_obsolete_guard, verbose, \
 warn_shadow_vars, warn_export_vars, warn_unused_records, \
 warn_unused_import, warn_export_all, warnings_as_errors

ifdef DEBUG
COPTS := $(COPTS), debug_info
endif

ifdef TEST
COPTS := $(COPTS), {d, 'TEST'}
endif

compile:
	mkdir -p ebin
	sed "s/{{VERSION}}/$(VERSION)/" src/$(APP).app.in > ebin/$(APP).app
	echo '["src/*"].' > Emakefile
	erl -noinput -eval "up_to_date=make:all([$(COPTS)]),halt()"

$(APP): compile
	rm -f -- $(APP).zip
	zip -j $(APP) ebin/*
	zip $(APP) priv/epv.lang priv/*.png priv/www/*
	{ echo '#!/usr/bin/env escript'; \
	  echo '%%!-smp'; \
	  cat $(APP).zip; } > $(APP)
	chmod 755 $(APP)

html:
	sed "s/{{VERSION}}/$(VERSION)/" doc/overview.edoc.in > doc/overview.edoc
	erl -noinput -eval \
		'edoc:application($(APP),".",[{application,$(APP)}]),halt()'

test:
	$(MAKE) -C test

eunit:
	$(MAKE) TEST=y clean compile
	erl -noinput -pa ebin \
		-eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

PLT = .dialyzer_plt
DIALYZER_OPTS = -Wunmatched_returns -Werror_handling -Wrace_conditions

dialyze: $(PLT)
	dialyzer --plt $< -r . $(DIALYZER_OPTS) --src
	$(MAKE) DEBUG=y clean compile
	dialyzer --plt $< -r . $(DIALYZER_OPTS)

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts kernel stdlib crypto compiler

all-tests:
	$(MAKE) eunit
	$(MAKE) test
	$(MAKE) dialyze

clean:
	rm -rf -- ebin doc/*.html doc/*.css doc/*.png doc/edoc-info \
	    $(APP).zip $(APP) *.log \
	    erl_crash.dump Emakefile doc/overview.edoc
	find . -type f -name '*~' -delete
	$(MAKE) -C test clean

install: compile
	install -m 755 --directory $(DESTDIR)/$(APP)-$(VERSION)/ebin
	install -m 644 ebin/*.beam ebin/*.app $(DESTDIR)/$(APP)-$(VERSION)/ebin
	install -m 755 --directory $(DESTDIR)/$(APP)-$(VERSION)/priv
	install -m 644 priv/epv.lang $(DESTDIR)/$(APP)-$(VERSION)/priv
	install -m 755 --directory $(DESTDIR)/$(APP)-$(VERSION)/priv/www
	install -m 644 priv/www/*.png priv/www/*.css \
	    $(DESTDIR)/$(APP)-$(VERSION)/priv/www

install-doc:
	install -m 755 --directory $(DESTDIR)
	install -m 644 README.md LICENSE $(DESTDIR)/

install-html: html
	install -m 755 --directory $(DESTDIR)
	install -m 644 doc/*.html doc/*.css doc/*.png $(DESTDIR)/
