APP = epv

VERSION = $(shell awk '{gsub("[()]","",$$2);print$$2;exit}' debian/changelog)

.PHONY: all compile doc clean eunit dialyze all-tests

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

eunit:
	$(MAKE) TEST=y clean compile
	erl -noinput -pa ebin \
		-eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

PLT = .dialyzer_plt
DIALYZER_OPTS = -Wunmatched_returns -Werror_handling -Wrace_conditions

dialyze: $(PLT)
	$(MAKE) DEBUG=y clean compile
	dialyzer --plt $< -r src $(DIALYZER_OPTS) --src
	dialyzer --plt $< -r . $(DIALYZER_OPTS)

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts kernel stdlib crypto compiler

all-tests:
	$(MAKE) eunit
	$(MAKE) dialyze

clean:
	rm -rf -- ebin doc/*.html doc/*.css doc/*.png doc/edoc-info \
	    $(APP).zip $(APP) *.log \
	    erl_crash.dump Emakefile doc/overview.edoc
	find . -type f -name '*~' -delete
