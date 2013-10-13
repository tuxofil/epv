APP = epv

VERSION = $(shell cat version)

.PHONY: all compile doc clean test eunit dialyze all-tests \
	install install-doc install-html \
	debian-install debian-uninstall

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

ifdef TRACE
COPTS := $(COPTS), {d, 'TRACE'}
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
	erl -noinput -eval "{ok,ZipData}=file:read_file(\"$(APP).zip\"), \
	   ok=escript:create(\"$(APP)\",[shebang,\
	   {emu_args,\"-smp\"},{archive,ZipData}]),halt()"
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

dialyze: $(PLT)
	dialyzer --src --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions
	$(MAKE) DEBUG=y clean compile
	dialyzer --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts kernel stdlib crypto compiler

all-tests:
	$(MAKE) eunit
	$(MAKE) test
	$(MAKE) dialyze

clean:
	rm -rf -- ebin doc/*.html doc/*.css doc/*.png doc/edoc-info \
	    $(APP).zip $(APP) \
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

debian-install:
	getent passwd $(APP) || \
	    adduser --system --shell /bin/sh --home /var/lib/$(APP) $(APP)
	$(MAKE) DESTDIR=/usr/lib/erlang/lib install
	$(MAKE) DESTDIR=/usr/share/doc/erlang-$(APP) install-doc
	$(MAKE) DESTDIR=/usr/share/doc/erlang-$(APP)/html install-html
	cat etc/app.config-template | \
	    sed 's#@@BIND_IP@@#any#' | \
	    sed 's#@@TCP_PORT@@#8080#' | \
	    sed 's#@@MEDIA_DIR@@#/var/lib/$(APP)/media#' | \
	    sed 's#@@META_DIR@@#/var/lib/$(APP)/meta#' | \
	    sed 's#@@SASL_LOG@@#/var/log/$(APP)/sasl.log#' > /etc/$(APP).config
	chmod 644 /etc/$(APP).config
	install -m 755 debian-init.d.sh /etc/init.d/$(APP)
	install -m 755 --directory -o $(APP) -g $(APP) /var/log/$(APP)

debian-uninstall:
	rm -f /etc/$(APP).config
	rm -f /etc/init.d/$(APP)
	rm -rf /usr/lib/erlang/lib/$(APP)-*
	rm -rf /usr/share/doc/erlang-$(APP)

