APP=epv

VERSION=`cat version`

.PHONY: all compile doc Emakefile clean \
	test eunit dialyze all-tests \
	install-debian uninstall-debian

all: compile doc

compile: ebin/$(APP).app Emakefile
	erl -noinput -eval 'up_to_date = make:all()' -s erlang halt

doc: doc/overview.edoc
	erl -noinput -eval \
		'edoc:application($(APP), ".", [{application, $(APP)}])' \
		-s erlang halt

test:
	$(MAKE) -C test

eunit:
	erl -noinput -pa ebin \
		-eval 'ok = eunit:test({application, $(APP)}, [verbose])' \
		-s erlang halt

PLT=.dialyzer_plt

dialyze: $(PLT)
	dialyzer --plt $(PLT) -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions
	dialyzer --src --plt $(PLT) -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions

$(PLT):
	dialyzer --build_plt --output_plt $(PLT) \
		--apps erts kernel stdlib crypto compiler

all-tests:
	$(MAKE) clean compile test
	$(MAKE) clean compile eunit TEST=1
	$(MAKE) clean compile dialyze DEBUG=1

clean:
	rm --force -- \
		./doc/*.html ./doc/*.css ./doc/*.png ./doc/edoc-info \
		./ebin/*.beam ./erl_crash.dump Emakefile \
		./doc/overview.edoc ./ebin/$(APP).app
	rm --recursive --force -- ./ebin
	find ./ -type f -name '*~' -print -delete
	$(MAKE) -C test clean

ebin/$(APP).app: version src/$(APP).app.src
	mkdir --parents ebin
	sed "s/{{VERSION}}/$(VERSION)/" \
		src/$(APP).app.src > ebin/$(APP).app

doc/overview.edoc: version doc/overview.edoc.src
	sed "s/{{VERSION}}/$(VERSION)/" \
		doc/overview.edoc.src > doc/overview.edoc

ifdef DEBUG
EXTRA_OPTS:=debug_info,
else
EXTRA_OPTS:=
endif

ifdef TEST
EXTRA_OPTS:=$(EXTRA_OPTS) {d,'TEST', true},
endif

Emakefile:
	sed "s/{{EXTRA_OPTS}}/$(EXTRA_OPTS)/" Emakefile.src > Emakefile

install-debian: compile doc
	getent passwd $(APP) || \
		adduser --system --shell /bin/sh --home /var/lib/$(APP) $(APP)
	install --mode=755 --directory \
		/usr/lib/erlang/lib/$(APP)-$(VERSION)/ebin \
		/usr/lib/erlang/lib/$(APP)-$(VERSION)/priv/www \
		/usr/share/doc/erlang-$(APP)/html
	install --mode=644 etc/app.config-template /etc/$(APP).config
	sed --in-place 's#@@BIND_IP@@#any#' /etc/$(APP).config
	sed --in-place 's#@@TCP_PORT@@#8080#' /etc/$(APP).config
	sed --in-place 's#@@MEDIA_DIR@@#/var/lib/$(APP)/media#' /etc/$(APP).config
	sed --in-place 's#@@META_DIR@@#/var/lib/$(APP)/meta#' /etc/$(APP).config
	sed --in-place 's#@@SASL_LOG@@#/var/log/$(APP)/sasl.log#' /etc/$(APP).config
	install --mode=755 debian-init.d.sh /etc/init.d/$(APP)
	install --mode=644 ebin/*.beam ebin/*.app \
		/usr/lib/erlang/lib/$(APP)-$(VERSION)/ebin
	install --mode=644 priv/epv.lang \
		/usr/lib/erlang/lib/$(APP)-$(VERSION)/priv
	install --mode=644 priv/www/*.png priv/www/*.css  \
		/usr/lib/erlang/lib/$(APP)-$(VERSION)/priv/www
	install --mode=644 README.md LICENSE \
		/usr/share/doc/erlang-$(APP)
	install --mode=644 doc/*.html doc/*.png doc/*.css \
		/usr/share/doc/erlang-$(APP)/html

uninstall-debian:
	rm --force /etc/$(APP).config
	rm --force /etc/init.d/$(APP)
	rm --force --recursive /usr/lib/erlang/lib/$(APP)-*
	rm --force --recursive /usr/share/doc/erlang-$(APP)

