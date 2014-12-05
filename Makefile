.PHONY: rel test

all:clean get-deps compile

compile:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean
# 	rm -f ebin/*


dev1 dev2 dev3: all
	rm -rf dev_rel/$@
	mkdir -p dev_rel/$@
	rm -f priv/configs/env.config
	ln -s dev.config priv/configs/env.config
	rm -f rel/reltool.config
	cp rel/reltool.config.dev rel/reltool.config
	rebar generate target_dir=../dev_rel/$@ overlay_vars=vars/$@_vars.config

dev: dev1 dev2 dev3

# rel: all
# 	rm -rf rel/smsfc
# 	rm -f priv/configs/env.config
# 	ln -s prod.config priv/configs/env.config
# 	rm -f rel/reltool.config
# 	ln -s reltool.config.prod rel/reltool.config
# 	./rebar generate overlay_vars=vars/prod_vars.config

test:
	rm -rf .eunit
	./rebar compile eunit
