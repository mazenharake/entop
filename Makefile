all:
	@rebar3 compile

check:
	@rebar3 as test do dialyzer,eunit,cover

clean:
	@rebar3 clean

maintainer-clean: clean
	rm -rf _build
