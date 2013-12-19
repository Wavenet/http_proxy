## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = http_proxy.app
EXTRA_DIST = http_proxy.app

beam_DATA = http_proxy.beam \
		http_proxy_app.beam \
		http_proxy_sup.beam \
		http_proxy_ua_sup.beam \
		http_proxy_ua_port_sup.beam \
		http_proxy_ua_pool_sup.beam \
		http_proxy_ua_accept_sup.beam \
		http_proxy_ua_connect_sup.beam \
		http_proxy_ua_listen_server.beam \
		http_proxy_ua_accept_server.beam \
		http_proxy_ua_connect_fsm.beam \
		http_proxy_origin_pool_sup.beam \
		http_proxy_origin_server_sup.beam \
		http_proxy_origin_connect_sup.beam \
		http_proxy_origin_server.beam \
		http_proxy_origin_connect_fsm.beam \
		http_proxy_util.beam

%.beam:$(top_srcdir)/src/%.erl $(top_srcdir)/include/http_proxy.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/include -I $(top_builddir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

