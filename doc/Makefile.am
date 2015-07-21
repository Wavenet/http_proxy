## Makefile.am
##
## Process this file with automake to produce Makefile.in

modhtml = http_proxy.html \
		http_proxy_app.html \
		http_proxy_sup.html \
		http_proxy_ua_sup.html \
		http_proxy_ua_port_sup.html \
		http_proxy_ua_pool_sup.html \
		http_proxy_ua_accept_sup.html \
		http_proxy_ua_connect_sup.html \
		http_proxy_ua_listen_server.html \
		http_proxy_ua_accept_server.html \
		http_proxy_ua_connect_fsm.html \
		http_proxy_origin_pool_sup.html \
		http_proxy_origin_server_sup.html \
		http_proxy_origin_connect_sup.html \
		http_proxy_origin_server.html \
		http_proxy_origin_connect_fsm.html \
		http_proxy_util.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html packages-frame.html \
		index.html stylesheet.css wavenetlogo.png

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_srcdir)'" \
			'[{sort_functions,false},{private,true},{todo,true}]'
	for i in `ls *.html`; \
			do sed -e 's!erlang logo!wavenet logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.wavenet.lk/\"!' \
					-e 's!erlang.png!wavenetlogo.png!' < $$i > t.html; \
							mv t.html $$i; done

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

#INSTALL += -o otpuser -g otp

