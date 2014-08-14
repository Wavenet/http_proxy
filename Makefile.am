## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = ebin doc include

check_DATA = .common_test_ok .dialyzer_ok

.common_test_ok:
	$(ERLANG_ROOT_DIR)/bin/ct_run \
			-dir test -config test/sys.cfg -logdir test/log -pa ebin -erl_args +W w

.dialyzer_ok:
	dialyzer --src src

install-exec-hook:
	chown -R otpuser:otp $(appdir)

clean-local:
	-rm test/*.beam
	-rm -r test/log/*

