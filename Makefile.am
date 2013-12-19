## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = ebin doc include

check_DATA = .common_test_ok .dialyzer_ok

.common_test_ok:
	cd test && $(ERLANG_ROOT_DIR)/bin/ct_run \
			-config sys.cfg -dir . -logdir log -pa ../ebin -erl_args +W w

.dialyzer_ok:
	dialyzer --src src

install-exec-hook:
	chown -R otpuser:otp $(appdir)

clean-local:
	-rm test/*.beam

