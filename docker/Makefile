default: tars

usr-lib:
	@mkdir usr-lib
libicuuc.so.55: | usr-lib
	@cp -L /usr/lib/x86_64-linux-gnu/libicuuc.so.55 usr-lib
libicui18n.so.55: | usr-lib
	@cp -L /usr/lib/x86_64-linux-gnu/libicui18n.so.55 usr-lib
libicudata.so.55: | usr-lib
	@cp -L /usr/lib/x86_64-linux-gnu/libicudata.so.55 usr-lib

usr-lib-deps: | libicuuc.so.55 libicui18n.so.55 libicudata.so.55

tars: | usr-lib-deps
	@cd usr-lib && tar -cf ../usr-lib.tar .
	@rm -r usr-lib

.PHONY: default tars
