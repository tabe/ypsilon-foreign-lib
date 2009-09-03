YPSILON = ypsilon --sitelib=sitelib

.PHONY: check test

check: test

test:
	$(YPSILON) tests/ypsilon/gcrypt.scm
