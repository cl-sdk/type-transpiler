ENV?=development

## run through roswell
LISPC?=sbcl

LISPFLAGS=--quit --non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISPC) \
	$(LISPFLAGS) --load tests-runner.lisp
