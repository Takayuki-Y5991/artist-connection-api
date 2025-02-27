.PHONY: test lint format check all clean

test:
	clojure -M:test:test/run --reporter documentation --no-randomize --color

lint:
	clojure -M:clj-kondo --lint src:test
	clojure -M:eastwood || true
	clojure -M:kibit || true

format-check:
	clojure -M:cljfmt check

format:
	clojure -M:cljfmt fix

check: lint format-check test

all: lint format test

clean:
	rm -rf target