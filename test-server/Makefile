.PHONY: run test clean

run:
	go run main.go

test:
	hurl --test tests.hurl

test-verbose:
	hurl --test --verbose tests.hurl

clean:
	pkill -f "go run main.go" || true

help:
	@echo "Available commands:"
	@echo "  run          - Start the test server"
	@echo "  test         - Run hurl tests"
	@echo "  test-verbose - Run hurl tests with verbose output"
	@echo "  clean        - Stop the server"
