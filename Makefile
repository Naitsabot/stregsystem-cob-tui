COB=cobc
COBFLAGS=-Wall -x -I src
MAIN_SRC=src/main.cob
HELPER_SRC=src/helper.cob
HTTP_CLIENT_SRC=src/http-client-curl.cob
API_SRC=src/stregsystem-api.cob
JSON_ENCODER_SRC=src/json.cob
JSON_DECODER_SRC=src/json-decoder.cob
TEST_API_SRC=src/test-http-api.cob
TEST_JSON_SRC=src/test-json.cob
TARGET=build/app
TEST_API_TARGET=build/test-api
TEST_JSON_TARGET=build/test-json

# Logging level for tests: 0=none, 1=minimal, 2=verbose
TEST_LOG ?= 0

# Load environment variables from .env file if it exists, otherwise use .env.example
ifneq (,$(wildcard .env))
	include .env
	export
else ifneq (,$(wildcard .env.example))
	include .env.example
	export
endif

$(TARGET): $(MAIN_SRC) $(HELPER_SRC) $(HTTP_CLIENT_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TARGET) $(MAIN_SRC) $(HELPER_SRC) $(HTTP_CLIENT_SRC)

$(TEST_API_TARGET): $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_API_TARGET) $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC)

$(TEST_JSON_TARGET): $(TEST_JSON_SRC) $(JSON_ENCODER_SRC) $(JSON_DECODER_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_JSON_TARGET) $(TEST_JSON_SRC) $(JSON_ENCODER_SRC) $(JSON_DECODER_SRC)

run: $(TARGET)
	./$(TARGET)

test: test-api test-json

test-api: $(TEST_API_TARGET)
	COB_HTTP_CLIENT_LOG=$(TEST_LOG) ./$(TEST_API_TARGET)

test-json: $(TEST_JSON_TARGET)
	LOG_LEVEL=$(TEST_LOG) ./$(TEST_JSON_TARGET)

clean:
	rm -rf build
	rm -f temp-json-output.txt
