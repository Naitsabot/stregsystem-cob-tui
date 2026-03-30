COB=cobc
COBFLAGS=-Wall -x -I src
MAIN_SRC=src/main.cob
TUI_SRC=src/tui.cob
HTTP_CLIENT_SRC=src/http-client-curl.cob
API_SRC=src/stregsystem-api.cob
JSON_ENCODER_SRC=src/json-encoder.cob
JSON_DECODER_SRC=src/json-decoder.cob
TEST_API_SRC=src/test-http-api.cob
TEST_JSON_SRC=src/test-json.cob
TEST_PRODUCT_DICT_SRC=src/test-product-dict.cob
TARGET=build/app
TEST_API_TARGET=build/test-api
TEST_JSON_TARGET=build/test-json
TEST_PRODUCT_DICT_TARGET=build/test-product-dict
SCREENIO_CPY=src/copybooks/screenio.cpy

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

$(TARGET): $(MAIN_SRC) $(TUI_SRC) $(HTTP_CLIENT_SRC) $(API_SRC) $(JSON_DECODER_SRC) $(SCREENIO_CPY)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TARGET) $(MAIN_SRC) $(TUI_SRC) $(API_SRC) $(HTTP_CLIENT_SRC) $(JSON_DECODER_SRC)

$(TEST_API_TARGET): $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC) $(JSON_DECODER_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_API_TARGET) $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC) $(JSON_DECODER_SRC)

$(TEST_JSON_TARGET): $(TEST_JSON_SRC) $(JSON_ENCODER_SRC) $(JSON_DECODER_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_JSON_TARGET) $(TEST_JSON_SRC) $(JSON_ENCODER_SRC) $(JSON_DECODER_SRC)

$(TEST_PRODUCT_DICT_TARGET): $(TEST_PRODUCT_DICT_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_PRODUCT_DICT_TARGET) $(TEST_PRODUCT_DICT_SRC)

$(SCREENIO_CPY):
	wget -O $(SCREENIO_CPY) https://raw.githubusercontent.com/JohnDovey/GNUCobol-Samples/refs/heads/main/screenio.cpy

run: $(TARGET)
	./$(TARGET)

test: test-api test-json test-product-dict

test-api: $(TEST_API_TARGET)
	COB_HTTP_CLIENT_LOG=$(TEST_LOG) ./$(TEST_API_TARGET)

test-json: $(TEST_JSON_TARGET)
	LOG_LEVEL=$(TEST_LOG) ./$(TEST_JSON_TARGET)

test-product-dict: $(TEST_PRODUCT_DICT_TARGET)
	./$(TEST_PRODUCT_DICT_TARGET)

clean:
	rm -rf build
	rm -f temp-json-output.txt
