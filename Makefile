COB=cobc
COBFLAGS=-Wall -x
MAIN_SRC=src/main.cob
HELPER_SRC=src/helper.cob
HTTP_CLIENT_SRC=src/http-client-netcat.cob
API_SRC=src/stregsystem-api.cob
TEST_API_SRC=src/test-http-api.cob
TARGET=build/app
TEST_TARGET=build/test-api

# Logging level for tests: 0=none, 1=minimal, 2=verbose
TEST_LOG ?= 0

$(TARGET): $(MAIN_SRC) $(HELPER_SRC) $(HTTP_CLIENT_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TARGET) $(MAIN_SRC) $(HELPER_SRC) $(HTTP_CLIENT_SRC)

$(TEST_TARGET): $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TEST_TARGET) $(TEST_API_SRC) $(API_SRC) $(HTTP_CLIENT_SRC)

run: $(TARGET)
	./$(TARGET)

test-api: $(TEST_TARGET)
	COB_HTTP_CLIENT_LOG=$(TEST_LOG) ./$(TEST_TARGET)

clean:
	rm -rf build
