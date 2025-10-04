COB=cobc
COBFLAGS=-Wall -x
MAIN_SRC=src/main.cob
HELPER_SRC=src/helper.cob
HTTP_SRC=src/http.cob
TARGET=build/app

$(TARGET): $(MAIN_SRC) $(HELPER_SRC) $(HTTP_SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TARGET) $(MAIN_SRC) $(HELPER_SRC) $(HTTP_SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf build