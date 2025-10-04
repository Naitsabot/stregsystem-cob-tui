COB=cobc
COBFLAGS=-Wall -x

SRC=$(wildcard src/*.cob)
TARGET=build/app

$(TARGET): $(SRC)
	mkdir -p build
	$(COB) $(COBFLAGS) -o $(TARGET) $(SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf build