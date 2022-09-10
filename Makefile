CXX ?= clang++
CXXFLAGS ?= -Wall -g -march=native -std=c++20
LDFLAGS ?= -lSDL2

OBJ_DIR ?= obj
_OBJ ?= GameBoy
OBJ ?= $(patsubst %, $(OBJ_DIR)/%, $(_OBJ).o)

SRC_DIR ?= src

MAIN ?= $(SRC_DIR)/main.cpp
BINARY ?= gbemu
TEST_DIR ?= test

all: $(BINARY) $(TEST_DIR)/test.gb

# Compile the target
$(BINARY): $(OBJ) $(MAIN)
	clang-format -i -style=file $(MAIN)
	$(CXX) $(LDFLAGS) $(CXXFLAGS) $^ -o $@

# Compile all the object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.hpp
	mkdir -p $(OBJ_DIR)
	clang-format -i -style=file $<
	$(CXX) $(CXXFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -f $(OBJ_DIR)/*.o $(BINARY)
	rm -f $(TEST_DIR)/test.gb

$(OBJ_DIR)/test.o: $(TEST_DIR)/test.asm
	rgbasm -L -o $(OBJ_DIR)/test.o $(TEST_DIR)/test.asm

$(TEST_DIR)/test.gb: $(OBJ_DIR)/test.o
	rgblink -o $(TEST_DIR)/test.gb $(OBJ_DIR)/test.o
