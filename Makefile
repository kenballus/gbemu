CPP := clang++
CPPFLAGS := -Wall -g -march=native -std=c++20
LINKERFLAGS := -lSDL2

OBJ_DIR := obj
_OBJ := GameBoy
OBJ := $(patsubst %, $(OBJ_DIR)/%, $(_OBJ).o)

SRC_DIR := src
MAIN := $(SRC_DIR)/main.cpp
BINARY := gbemu
TEST := test

all: $(BINARY) $(TEST).gb

# Compile the target
$(BINARY): $(OBJ) $(MAIN)
#	clang-format -i -style=file $(MAIN)
	$(CPP) $(LINKERFLAGS) $(CPPFLAGS) $^ -o $@

# Compile all the object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.hpp
	mkdir -p $(OBJ_DIR)
# 	clang-format -i -style=file $<
	$(CPP) $(CPPFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -f $(OBJ_DIR)/*.o $(BINARY)
	rm -f $(TEST)/$(TEST).o
	rm -f $(TEST).gb

$(TEST)/$(TEST).o: $(TEST)/$(TEST).asm
	rgbasm -L -o $(TEST)/$(TEST).o $(TEST)/$(TEST).asm

$(TEST).gb: $(TEST)/$(TEST).o
	rgblink -o $(TEST).gb $(TEST)/$(TEST).o
