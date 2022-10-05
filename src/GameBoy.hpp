#pragma once

#include <cstdint>
#include <fstream>
#include <utility>

#define DEBUG_LEVEL 1
// #define DEBUG(lvl, stuff)
#define DEBUG(lvl, stuff) do { if (lvl <= DEBUG_LEVEL) std::cerr << stuff << std::endl; } while (0)
#define DEBUGPPU(x)
// #define DEBUGPPU(x) do { std::cerr << "[PPU] " << x; } while (0)

int64_t const GB_SCREEN_WIDTH = 160;
int64_t const GB_SCREEN_HEIGHT = 144;
uint64_t const NUM_REGISTERS = 8;
uint32_t const TOTAL_ADDRESSABLE_BYTES = 0x10000;

uint16_t const IVT_OFFSET = 0x0000;
uint16_t const HEADER_OFFSET = 0x0100;
uint16_t const ROM_BANK_0 = 0x0150;
uint16_t const ROM_BANK_1 = 0x4000;
// This is where tiles live:
uint16_t const UNSIGNED_TILE_DATA_BASE = 0x8000;
uint16_t const SIGNED_TILE_DATA_BASE = 0x9000;
// This is where tile maps live:
uint16_t const TILE_MAP_1 = 0x9800;
uint16_t const TILE_MAP_2 = 0x9C00;

uint16_t const CARTRIDGE_RAM = 0xA000;
uint16_t const WRAM = 0xC000;
uint16_t const ECHO_RAM = 0xE000;
uint16_t const OAM = 0xFE00;
uint16_t const UNUSED_ADDRESSES = 0xFEA0;
uint16_t const IO_REGS = 0xFF00;
uint16_t const FAST_RAM = 0xFF80;

uint16_t const VBLANK_INTERRUPT_ADDRESS = 0x0040;
uint16_t const LCD_STAT_INTERRUPT_ADDRESS = 0x0048;
uint16_t const TIMER_INTERRUPT_ADDRESS = 0x0050;
uint16_t const SERIAL_INTERRUPT_ADDRESS = 0x0058;
uint16_t const JOYPAD_INTERRUPT_ADDRESS = 0x0060;

uint16_t const LCD_CONTROL = 0xFF40;
uint16_t const LCD_STATUS = 0xFF41;
uint16_t const SCY = 0xFF42;
uint16_t const SCX = 0xFF43;
uint16_t const LY = 0xFF44;
uint16_t const LYC = 0xFF45;
uint16_t const OAM_DMA_START = 0xFF46;
uint16_t const BGP = 0xFF47;
uint16_t const OBP0 = 0xFF48;
uint16_t const OBP1 = 0xFF49;
uint16_t const WY = 0xFF4A;
uint16_t const WX = 0xFF4B;

uint16_t const JOYPAD_PORT = 0xFF00;
uint16_t const SERIAL_DATA = 0xFF01;
uint16_t const SERIAL_CONTROL = 0xFF02;
uint16_t const DIVIDER_REGISTER = 0xFF04;
uint16_t const TIMA = 0xFF05;
uint16_t const TMA = 0xFF06;
uint16_t const TAC = 0xFF07;
uint16_t const INTERRUPT_FLAGS = 0xFF0F;
uint16_t const INTERRUPT_ENABLE = 0xFFFF;

uint8_t const FIRST_VBLANK_SCANLINE = 144;
uint64_t const TILE_MAP_WIDTH = 32; // Tiles
uint64_t const TILE_MAP_HEIGHT = 32; // Tiles
uint64_t const TILE_WIDTH = 8; // Pixels
uint64_t const TILE_HEIGHT = 8; // Pixels
uint64_t const BYTES_PER_TILE = 0x10;
uint64_t const TILE_MAP_SIZE = 0x400; // Bytes
uint64_t const DIVIDER_REGISTER_RATE = 16384;
std::uint64_t const CLOCK_SPEED = 1048576; // M-cycles
// So we do 64 M-cycles for each increment of the divider:
uint64_t const CLOCKS_PER_DIVIDER_INCREMENT = 64;
uint64_t const NUM_SPRITES = 40;

// 16 ms/frame gets us a little over 60 fps
uint64_t const MS_PER_CYCLE = 100;

enum JoypadButton {
    GB_KEY_A = 0,
    GB_KEY_B = 1,
    GB_KEY_START = 2,
    GB_KEY_SELECT = 3,
    GB_KEY_UP = 4,
    GB_KEY_DOWN = 5,
    GB_KEY_LEFT = 6,
    GB_KEY_RIGHT = 7,
};

enum Register8 {
    REG_B = 0b000,
    REG_C = 0b001,
    REG_D = 0b010,
    REG_E = 0b011,
    REG_H = 0b100,
    REG_L = 0b101,
    DUMMY = 0b110,
    REG_F = 0b110,  // Not actually true, but useful internally to have an enum for REG_F
    REG_A = 0b111,
    REG_S = 0b1000, // These don't exist but will be useful for us
    REG_P = 0b1001, // These don't exist but will be useful for us
};

enum Flag {
    FL_Z = 1 << 7,
    FL_N = 1 << 6,
    FL_H = 1 << 5,
    FL_C = 1 << 4,
};

enum JoypadMode {
    DIRECTIONS = 0b01,
    ACTIONS = 0b10,
    BOTH = 0b11,
};

enum GraphicsMode {
    HBLANK = 0,
    VBLANK = 1,
    SEARCHING = 2,
    TRANSFERRING = 3,
};

class GameBoy {
public: // change to private when done debugging
    uint16_t pc = HEADER_OFFSET;
    bool ime = 1;
    JoypadMode joypad_mode = BOTH; // idk if this is true
    uint8_t ram[TOTAL_ADDRESSABLE_BYTES];
    uint8_t registers[NUM_REGISTERS + 2] = {0}; // The +2 is for SP. We set these in the constructor for readability

    uint64_t cycles_to_wait = 0; // M-cycles
    uint64_t cycle_count = 0;

    uint32_t dot_count = 0;
    GraphicsMode graphics_mode = SEARCHING;

    bool buttons_pressed[8] = {true};

    bool need_to_do_interrupts = true;
    bool halted = false;

    uint8_t screen[TILE_MAP_HEIGHT * TILE_HEIGHT][TILE_MAP_WIDTH * TILE_WIDTH]; // We render everything to here.

    GameBoy(void);
    void load_rom(std::string const &romfile);
    void wait(void);
    void handle_interrupts(void);

    uint8_t read_joypad(void) const;
    uint8_t read_mem8(uint16_t) const;
    uint16_t read_mem16(uint16_t) const;
    void write_mem8(uint16_t, uint8_t);
    void write_mem16(uint16_t, uint16_t);
    void set_register8(Register8 const, uint8_t const);
    uint8_t get_register8(Register8) const;
    void set_doublereg(Register8, Register8, uint16_t);
    uint16_t get_doublereg(Register8, Register8) const;
    void set_flag(Flag, bool);
    bool get_flag(Flag) const;
    void do_dma(uint8_t const);
    void enter_vblank(void);
    void enter_hblank(void);
    void enter_searching(void);
    void enter_transferring(void);
    void update_screen(void);
    void render_background(void);
    void render_window(void);
    void render_8x8_sprite(uint16_t const sprite_address);
    void render_8x16_sprite(uint16_t const sprite_address);

    void render_sprites(void);
    void render_tile(int16_t const r, int16_t const c, uint16_t const tile_address, uint16_t const palette_address, bool const is_sprite_tile, bool const y_flip=false, bool const x_flip=false, bool const bg_and_window_over_obj=false);
    void press_button(JoypadButton);
    void release_button(JoypadButton);
    std::pair<int16_t, int16_t> get_screen_origin(void) const;
    void call(uint16_t);
    void pop(Register8, Register8);
    void push(uint16_t);
    void render_tilemap(bool const addressing_mode, uint16_t const tile_map, uint16_t const palette_address, uint8_t const origin_y, uint8_t const origin_x);

    void execute_instruction(void);
    void dump_regs(void) const;
    void dump_mem(void) const;
    void dump_screen(void) const;
    void dump_vram(void) const;
};