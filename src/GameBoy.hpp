#pragma once

#include <cstdint>
#include <fstream>

#define DEBUG_LEVEL 1
// #define DEBUG(lvl, stuff)
#define DEBUG(lvl, stuff) do { if (lvl <= DEBUG_LEVEL) std::cerr << stuff; } while (0)
#define DEBUGPPU(x)
// #define DEBUGPPU(x) do { std::cerr << "[PPU] " << x; } while (0)

std::int64_t const GB_SCREEN_WIDTH = 160;
std::int64_t const GB_SCREEN_HEIGHT = 144;
std::uint64_t const NUM_REGISTERS = 8;
std::uint32_t const TOTAL_ADDRESSABLE_BYTES = 0x10000;

std::uint16_t const IVT_OFFSET = 0x0000;
std::uint16_t const HEADER_OFFSET = 0x0100;
std::uint16_t const ROM_BANK_0 = 0x0150;
std::uint16_t const ROM_BANK_1 = 0x4000;
// This is where tiles live:
std::uint16_t const UNSIGNED_TILE_DATA_BASE = 0x8000;
std::uint16_t const SIGNED_TILE_DATA_BASE = 0x9000;
// This is where tile maps live:
std::uint16_t const BG_MAP_DATA_1 = 0x9800;
std::uint16_t const BG_MAP_DATA_2 = 0x9C00;

std::uint16_t const CARTRIDGE_RAM = 0xA000;
std::uint16_t const WRAM = 0xC000;
std::uint16_t const ECHO_RAM = 0xE000;
std::uint16_t const OAM = 0xFE00;
std::uint16_t const UNUSED_ADDRESSES = 0xFEA0;
std::uint16_t const IO_REGS = 0xFF00;
std::uint16_t const FAST_RAM = 0xFF80;
std::uint16_t const INTERRUPT_ENABLE = 0xFFFF;

std::uint16_t const VBLANK_INTERRUPT_ADDRESS = 0x0040;
std::uint16_t const LCD_STAT_INTERRUPT_ADDRESS = 0x0048;
std::uint16_t const TIMER_INTERRUPT_ADDRESS = 0x0050;
std::uint16_t const SERIAL_INTERRUPT_ADDRESS = 0x0058;
std::uint16_t const JOYPAD_INTERRUPT_ADDRESS = 0x0060;

std::uint16_t const LCD_CONTROL = 0xFF40;
std::uint16_t const LCD_STATUS = 0xFF41;
std::uint16_t const SCY = 0xFF42;
std::uint16_t const SCX = 0xFF43;
std::uint16_t const LY = 0xFF44;
std::uint16_t const LYC = 0xFF45;
std::uint16_t const OAM_DMA_START = 0xFF46;
std::uint16_t const BGP = 0xFF47;
std::uint16_t const WY = 0xFF4A;
std::uint16_t const WX = 0xFF4B;

std::uint16_t const JOYPAD_PORT = 0xFF00;
std::uint16_t const SERIAL_DATA = 0xFF01;
std::uint16_t const SERIAL_CONTROL = 0xFF02;
std::uint16_t const INTERRUPT_FLAGS = 0xFF0F;
std::uint16_t const DIVIDER_REGISTER = 0xFF04;
std::uint16_t const TIMA = 0xFF05;
std::uint16_t const TMA = 0xFF06;
std::uint16_t const TAC = 0xFF07;

std::uint8_t const FIRST_VBLANK_SCANLINE = 144;
std::uint64_t const BG_MAP_WIDTH = 32; // Tiles
std::uint64_t const BG_MAP_HEIGHT = 32; // Tiles
std::uint64_t const TILE_WIDTH = 8; // Pixels
std::uint64_t const BYTES_PER_TILE = 0x10;
std::uint64_t const BG_MAP_SIZE = 0x400;
// The true clock speed is 4x this, but every instruction takes a multiple of 4 cycles so we're dividing everything by 4
std::uint64_t const CLOCK_SPEED = 1048576;
std::uint64_t const DIVIDER_REGISTER_RATE = 16384;
// So we do (our) clock 64 times for each increment of the divider:
std::uint64_t const CLOCKS_PER_DIVIDER_INCREMENT = 64;

// 16 ms/frame gets us a little over 60 fps
std::uint64_t const MS_PER_CYCLE = 100;

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
    DIRECTIONS = 0,
    ACTIONS = 1,
};

enum GraphicsMode {
    HBLANK = 0,
    VBLANK = 1,
    SEARCHING = 2,
    TRANSFERRING = 3,
};

class GameBoy {
public: // change to private when done debugging
    std::uint16_t pc = HEADER_OFFSET;
    bool ime = 1;
    JoypadMode joypad_mode = DIRECTIONS; // idk if this is true
    std::uint8_t ram[TOTAL_ADDRESSABLE_BYTES] = {0};
    std::uint8_t registers[NUM_REGISTERS + 2] = {0}; // The +2 is for SP. We set these in the constructor for readability

    std::uint64_t cycles_to_wait = 0;
    std::uint64_t cycle_count = 0;

    std::uint32_t dot_count = 0;
    GraphicsMode graphics_mode = SEARCHING;

    bool keys_pressed[8];

    bool halted = false;

    std::uint8_t screen[GB_SCREEN_HEIGHT][GB_SCREEN_WIDTH]; // We render everything to here.

    GameBoy(void);
    void zero_screen(void);
    void load_rom(std::string romfile);
    void wait(void);
    void handle_interrupts(void);

    std::uint8_t read_joypad(void) const;
    std::uint8_t read_mem8(std::uint16_t addr) const;
    std::uint16_t read_mem16(std::uint16_t addr) const;
    void write_mem8(std::uint16_t addr, std::uint8_t val);
    void write_mem16(std::uint16_t addr, std::uint16_t val);
    void set_register8(Register8 const r8, std::uint8_t const u8);
    std::uint8_t get_register8(Register8 r8) const;
    void set_doublereg(Register8 r8_1, Register8 r8_2, std::uint16_t val);
    std::uint16_t get_doublereg(Register8 r8_1, Register8 r8_2) const;
    void set_flag(Flag flag, bool val);
    bool get_flag(Flag flag) const;
    void do_dma(std::uint8_t const start_address);
    void enter_vblank(void);
    void enter_hblank(void);
    void enter_searching(void);
    void enter_transferring(void);
    void update_screen(void);
    void render_background(void);
    void write_bg_tile_to_screen(std::uint8_t, std::uint8_t, std::uint16_t);
    void press_button(JoypadButton);
    void release_button(JoypadButton);

    int execute_instruction(std::uint16_t addr);
    void dump_regs(void) const;
    void dump_mem(void) const;
    void dump_screen(void) const;
};