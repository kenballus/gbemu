#pragma once

#include <cstdint>
#include <fstream>

#define GB_SCREEN_WIDTH 160
#define GB_SCREEN_HEIGHT 144
#define NUM_REGISTERS 8
#define TOTAL_RAM 0xFFFF

#define IVT_OFFSET 0x0000
#define HEADER_OFFSET 0x0100
#define ROM_BANK_0 0x0150
#define ROM_BANK_1 0x4000 // Switchable
#define CHARACTER_RAM 0x8000
#define BG_MAP_DATA_1 0x9800
#define BG_MAP_DATA_2 0x9C00
#define CARTRIDGE_RAM 0xA000
#define WRAM 0xC000
#define ECHO_RAM 0xE000 // Not recommended for use
#define OAM 0xFE00
#define UNUSED_ADDRESSES 0xFEA0 // Not recommended for use
#define IO_REGS 0xFF00
#define FAST_RAM 0xFF80
#define INTERRUPT_ENABLE 0xFFFF

#define VBLANK_INTERRUPT_ADDRESS 0x0040
#define LCD_STAT_INTERRUPT_ADDRESS 0x0048
#define TIMER_INTERRUPT_ADDRESS 0x0050
#define SERIAL_INTERRUPT_ADDRESS 0x0058
#define JOYPAD_INTERRUPT_ADDRESS 0x0060

#define LCD_CONTROL 0xFF40
#define LCD_STATUS 0xFF41
#define SCY 0xFF42
#define SCX 0xFF43
#define LY 0xFF44
#define LYC 0xFF45
#define WY 0xFF4A

#define JOYPAD_PORT 0xFF00
#define INTERRUPT_FLAGS 0xFF0F
#define DIVIDER_REGISTER 0xFF04
#define TIMA 0xFF05
#define TMA 0xFF06
#define TAC 0xFF07

// The true clock speed is 4x this, but every instruction takes a multiple of 4 cycles so we're dividing everything by 4
#define CLOCK_SPEED 1048576
#define DIVIDER_REGISTER_RATE 16384
// So we do (our) clock 64 times for each increment of the divider:
#define CLOCKS_PER_DIVIDER_INCREMENT 64

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
    BUTTONS = 1,
};

class GameBoy {
public: // change to private when done debugging
    std::uint16_t pc = HEADER_OFFSET;
    bool ime = 1;
    JoypadMode joypad_mode = DIRECTIONS; // idk if this is true
    std::uint8_t ram[TOTAL_RAM] = {0};
    std::uint8_t registers[NUM_REGISTERS + 2] = {0}; // The +2 is for SP. We set these in the constructor for readability

    std::uint64_t cycles_to_wait = 0;
    std::uint64_t cycle_count = 0;

    bool screen[GB_SCREEN_WIDTH * GB_SCREEN_HEIGHT] = {0}; // We render everything to here.

    GameBoy();
    void load_rom(std::string romfile);
    void wait();
    void handle_interrupts();

    std::uint8_t read_joypad() const;
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

    int execute_instruction(std::uint16_t addr);
    void dump_state() const;
    void dump_mem() const;
    void dump_screen() const;
};