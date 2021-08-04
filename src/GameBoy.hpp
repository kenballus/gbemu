#pragma once

#include <cstdint>

#define GB_SCREEN_WIDTH 160
#define GB_SCREEN_HEIGHT 144
#define NUM_PX (GB_SCREEN_WIDTH * GB_SCREEN_HEIGHT)
#define NUM_REGISTERS 8
#define TOTAL_RAM 0xFFFF
#define ENTRY_OFFSET 0x100

enum Register8 {
    REG_B = 0b000,
    REG_C = 0b001,
    REG_D = 0b010,
    REG_E = 0b011,
    REG_H = 0b100,
    REG_L = 0b101,
    DUMMY = 0b110,
    REG_F = 0b110,  // Not actually true, but useful
    REG_A = 0b111,
};

enum Register16 {
    REG_BC = 0b00,
    REG_DE = 0b01,
    REG_HL = 0b10,
    REG_SP = 0b11,
    REG_AF = 0b11, // Depending on the instruction, 0b11 can also mean sp
};

enum Flag {
    FL_C = 1 << 7,
    FL_H = 1 << 6,
    FL_N = 1 << 5,
    FL_Z = 1 << 4,
};

class GameBoy {
private:
    std::uint16_t sp = 0;
    std::uint16_t pc = ENTRY_OFFSET;
    std::uint8_t ram[TOTAL_RAM] = {0};
    std::uint8_t registers[NUM_REGISTERS] = {0};

    int cycles_to_wait = 0;

private:
    void wait_for_cycles();
    void write_mem8(std::uint16_t addr, std::uint8_t val);
    void write_mem16(std::uint16_t addr, std::uint16_t val);
    void set_register(Register8 reg, std::uint8_t val);
    void set_register(Register16 reg, std::uint16_t val);
    void set_flag(Flag flag, bool val);
    bool detect_add_carry(std::uint32_t a, std::uint32_t b, std::uint8_t bit) const;

    void ld_r8_r8(Register8 r1, Register8 r2);
    void ld_r8_n8(Register8 reg, std::uint8_t val);
    void ld_r8_hl_addr(Register8 reg);
    void ld_hl_addr_r8(Register8 reg);
    void ld_hl_addr_n8(std::uint8_t val);
    void ld_a_r16_addr(Register16 reg);
    void ld_a_ffr8_addr(Register8 reg);
    void ld_ffr8_addr_a(Register8 reg);
    void ld_a_ffn8_addr(std::uint8_t n8);
    void ld_ffn8_addr_a(std::uint8_t n8);
    void ld_a_n16_addr(std::uint16_t n16);
    void ld_n16_addr_a(std::uint16_t addr);
    void ld_a_hli_addr();
    void ld_a_hld_addr();
    void ld_r16_addr_a(Register16 reg);
    void ld_hli_addr_a();
    void ld_hld_addr_a();
    void ld_r16_n16(Register16 reg, std::uint16_t n16);
    void ld_sp_r16(Register16 r2);
    void push(Register16 reg);
    void pop(Register16 reg);
    void ldhl_sp_e(std::int8_t e8);
    void ld_n16_addr_sp(std::uint16_t n16);
    void add_a_r8(Register8 reg);
    void add_a_n8(std::uint8_t n8);
    void add_a_hl_addr();
    
    void DAA();
    void CPL();
    void NOP();
    void CCF();
    void SCF();
    void DI();
    void EI();

public:
    std::uint8_t read_mem8(std::uint16_t address) const;
    std::uint16_t read_mem16(std::uint16_t address) const;
    std::uint8_t get_register(Register8 reg) const;
    std::uint16_t get_register(Register16 reg) const;
    bool get_flag(Flag flag) const;

    void execute_instruction(std::uint32_t ins32);
    void dump_state() const;
    void dump_mem() const;
};