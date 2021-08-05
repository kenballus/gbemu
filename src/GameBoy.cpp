#include "GameBoy.hpp"

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

using std::int8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint8_t;

bool GameBoy::detect_carry(uint32_t a, uint32_t b, uint8_t bit) const {
    // Only works if 0 <= bit <= 30
    uint8_t mask = (1 << (bit + 1)) - 1;
    a &= mask;
    b &= mask;

    return (a + b) >= (1 << (bit + 1));
}

bool GameBoy::detect_borrow(uint32_t a, uint32_t b, uint8_t bit) const {
    // Only works if 0 <= bit <= 30
    a &= (1 << (bit + 1)) - 1;  // mask off everything past bit
    b &= (1 << (bit + 1)) - 1;

    return a > b;
}

uint8_t GameBoy::read_mem8(uint16_t addr) const { return ram[addr]; }

uint16_t GameBoy::read_mem16(uint16_t addr) const {
    // This could just be a 16-bit write, but we want
    // compatibility with big-endian hosts
    return (ram[addr] << 8) | ram[addr + 1];
}

void GameBoy::write_mem8(uint16_t addr, uint8_t val) { ram[addr] = val; }

// Little endian
void GameBoy::write_mem16(uint16_t addr, uint16_t val) {
    ram[addr] = val;           // low byte (Type conversion)
    ram[addr + 1] = val >> 8;  // high byte
}

void GameBoy::set_register(Register8 r8, uint8_t val) { registers[r8] = val; }

void GameBoy::set_register(Register16 r16, uint16_t val) {
    switch (r16) {
        case REG_BC:
            set_register(REG_B, val >> 8);
            set_register(REG_C, val);  // Type conversion
            break;
        case REG_DE:
            set_register(REG_D, val >> 8);
            set_register(REG_E, val);  // Type conversion
            break;
        case REG_HL:
            set_register(REG_H, val >> 8);
            set_register(REG_L, val);  // Type conversion
            break;
        case REG_AF:
            set_register(REG_A, val >> 8);
            set_register(REG_F, val);  // Type conversion
            exit(1);
        default:
            exit(1);
    }
}

uint8_t GameBoy::get_register(Register8 r8) const { return registers[r8]; }

uint16_t GameBoy::get_register(Register16 r16) const {
    switch (r16) {
        case REG_BC:
            return (get_register(REG_B) << 8) | get_register(REG_C);
        case REG_DE:
            return (get_register(REG_D) << 8) | get_register(REG_E);
        case REG_HL:
            return (get_register(REG_H) << 8) | get_register(REG_L);
        case REG_AF:
            return (get_register(REG_A) << 8) | get_register(REG_F);
        default:
            exit(1);
    }

    std::cerr << "TRIED TO GET NONEXISTENT REGISTER " << std::hex << r16 << std::dec << std::endl;
    return 0xFFFF;
}

void GameBoy::set_flag(Flag flag, bool val) { set_register(REG_F, get_register(REG_F) | (flag * val)); }

bool GameBoy::get_flag(Flag flag) const { return get_register(REG_F) & flag; }

// 01rrrRRR
void GameBoy::ld_r8_r8(Register8 r8_1, Register8 r8_2) {
    // Copy r2 into r1
    set_register(r8_1, get_register(r8_2));
    cycles_to_wait += 1;
}

// 00rrr110
// nnnnnnnn
void GameBoy::ld_r8_n8(Register8 r8, uint8_t val) {
    // Copy val into reg
    set_register(r8, val);
    cycles_to_wait += 2;
}

// 01rrr110
void GameBoy::ld_r8_hl_addr(Register8 r8) {
    // Copy [HL] into reg
    set_register(r8, read_mem8(get_register(REG_HL)));
    cycles_to_wait += 2;
}

// 01110rrr
void GameBoy::ld_hl_addr_r8(Register8 r8) {
    // Copy reg into [HL]
    write_mem8(get_register(REG_HL), get_register(r8));
    cycles_to_wait += 2;
}

// 00110110
// nnnnnnnn
void GameBoy::ld_hl_addr_n8(uint8_t val) {
    // Copy val into [HL]
    write_mem8(get_register(REG_HL), val);
    cycles_to_wait += 3;
}

// 00rr1010, rr = 00|01 (BC or DE)
void GameBoy::ld_a_r16_addr(Register16 r16) {
    // Copy [r16] into A
    set_register(REG_A, read_mem8(get_register(r16)));
    cycles_to_wait += 2;
}

// 11110010 (for C)
void GameBoy::ld_a_ffr8_addr(Register8 r8) {
    // Copy [FFreg] into A
    set_register(REG_A, read_mem8(0xFF00 + get_register(r8)));
    cycles_to_wait += 2;
}

void GameBoy::ld_ffr8_addr_a(Register8 r8) {
    // Copy A into [FFreg]
    write_mem8(0xFF00 + get_register(r8), get_register(REG_A));
    cycles_to_wait += 2;
}

void GameBoy::ld_a_ffn8_addr(uint8_t n8) {
    // Copy [0xFF00 + n8] into A
    set_register(REG_A, read_mem8(0xFF00 + n8));
    cycles_to_wait += 3;
}

void GameBoy::ld_ffn8_addr_a(uint8_t n8) {
    // Copy A into [0xFF00 + n8]
    write_mem8(0xFF00 + n8, get_register(REG_A));
    cycles_to_wait += 3;
}

void GameBoy::ld_a_n16_addr(uint16_t n16) {
    // Copy [n16] into A
    set_register(REG_A, read_mem8(n16));
    cycles_to_wait += 4;
}

void GameBoy::ld_n16_addr_a(uint16_t n16) {
    // Copy A into [n16]
    write_mem8(n16, get_register(REG_A));
    cycles_to_wait += 4;
}

void GameBoy::ld_a_hli_addr() {
    // Copy [HL] into A, then inc HL
    set_register(REG_A, read_mem8(get_register(REG_HL)));
    set_register(REG_HL, get_register(REG_HL) + 1);
    cycles_to_wait += 2;
}

void GameBoy::ld_a_hld_addr() {
    // Copy [HL] into A, then dec HL
    set_register(REG_A, read_mem8(get_register(REG_HL)));
    set_register(REG_HL, get_register(REG_HL) - 1);
    cycles_to_wait += 2;
}

void GameBoy::ld_r16_addr_a(Register16 r16) {
    // Copy A into [r16]
    write_mem8(get_register(r16), get_register(REG_A));
    cycles_to_wait += 2;
}

void GameBoy::ld_hli_addr_a() {
    // Copy A into [HL], then inc HL
    write_mem8(get_register(REG_HL), get_register(REG_A));
    set_register(REG_HL, get_register(REG_HL) + 1);
    cycles_to_wait += 2;
}

void GameBoy::ld_hld_addr_a() {
    // Copy A into [HL], then dec HL
    write_mem8(get_register(REG_HL), get_register(REG_A));
    set_register(REG_HL, get_register(REG_HL) - 1);
    cycles_to_wait += 2;
}

void GameBoy::ld_r16_n16(Register16 r16, uint16_t n16) {
    if (r16 == REG_SP) {
        sp = n16;
    } else {
        set_register(r16, n16);
    }
    cycles_to_wait += 3;
}

void GameBoy::ld_sp_r16(Register16 r16) {
    sp = get_register(r16);
    cycles_to_wait += 4;
}

void GameBoy::push(Register16 r16) {
    sp -= 2;
    write_mem16(sp, get_register(r16));
    cycles_to_wait += 4;
}

void GameBoy::pop(Register16 r16) {
    set_register(r16, read_mem16(sp));
    sp += 2;
    cycles_to_wait += 3;
}

void GameBoy::ldhl_sp_e(int8_t e8) {
    // This instruction sets FL_C if there was a carry from bit 15,
    // and sets FL_H if there was a carry from bit 11.
    // It doesn't set any flags for borrows, even though that can happen.
    set_flag(FL_Z, false);
    set_flag(FL_N, false);
    set_flag(FL_C, detect_carry(sp, e8, 15));
    set_flag(FL_H, detect_carry(sp, e8, 11));
    set_register(REG_HL, sp + e8);
    cycles_to_wait += 3;
}

void GameBoy::ld_n16_addr_sp(uint16_t n16) {
    write_mem16(n16, sp);
    cycles_to_wait += 5;
}

void GameBoy::add_a_r8(Register8 r8) {
    uint8_t a_val = get_register(REG_A);
    uint8_t r8_val = get_register(r8);
    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + r8_val) == 0);
    set_flag(FL_H, detect_carry(a_val, r8_val, 3));
    set_flag(FL_C, detect_carry(a_val, r8_val, 7));
    set_register(REG_A, a_val + r8_val);
    cycles_to_wait += 1;
}

void GameBoy::add_a_n8(uint8_t n8) {
    uint8_t a_val = get_register(REG_A);
    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + n8) == 0);
    set_flag(FL_H, detect_carry(a_val, n8, 3));
    set_flag(FL_C, detect_carry(a_val, n8, 7));
    set_register(REG_A, a_val + n8);
    cycles_to_wait += 2;
}
void GameBoy::add_a_hl_addr() {
    uint8_t a_val = get_register(REG_A);
    uint8_t hl_addr_val = read_mem8(get_register(REG_HL));

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + hl_addr_val) == 0);
    set_flag(FL_H, detect_carry(a_val, hl_addr_val, 3));
    set_flag(FL_C, detect_carry(a_val, hl_addr_val, 7));
    set_register(REG_A, a_val + hl_addr_val);
    cycles_to_wait += 2;
}

void GameBoy::adc_a_r8(Register8 r8) {
    uint8_t r8_val = get_register(r8);
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + fl_c_val + r8_val) == 0);
    set_flag(FL_H, detect_carry(a_val, r8_val + fl_c_val, 3));
    set_flag(FL_C, detect_carry(a_val, r8_val + fl_c_val, 7));
    set_register(REG_A, a_val + r8_val + fl_c_val);
    cycles_to_wait += 1;
}

void GameBoy::adc_a_n8(uint8_t n8) {
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + fl_c_val + n8) == 0);
    set_flag(FL_H, detect_carry(a_val, n8 + fl_c_val, 3));
    set_flag(FL_C, detect_carry(a_val, n8 + fl_c_val, 7));
    set_register(REG_A, a_val + n8 + fl_c_val);
    cycles_to_wait += 2;
}

void GameBoy::adc_a_hl_addr() {
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);
    uint8_t hl_addr_val = read_mem8(get_register(REG_HL));

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val + fl_c_val + hl_addr_val) == 0);
    set_flag(FL_H, detect_carry(a_val, hl_addr_val + fl_c_val, 3));
    set_flag(FL_C, detect_carry(a_val, hl_addr_val + fl_c_val, 7));
    set_register(REG_A, a_val + hl_addr_val + fl_c_val);
    cycles_to_wait += 2;
}

void GameBoy::sub_r8(Register8 r8) {
    uint8_t r8_val = get_register(r8);
    uint8_t a_val = get_register(REG_A);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - r8_val) == 0);
    set_flag(FL_H, detect_borrow(a_val, r8_val, 4));
    set_flag(FL_C, detect_borrow(a_val, r8_val, 8));
    set_register(REG_A, a_val - r8_val);
    cycles_to_wait += 1;
}

void GameBoy::sub_n8(uint8_t n8) {
    uint8_t a_val = get_register(REG_A);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - n8) == 0);
    set_flag(FL_H, detect_borrow(a_val, n8, 4));
    set_flag(FL_C, detect_borrow(a_val, n8, 8));
    set_register(REG_A, a_val - n8);
    cycles_to_wait += 2;
}

void GameBoy::sub_hl_addr() {
    uint8_t a_val = get_register(REG_A);
    uint8_t hl_addr_val = read_mem8(get_register(REG_HL));

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - hl_addr_val) == 0);
    set_flag(FL_H, detect_borrow(a_val, hl_addr_val, 4));
    set_flag(FL_C, detect_borrow(a_val, hl_addr_val, 8));
    set_register(REG_A, a_val - hl_addr_val);
    cycles_to_wait += 2;
}

void GameBoy::sbc_a_r8(Register8 r8) {
    uint8_t r8_val = get_register(r8);
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - fl_c_val - r8_val) == 0);
    set_flag(FL_H, detect_borrow(a_val, r8_val - fl_c_val, 4));
    set_flag(FL_C, detect_borrow(a_val, r8_val - fl_c_val, 8));
    set_register(REG_A, a_val - r8_val - fl_c_val);
    cycles_to_wait += 1;
}

void GameBoy::sbc_a_n8(uint8_t n8) {
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - fl_c_val + n8) == 0);
    set_flag(FL_H, detect_borrow(a_val, n8 - fl_c_val, 4));
    set_flag(FL_C, detect_borrow(a_val, n8 - fl_c_val, 8));
    set_register(REG_A, a_val - n8 - fl_c_val);
    cycles_to_wait += 2;
}

void GameBoy::sbc_a_hl_addr() {
    uint8_t a_val = get_register(REG_A);
    bool fl_c_val = get_flag(FL_C);
    uint8_t hl_addr_val = read_mem8(get_register(REG_HL));

    set_flag(FL_N, false);
    set_flag(FL_Z, static_cast<uint8_t>(a_val - fl_c_val - hl_addr_val) == 0);
    set_flag(FL_H, detect_borrow(a_val, hl_addr_val - fl_c_val, 4));
    set_flag(FL_C, detect_borrow(a_val, hl_addr_val - fl_c_val, 8));
    set_register(REG_A, a_val - hl_addr_val - fl_c_val);
    cycles_to_wait += 2;
}

void GameBoy::and_r8(Register8 r8) {
    set_register(REG_A, get_register(REG_A) & get_register(r8));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, true);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 1;
}

void GameBoy::and_n8(uint8_t n8) {
    set_register(REG_A, get_register(REG_A) & n8);
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, true);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::and_hl_addr() {
    set_register(REG_A, get_register(REG_A) & read_mem8(get_register(REG_HL)));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, true);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::or_r8(Register8 r8) {
    set_register(REG_A, get_register(REG_A) | get_register(r8));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 1;
}

void GameBoy::or_n8(uint8_t n8) {
    set_register(REG_A, get_register(REG_A) | n8);
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::or_hl_addr() {
    set_register(REG_A, get_register(REG_A) | read_mem8(get_register(REG_HL)));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::xor_r8(Register8 r8) {
    set_register(REG_A, get_register(REG_A) ^ get_register(r8));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 1;
}

void GameBoy::xor_n8(uint8_t n8) {
    set_register(REG_A, get_register(REG_A) ^ n8);
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::xor_hl_addr() {
    set_register(REG_A, get_register(REG_A) ^ read_mem8(get_register(REG_HL)));
    set_flag(FL_C, false);
    set_flag(FL_N, false);
    set_flag(FL_H, false);
    set_flag(FL_Z, get_register(REG_A) == 0);
    cycles_to_wait += 2;
}

void GameBoy::cp_r8(Register8 r8) {}

void GameBoy::cp_n8(uint8_t n8) {}

void GameBoy::cp_hl_addr() {}

void GameBoy::execute_instruction(uint32_t ins32) {
    uint8_t ins8 = ins32 >> 24;
    uint8_t op = ins8 >> 6;
    Register8 r8_1 = (Register8)(ins8 >> 3 & 0b111);
    Register8 r8_2 = (Register8)(ins8 & 0b111);
    Register16 r16 = (Register16)(ins8 >> 4 & 0b11);
    uint16_t ins16 = ins32 >> 16;
    uint8_t n8 = ins16;  // Type conversion
    int8_t e8 = n8;      // Type conversion
    uint16_t n16 = n8;   // Type conversion (not big-endian compatible)

    if (op == 0b00 && r8_1 != DUMMY && r8_2 == DUMMY) {  // Start 8-bit transfer+IO
        ld_r8_n8(r8_1, n8);
    } else if (op == 0b01 && r8_1 != DUMMY && r8_2 != DUMMY) {
        ld_r8_r8(r8_1, r8_2);
    } else if (op == 0b01 && r8_1 != DUMMY && r8_2 == DUMMY) {
        ld_r8_hl_addr(r8_1);
    } else if (op == 0b01 && r8_1 == DUMMY && r8_2 != DUMMY) {
        ld_hl_addr_r8(r8_2);
    } else if (op == 0b00 && r8_1 == DUMMY && r8_2 == DUMMY) {
        ld_hl_addr_n8(n8);
    } else if (ins8 == 0b00001010) {
        ld_a_r16_addr(REG_BC);
    } else if (ins8 == 0b00011010) {
        ld_a_r16_addr(REG_DE);
    } else if (ins8 == 0b11110010) {
        ld_a_ffr8_addr(REG_C);
    } else if (ins8 == 0b11100010) {
        ld_ffr8_addr_a(REG_C);
    } else if (ins8 == 0b11110000) {
        ld_a_ffn8_addr(n8);
    } else if (ins8 == 0b11100000) {
        ld_ffn8_addr_a(n8);
    } else if (ins8 == 0b11111010) {
        ld_a_n16_addr(n16);
    } else if (ins8 == 0b11101010) {
        ld_n16_addr_a(n16);
    } else if (ins8 == 0b00101010) {
        ld_a_hli_addr();
    } else if (ins8 == 0b00111010) {
        ld_a_hld_addr();
    } else if (ins8 == 0b00000010) {
        ld_r16_addr_a(REG_BC);
    } else if (ins8 == 0b00010010) {
        ld_r16_addr_a(REG_DE);
    } else if (ins8 == 0b00100010) {
        ld_hli_addr_a();
    } else if (ins8 == 0b00110010) {
        ld_hld_addr_a();
    } else if (op == 0b00 && (ins8 & 0b1111) == 0b0001) {  // Start 16-bit transfers
        ld_r16_n16(r16, n16);
    } else if (ins8 == 0b11111001) {
        ld_sp_r16(REG_HL);
    } else if (op == 0b11 && (ins8 & 0b1111) == 0b0101) {
        push(r16);
    } else if (op == 0b11 && (ins8 & 0b1111) == 0b0001) {
        pop(r16);
    } else if (ins8 == 0b11111000) {
        ldhl_sp_e(e8);
    } else if (ins8 == 0b00001000) {
        ld_n16_addr_sp(n16);
    } else if (op == 0b10 && r8_1 == 0b000) {  // Start 8-bit arithmetic and logical operation instructions
        add_a_r8(r8_2);
    } else if (ins8 == 0b11000110) {
        add_a_n8(n8);
    } else if (ins8 == 0b10000110) {
        add_a_hl_addr();
    } else if ((ins8 & 0b11111000) == 0b10001000 && r8_2 != DUMMY) {
        adc_a_r8(r8_2);
    } else if (ins8 == 0b11001110) {
        adc_a_n8(n8);
    } else if (ins8 == 0b10001110) {
        adc_a_hl_addr();
    } else if ((ins8 & 0b11111000) == 0b10010000 && r8_2 != DUMMY) {
        sub_r8(r8_2);
    } else if (ins8 == 0b11010110) {
        sub_n8(n8);
    } else if (ins8 == 0b10010110) {
        sub_hl_addr();
    } else if ((ins8 & 0b11111000) == 0b10011000 && r8_2 != DUMMY) {
        sbc_a_r8(r8_2);
    } else if (ins8 == 0b11011110) {
        sbc_a_n8(n8);
    } else if (ins8 == 0b10011110) {
        sbc_a_hl_addr();
    } else {
        exit(1);
    }

    wait_for_cycles();
}

void GameBoy::wait_for_cycles() {
    // dummy
    cycles_to_wait = 0;
}

void GameBoy::dump_state() const {
    std::cout << std::setfill('0') << std::uppercase << std::hex;
    std::cout << "AF: " << std::setw(4) << get_register(REG_AF) << std::setw(0) << ",  ";
    std::cout << "BC: " << std::setw(4) << get_register(REG_BC) << std::setw(0) << ",  ";
    std::cout << "DE: " << std::setw(4) << get_register(REG_DE) << std::setw(0) << ",  ";
    std::cout << "HL: " << std::setw(4) << get_register(REG_HL) << std::setw(0) << ",  ";
    std::cout << "SP: " << std::setw(4) << sp << std::setw(0) << ",  ";
    std::cout << "PC: " << std::setw(4) << pc << std::setw(0) << "  \n";
    std::cout << "C:  " << get_flag(FL_C) << ",     ";
    std::cout << "H:  " << get_flag(FL_H) << ",     ";
    std::cout << "N:  " << get_flag(FL_N) << ",     ";
    std::cout << "Z:  " << get_flag(FL_Z) << "\n";
    std::cout << std::dec;
}

void GameBoy::dump_mem() const {
    std::string prev_line = "";
    std::string line = "";
    bool starring = false;
    std::stringstream ss;
    ss << std::setfill('0') << std::uppercase << std::hex;

    for (int i = 0; i < TOTAL_RAM; i++) {
        ss << std::setw(2) << (int)read_mem8(i) << std::setw(0) << (i % 16 == 7 ? "  " : (i % 16 == 15 ? "\n" : " "));
        if (i % 16 == 15) {
            prev_line = line;
            line = ss.str();
            ss.str("");
            if (!starring && line == prev_line) {
                std::cout << "*\n";
                starring = true;
            } else if (line != prev_line) {
                std::cout << std::setw(4) << std::setfill('0') << std::hex << i - 15 << "  " << line;
                starring = false;
            }
        }
    }
    std::cout << TOTAL_RAM << std::endl << std::dec << std::setw(0);
}

// Binary Decimal adjustment (use after addition or subtraction)
// void GameBoy::daa() {
//     // init
//     bool c_contents = get_flag(FL_C);
//     bool h_contents = get_flag(FL_H);
//     uint8_t a_contents = get_register(REG_A);

//     if (get_flag(FL_N)) { // subtraction
//         if (c_contents){
//             if (h_contents){
//                 if(0x66 <= a_contents && a_contents <= 0xFF){
//                     set_register(REG_A, a_contents + 0x9A);
//                 }
//             } else {
//                 if(0x07 <= a_contents && a_contents <= 0x9F){
//                     set_register(REG_A, a_contents + 0xA0);
//                 }
//             }
//         } else {
//             if (h_contents){

//             }
//         }
//     } else { // addition

//     }
// }


// Set register A's contents to the complement of its contents
void GameBoy::CPL() {
    set_register(REG_A, get_register(REG_A) ^ 0xFF);
    cycles_to_wait += 1;
}

// No op, wait 1 cycle
void GameBoy::NOP() { cycles_to_wait += 1; }

// Flip carry flag value
void GameBoy::CCF() {
    set_flag(FL_C, !get_flag(FL_C));
    cycles_to_wait += 1;
}

// Set Carry to 1
void GameBoy::SCF() {
    set_flag(FL_C, true);
    cycles_to_wait += 1;
}

// Set EI to 0
void GameBoy::DI() {
    write_mem8(IME_OFFSET, 0);
    cycles_to_wait += 1;
}

// Set EI to 1
void GameBoy::EI() {
    write_mem8(IME_OFFSET, 1);
    cycles_to_wait += 1;
}