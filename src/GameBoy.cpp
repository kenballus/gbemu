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

void GameBoy::set_register(Register8 reg, uint8_t val) { registers[reg] = val; }

void GameBoy::set_register(Register16 reg, uint16_t val) {
    switch (reg) {
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

uint8_t GameBoy::get_register(Register8 reg) const { return registers[reg]; }

uint16_t GameBoy::get_register(Register16 reg) const {
    switch (reg) {
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

    std::cerr << "TRIED TO GET NONEXISTENT REGISTER " << std::hex << reg << std::dec << std::endl;
    return 0xFFFF;
}

// 01rrrRRR
void GameBoy::ld_r8_r8(Register8 r1, Register8 r2) {
    // Copy r2 into r1
    set_register(r1, get_register(r2));
    cycles_to_wait += 1;
}

// 00rrr110
// nnnnnnnn
void GameBoy::ld_r8_n8(Register8 reg, uint8_t val) {
    // Copy val into reg
    set_register(reg, val);
    cycles_to_wait += 2;
}

// 01rrr110
void GameBoy::ld_r8_hl_addr(Register8 reg) {
    // Copy [HL] into reg
    set_register(reg, read_mem8(get_register(REG_HL)));
    cycles_to_wait += 2;
}

// 01110rrr
void GameBoy::ld_hl_addr_r8(Register8 reg) {
    // Copy reg into [HL]
    write_mem8(get_register(REG_HL), get_register(reg));
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
void GameBoy::ld_a_r16_addr(Register16 reg) {
    // Copy [reg] into A
    set_register(REG_A, read_mem8(get_register(reg)));
    cycles_to_wait += 2;
}

// 11110010 (for C)
void GameBoy::ld_a_ffr8_addr(Register8 reg) {
    // Copy [FFreg] into A
    set_register(REG_A, read_mem8(0xFF00 + get_register(reg)));
    cycles_to_wait += 2;
}

void GameBoy::ld_ffr8_addr_a(Register8 reg) {
    // Copy A into [FFreg]
    write_mem8(0xFF00 + get_register(reg), get_register(REG_A));
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

void GameBoy::ld_r16_addr_a(Register16 reg) {
    // Copy A into [reg]
    write_mem8(get_register(reg), get_register(REG_A));
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

void GameBoy::ld_r16_n16(Register16 reg, uint16_t n16) {
    if (reg == REG_SP) {
        sp = n16;
    } else {
        set_register(reg, n16);
    }
    cycles_to_wait += 3;
}

void GameBoy::ld_sp_r16(Register16 reg) {
    sp = get_register(reg);
    cycles_to_wait += 4;
}

void GameBoy::push(Register16 reg) {
    sp -= 2;
    write_mem16(sp, get_register(reg));
    cycles_to_wait += 4;
}

void GameBoy::pop(Register16 reg) {
    set_register(reg, read_mem16(sp));
    sp += 2;
    cycles_to_wait += 3;
}

void GameBoy::ldhl_sp_e(int8_t e8) {
    sp = get_register(REG_HL) + e8;
    cycles_to_wait += 3;
}

void GameBoy::ld_n16_addr_sp(uint16_t n16) {
    write_mem16(n16, sp);
    cycles_to_wait += 1;
}

void GameBoy::add_a_r8(Register8 reg) {
    set_register(REG_A, get_register(REG_A) + get_register(reg));
    cycles_to_wait += 1;
}

void GameBoy::add_a_n8(uint8_t n8) {
    set_register(REG_A, get_register(REG_A) + n8);
    cycles_to_wait += 2;
}
void GameBoy::add_a_hl_addr() {
    set_register(REG_A, get_register(REG_A) + read_mem8(get_register(REG_HL)));
    cycles_to_wait += 5;
}

void GameBoy::execute_instruction(uint32_t ins32) {
    uint8_t ins8 = ins32 >> 24;
    uint8_t op = ins8 >> 6;
    Register8 r8_1 = (Register8)(ins8 >> 3 & 0b111);
    Register8 r8_2 = (Register8)(ins8 & 0b111);
    Register16 r16 = (Register16)(ins8 >> 4 & 0b11);
    uint16_t ins16 = ins32 >> 16;
    uint8_t n8 = ins16;  // Type conversion
    int8_t e8 = *reinterpret_cast<int8_t*>(&n8);
    uint16_t n16 = ins32 >> 8;  // Type conversion

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

    } else {
        exit(1);
    }
}

void GameBoy::wait_for_cycles() {
    // dummy
    cycles_to_wait = 0;
}

void GameBoy::dump_state() {
    std::cout << "A:  " << std::hex << (int)registers[REG_A] << ",\t";
    std::cout << "B:  " << std::hex << (int)registers[REG_B] << ",\t";
    std::cout << "C:  " << std::hex << (int)registers[REG_C] << ",\t";
    std::cout << "D:  " << std::hex << (int)registers[REG_D] << ",\t";
    std::cout << "E:  " << std::hex << (int)registers[REG_E] << ",\t";
    std::cout << "F:  " << std::hex << (int)registers[REG_F] << ",\t";
    std::cout << "H:  " << std::hex << (int)registers[REG_H] << ",\t";
    std::cout << "L:  " << std::hex << (int)registers[REG_L] << ",\t";
    std::cout << "SP: " << std::hex << (int)sp << std::dec << ",\t";
    std::cout << "PC: " << std::hex << (int)pc << std::dec << "\n";
}

void GameBoy::dump_mem() {
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