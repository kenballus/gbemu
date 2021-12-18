#include "GameBoy.hpp"

#include <bitset>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

using std::int8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint8_t;

void GameBoy::load_rom(std::string romfile) {
    std::streampos size;

    std::ifstream ifs = std::ifstream(romfile, std::ios::in | std::ios::binary | std::ios::ate);
    if (ifs.is_open()) {
        size = ifs.tellg();
        ifs.seekg(0, std::ios::beg);
        ifs.read((char*)ram, size);
        ifs.close();
    } else {
        std::cerr << "Unable to open ROM.";
    }
}

void GameBoy::wait_cycles(uint8_t cycles_to_wait) {
    cycles_to_wait = 0;  // Dummy
}

bool is_writable(uint16_t addr) {
    return (addr >= CHARACTER_RAM) && (addr < ECHO_RAM || addr >= OAM) && (addr < UNUSED_ADDRESSES || addr >= IO_REGS);
}

uint8_t GameBoy::read_mem8(uint16_t addr) const {
    if (addr >= ECHO_RAM and addr < OAM) {
        addr -= 0x2000;
    }
    return ram[addr];
}

uint16_t GameBoy::read_mem16(uint16_t addr) const {
    // This could just be a 16-bit read, but we want
    // compatibility with big-endian hosts
    return (ram[addr + 1] << 8) | ram[addr];
}

// Stub
void GameBoy::switch_bank(uint8_t val) { return; }

void GameBoy::write_mem8(uint16_t addr, uint8_t val) {
    if (is_writable(addr)) {
        ram[addr] = val;
    } else if (addr >= ROM_BANK_1 && addr < CHARACTER_RAM) {
        switch_bank(val);
    } else {
        std::cerr << "Attempted illegal write of " << std::hex << (int)val << " to address " << (int)addr << std::dec
                  << "\n";
    }
}

// Little endian
void GameBoy::write_mem16(uint16_t addr, uint16_t val) {
    write_mem8(addr + 1, val >> 8);
    write_mem8(addr, val & 0xFF);  // I think this type conversion would happen automatically anyway.
}

void GameBoy::set_register8(Register8 const r8, uint8_t const u8) {
    registers[r8] = u8;
    std::cerr << "Setting register " << std::bitset<3>{r8} << " to " << std::hex << (int)u8 << std::dec << std::endl;
}

uint8_t GameBoy::get_register8(Register8 r8) const { return registers[r8]; }

void GameBoy::set_doublereg(Register8 r8_1, Register8 r8_2, uint16_t val) {
    registers[r8_1] = val >> 8;
    registers[r8_2] = val;
}

uint16_t GameBoy::get_doublereg(Register8 r8_1, Register8 r8_2) const {
    return (get_register8(r8_1) << 8) + get_register8(r8_2);
}

void GameBoy::set_flag(Flag flag, bool val) {
    std::cerr << "Setting flag " << std::bitset<8>{flag} << " to " << val << "\n";
    registers[REG_F] = (get_register8(REG_F) & ~flag) | (flag * val);
}

bool GameBoy::get_flag(Flag flag) const { return get_register8(REG_F) & flag; }

void GameBoy::dump_state() const {
    std::cout << std::setfill('0') << std::uppercase << std::hex << "AF: " << std::setw(4)
              << (int)get_doublereg(REG_A, REG_F) << std::setw(0) << ",  "
              << "BC: " << std::setw(4) << (int)get_doublereg(REG_B, REG_C) << std::setw(0) << ",  "
              << "DE: " << std::setw(4) << (int)get_doublereg(REG_D, REG_E) << std::setw(0) << ",  "
              << "HL: " << std::setw(4) << (int)get_doublereg(REG_H, REG_L) << std::setw(0) << ",  "
              << "SP: " << std::setw(4) << (int)get_doublereg(REG_S, REG_P) << std::setw(0) << ",  "
              << "PC: " << std::setw(4) << pc << std::setw(0) << "  \n"
              << "C:  " << get_flag(FL_C) << ",     "
              << "H:  " << get_flag(FL_H) << ",     "
              << "N:  " << get_flag(FL_N) << ",     "
              << "Z:  " << get_flag(FL_Z) << "\n"
              << std::dec;
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

int GameBoy::execute_instruction(uint16_t addr) {
    uint8_t const instruction = read_mem8(addr);
    uint8_t const top_two = instruction >> 6;

    Register8 const r8_1 = (Register8)(instruction >> 3 & 0b111);
    Register8 const r8_2 = (Register8)(instruction & 0b111);
    uint8_t const top_five = instruction >> 3;
    uint8_t const bottom_three = r8_2;

    uint8_t const doublereg_bits = (instruction >> 4) & 0b11;
    Register8 d1;
    Register8 d2;
    Register8 q1;
    Register8 q2;
    switch (doublereg_bits) {
        case 0b00:
            d1 = REG_B;
            d2 = REG_C;
            q1 = REG_B;
            q2 = REG_C;
            break;
        case 0b01:
            d1 = REG_D;
            d2 = REG_E;
            q1 = REG_D;
            q2 = REG_E;
            break;
        case 0b10:
            d1 = REG_H;
            d2 = REG_L;
            q1 = REG_H;
            q2 = REG_L;
            break;
        case 0b11:
            d1 = REG_S;
            d2 = REG_P;
            q1 = REG_A;
            q2 = REG_F;
            break;
    }
    Register8 const s1 = d1;  // This is just to go with the naming convention in the docs
    Register8 const s2 = d2;

    uint8_t const n8 = read_mem8(addr + 1);
    int8_t const e8 = n8;

    uint16_t const n16 = read_mem16(addr);

    std::cerr << "Executing instruction " << std::bitset<8>{instruction} << std::dec << ": ";

    if (top_two == 0b01 && r8_1 != DUMMY && r8_2 != DUMMY) {  // LD r, r'
        set_register8(r8_1, get_register8(r8_2));
        wait_cycles(1);
        pc += 1;
    } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b110) {  // LD r, n
        std::cerr << "LD r, n\n";
        set_register8(r8_1, n8);
        wait_cycles(2);
        pc += 2;
    } else if (top_two == 0b01 && r8_1 != DUMMY && bottom_three == 0b110) {  // LD r, (HL)
        std::cerr << "LD r, (HL)\n";
        set_register8(r8_1, read_mem8(get_doublereg(REG_H, REG_L)));
        wait_cycles(2);
        pc += 1;
    } else if (top_five == 0b01110 && r8_2 != DUMMY) {  // LD (HL), r
        std::cerr << "LD (HL), r\n";
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(r8_2));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00110110) {  // LD (HL), n
        std::cerr << "LD (HL), n\n";
        write_mem8(get_doublereg(REG_H, REG_L), n8);
        wait_cycles(3);
        pc += 2;
    } else if (instruction == 0b00001010) {  // LD A, (BC)
        std::cerr << "LD A, (BC)\n";
        set_register8(REG_A, read_mem8(get_doublereg(REG_B, REG_C)));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00011010) {  // LD A, (DE)
        std::cerr << "LD A, (DE)\n";
        set_register8(REG_A, read_mem8(get_doublereg(REG_D, REG_E)));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b11110010) {  // LD A, (C)
        std::cerr << "LD A, (C)\n";
        set_register8(REG_A, read_mem8(0xFF00 + get_register8(REG_C)));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b11100010) {  // LD (C), A
        std::cerr << "LD (C), A\n";
        write_mem8(0xFF00 + get_register8(REG_C), get_register8(REG_A));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b11110000) {  // LD A, (n)
        std::cerr << "LD A, (n)\n";
        set_register8(REG_A, read_mem8(0xFF00 + n8));
        wait_cycles(3);
        pc += 2;
    } else if (instruction == 0b11100000) {  // LD (n), A
        std::cerr << "LD (n), A\n";
        write_mem8(0xFF00 + n8, get_register8(REG_A));
        wait_cycles(3);
        pc += 2;
    } else if (instruction == 0b11111010) {  // LD A, (nn)
        std::cerr << "LD A, (nn)\n";
        set_register8(REG_A, read_mem8(n16));
        wait_cycles(4);
        pc += 3;
    } else if (instruction == 0b11101010) {  // LD (nn), A
        std::cerr << "LD (nn), A\n";
        write_mem8(n16, get_register8(REG_A));
        wait_cycles(4);
        pc += 3;
    } else if (instruction == 0b00101010) {  // LD A, (HLI)
        std::cerr << "LD A, (HLI)\n";
        set_register8(REG_A, read_mem8(get_doublereg(REG_H, REG_L)));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) + 1);  // This should roll over automatically
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00111010) {  // LD A, (HLD)
        std::cerr << "LD A, (HLD)\n";
        set_register8(REG_A, read_mem8(get_doublereg(REG_H, REG_L)));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) - 1);  // This should roll over automatically
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00000010) {  // LD (BC), A
        std::cerr << "LD (BC), A\n";
        write_mem8(get_doublereg(REG_B, REG_C), get_register8(REG_A));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00010010) {  // LD (DE), A
        std::cerr << "LD (DE), A\n";
        write_mem8(get_doublereg(REG_D, REG_E), get_register8(REG_A));
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00100010) {  // LD (HLI), A
        std::cerr << "LD (HLI), A\n";
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(REG_A));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) + 1);  // This should roll over automatically
        wait_cycles(2);
        pc += 1;
    } else if (instruction == 0b00110010) {  // LD (HLD), A
        std::cerr << "LD (HLD), A\n";
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(REG_A));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) - 1);  // This should roll over automatically
        wait_cycles(2);
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b00000001) {  // LD dd, nn
        std::cerr << "LD dd, nn\n";
        set_doublereg(d1, d2, n16);
        wait_cycles(3);
        pc += 3;
    } else if (instruction == 0b11111001) {  // LD SP, HL
        std::cerr << "LD SP, HL\n";
        set_doublereg(REG_S, REG_P, get_doublereg(REG_H, REG_L));
        wait_cycles(2);
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b11000101) {  // PUSH qq
        std::cerr << "PUSH qq\n";
        // These could be doublereg operations but it's easier to not think about it.
        write_mem8(get_doublereg(REG_S, REG_P) - 1, get_register8(q1));
        write_mem8(get_doublereg(REG_S, REG_P) - 2, get_register8(q2));
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) - 2);
        wait_cycles(4);
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b11000001) {  // POP qq
        std::cerr << "POP qq\n";
        // These could be doublereg operations but it's easier to not think about it.
        set_register8(q2, read_mem8(get_doublereg(REG_S, REG_P)));
        set_register8(q1, read_mem8(get_doublereg(REG_S, REG_P) + 1));
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) + 2);
        wait_cycles(3);
        pc += 1;
    // } else if (instruction == 0b11111000) { // LDHL SP, e
    } else if (instruction == 0b00001000) {  // LD (nn), SP
        std::cerr << "LD (nn), SP\n";
        write_mem16(n16, get_doublereg(REG_S, REG_P));
        wait_cycles(5);
        pc += 3;
    // } else if (top_five == 0b10000 && r8_2 != DUMMY) { // ADD A, r
    // } else if (instruction == 0b11000110) { // ADD A, n
    // } else if (instruction == 0b10000110) { // ADD A, (HL)
    // } else if (top_five == 0b10001 && r8_2 != DUMMY) { // ADC A, r
    // } else if (instruction == 0b11001110) { // ADC A, n
    // } else if (instruction == 0b10001110) { // ADC A, (HL)
    // } else if (top_five == 0b10010 && r8_2 != DUMMY) { // SUB r
    // } else if (instruction == 0b11010110) { // SUB n
    // } else if (instruction == 0b10010110) { // SUB (HL)
    // } else if (top_five == 0b10011 && r8_2 != DUMMY) { // SBC A, r
    // } else if (instruction == 0b11011110) { // SBC A, n
    // } else if (instruction == 0b10011110) { // SBC A, (HL)
    // } else if (top_five == 0b10100 && r8_2 != DUMMY) { // AND r
    // } else if (instruction == 0b11100110) { // AND n
    // } else if (instruction == 0b10100110) { // AND (HL)
    } else if (top_five == 0b10110 && r8_2 != DUMMY) {  // OR r
        uint8_t a = get_register8(REG_A);
        uint8_t r = get_register8(r8_1);
        set_register8(REG_A, a | r);
        set_flag(FL_Z, r == 0 && a == 0);  // a | b == 0 only when a == b == 0
        set_flag(FL_N, 0);
        set_flag(FL_H, 0);
        set_flag(FL_C, 0);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b11110110) {  // OR n
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a | n8);
        set_flag(FL_Z, n8 == 0 && a == 0);  // a | b == 0 only when a == b == 0
        set_flag(FL_N, 0);
        set_flag(FL_H, 0);
        set_flag(FL_C, 0);
        wait_cycles(2);
        pc += 2;
    } else if (instruction == 0b10110110) {  // OR (HL)
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a | hl_read);
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, hl_read == 0 && a == 0);  // a | b == 0 only when a == b == 0
        wait_cycles(2);
        pc += 1;
    } else if (top_five == 0b10101 && r8_2 != DUMMY) {  // XOR r
        std::cerr << "XOR r\n";
        set_register8(REG_A, get_register8(REG_A) ^ get_register8(r8_2));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b11101110) {  // XOR n
        std::cerr << "XOR n\n";
        set_register8(REG_A, get_register8(REG_A) ^ n8);
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        wait_cycles(2);
        pc += 2;
    } else if (instruction == 0b10101110) {  // XOR (HL)
        std::cerr << "XOR (HL)\n";
        set_register8(REG_A, get_register8(REG_A) ^ read_mem8(get_doublereg(REG_H, REG_L)));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        wait_cycles(2);
        pc += 1;
    // } else if (top_five == 0b10111 && r8_2 != DUMMY) { // CP r
    // } else if (instruction == 0b11111110) { // CP n
    // } else if (instruction == 0b10111110) { // CP (HL)
    // } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b100) { // INC r
    // } else if (instruction == 0b00110100) { // INC (HL)
    // } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b101) { // DEC r
    // } else if (instruction == 0b00110101) { // DEC (HL)
    // } else if ((instruction & 0b11001111) == 0b00001001) { // ADD HL, ss
    // } else if (instruction == 0b11101000) { // ADD SP, e
    } else if ((instruction & 0b11001111) == 0b00000011) {  // INC ss
        std::cerr << "INC ss\n";
        set_doublereg(s1, s2, get_doublereg(s1, s2) + 1);
        wait_cycles(2);
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b00001011) {  // DEC ss
        std::cerr << "DEC ss\n";
        set_doublereg(s1, s2, get_doublereg(s1, s2) - 1);
        wait_cycles(2);
        pc += 1;
    // } else if (instruction == 0b00000111) { // RLCA
    //     std::cerr << "RLCA\n";
    //     bool lost_bit = get_register8(REG_A) >> 7;
    //     set_register8(REG_A, (get_register8(REG_A) << 1) + lost_bit);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, 0);
    //     wait_cycles(1);
    //     pc += 1;
    // } else if (instruction == 0b00010111) { // RLA
    //     std::cerr << "RLA\n";
    //     bool lost_bit = get_register8(REG_A) >> 7;
    //     set_register8(REG_A, get_register8(REG_A) << 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, 0);
    //     wait_cycles(1);
    //     pc += 1;
    // } else if (instruction == 0b00001111) { // RRCA
    //     std::cerr << "RRCA\n";
    //     bool lost_bit = get_register8(REG_A) & 1;
    //     set_register8(REG_A, (get_register8(REG_A) >> 1) + (lost_bit << 7));
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, 0);
    //     wait_cycles(1);
    //     pc += 1;
    // } else if (instruction == 0b00011111) { // RRA
    //     std::cerr << "RRA\n";
    //     bool lost_bit = get_register8(REG_A) & 1;
    //     set_register8(REG_A, get_register8(REG_A) >> 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, 0);
    //     wait_cycles(1);
    //     pc += 1;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00000 && (n8 & 0b111) != DUMMY) { // RLC r
    //     std::cerr << "RLC r\n";
    //     auto r = n8 & 0b111;
    //     bool lost_bit = get_register8(r) >> 7;
    //     set_register8(r, (get_register8(r) << 1) + lost_bit);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r) == 0);
    //     wait_cycles(2);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && n8 == 0b00000110) { // RLC (HL)
    //     std::cerr << "RLC (HL)\n";
    //     auto hl = get_doublereg(REG_H, REG_L);
    //     bool lost_bit = read_mem8(hl) >> 7;
    //     write_mem8(hl, (read_mem8(hl) << 1) + lost_bit);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(hl) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00010 && (n8 & 0b111) != DUMMY) { // RL r
    //     std::cerr << "RL r\n";
    //     auto r = n8 & 0b111;
    //     bool lost_bit = get_register8(r) >> 7;
    //     set_register8(r, get_register8(r) << 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r));
    //     wait_cycles(2);
    //     pc += 1;
    // } else if (instruction == 0b11001011 && n8 == 0b00010110) { // RL (HL)
    //     std::cerr << "RL (HL)\n";
    //     auto hl = get_doublereg(REG_H, REG_L);
    //     bool lost_bit = read_mem8(hl) >> 7;
    //     write_mem8(hl, read_mem8(hl) << 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(hl) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00001 && (n8 & 0b111) != DUMMY) { // RRC r
    //     std::cerr << "RRC r\n";
    //     auto r = n8 & 0b111;
    //     bool lost_bit = get_register8(r) & 0b1;
    //     set_register8(r, (get_register8(r) >> 1) + lost_bit * 0b10000000);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r) == 0);
    //     wait_cycles(2);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && n8 == 0b00001110) { // RRC (HL)
    //     std::cerr << "RRC (HL)\n";
    //     auto hl = get_doublereg(REG_H, REG_L);
    //     bool lost_bit = read_mem8(hl) & 0b1;
    //     write_mem8(hl, (read_mem8(hl) >> 1) + lost_bit * 0b10000000);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(hl) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00011 && (n8 & 0b111) != DUMMY) { // RR r
    //     std::cerr << "RR r\n";
    //     auto r = n8 & 0b111;
    //     bool lost_bit = get_register8(r) & 0b1;
    //     set_register8(r, get_register8(r) >> 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r));
    //     wait_cycles(2);
    //     pc += 1;
    // } else if (instruction == 0b11011011 && n8 == 0b00011110) { // RR (HL)
    //     std::cerr << "RR (HL)\n";
    //     auto hl = get_doublereg(REG_H, REG_L);
    //     bool lost_bit = read_mem8(hl) & 0b1;
    //     write_mem8(hl, read_mem8(hl) >> 1);
    //     set_flag(FL_C, lost_bit);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(hl) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00100 && (n8 & 0b111) != DUMMY) { // SLA r
    //     std::cerr << "SLA r\n";
    //     uint8_t const r = n8 & 0b111;
    //     set_flag(FL_C, get_register8(r) >> 7);
    //     set_register8(r, get_register8(r) << 1);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r) == 0);
    //     wait_cycles(2);
    //     pc += 2;
    // } else if (instruction == 0b11011011 && n8 == 0b00100110) { // SLA (HL)
    //     std::cerr << "SLA (HL)\n";
    //     uint16_t const hl = get_doublereg(REG_H, REG_L);
    //     uint8_t const hl_read = read_mem8(REG_H, REG_L);
    //     set_flag(FL_C, hl_read >> 7);
    //     write_mem8(hl, hl_read << 1);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(REG_H, REG_L) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00101 && (n8 & 0b111) != DUMMY) { // SRA r
    //     std::cerr << "SRA r\n";
    //     uint8_t const r = n8 & 0b111;
    //     set_flag(FL_C, get_register8(r) & 0b1);
    //     set_register8(r, get_register8(r) >> 1);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, get_register8(r) == 0);
    //     wait_cycles(2);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && n8 == 0b00101110) { // SRA (HL)
    //     std::cerr << "SLA (HL)\n";
    //     uint16_t const hl = get_doublereg(REG_H, REG_L);
    //     uint8_t const hl_read = read_mem8(REG_H, REG_L);
    //     set_flag(FL_C, hl_read & 0b1);
    //     write_mem8(hl, hl_read >> 1);
    //     set_flag(FL_H, 0);
    //     set_flag(FL_N, 0);
    //     set_flag(FL_Z, read_mem8(REG_H, REG_L) == 0);
    //     wait_cycles(4);
    //     pc += 2;
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00111 && (n8 & 0b111) != DUMMY) { // SRL r
    // } else if (instruction == 0b11001011 && n8 == 0b00111110) { // SRL (HL)
    // } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00110 && (n8 & 0b111) != DUMMY) { // SWAP r
    // } else if (instruction == 0b11001011 && n8 == 0b00110110) { // SWAP (HL)
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b01 && n8 & 0b111 != DUMMY) { // BIT b, r
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b01 && n8 & 0b111 == DUMMY) { // BIT b, (HL)
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b11 && n8 & 0b111 != DUMMY) { // SET b, r
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b11 && n8 & 0b111 == DUMMY) { // SET b, (HL)
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b10 && n8 & 0b111 != DUMMY) { // RES b, r
    // } else if (instruction == 0b11001011 && n8 >> 6 == 0b10 && n8 & 0b111 == DUMMY) { // RES b, (HL)
    } else if (instruction == 0b11000011) {  // JP nn
        std::cerr << "JP nn\n";
        pc = n16;
        wait_cycles(4);
        // Don't increment pc here.
    } else if ((instruction & 0b11100111) == 0b11000010) {  // JP cc, nn
        uint8_t cc = (instruction >> 3) & 0b11;
        bool jumping = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (jumping) {
            pc = n16;
            wait_cycles(4);
        } else {
            wait_cycles(3);
            pc += 3;
        }
    // } else if (instruction == 0b00011000) { // JR e
    // } else if ((instruction & 0b11100111) == 0b00100000) { // JR cc, e
    } else if (instruction == 0b11101001) {  // JP (HL)
        std::cerr << "JP (HL)\n";
        pc = get_doublereg(REG_H, REG_L);
        wait_cycles(1);
        // Don't increment pc here
    } else if (instruction == 0b11001101) {  // CALL nn
        std::cerr << "CALL nn\n";
        write_mem8(get_doublereg(REG_S, REG_P) - 1, pc >> 8);
        write_mem8(get_doublereg(REG_S, REG_P) - 2, pc & 0xFF);
        pc = n16;
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) - 2);
        wait_cycles(6);
        // Don't increment pc here
    } else if ((instruction & 0b11100111) == 0b11000100) {  // CALL cc, nn
        uint8_t cc = (instruction >> 3) & 0b11;
        uint16_t const sp = get_doublereg(REG_S, REG_P);
        bool calling = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (calling) {
            write_mem8(sp - 1, pc >> 8);
            write_mem8(sp - 2, pc & 0xFF);
            pc = n16;
            set_doublereg(REG_S, REG_P, sp - 2);
            wait_cycles(6);
            // Don't increment pc here
        } else {
            wait_cycles(3);
            pc += 3;
        }
    } else if (instruction == 0b11001001) {  // RET
        auto const sp = get_doublereg(REG_S, REG_P);
        pc = (read_mem8(sp + 1) << 8) | read_mem8(sp);
        set_doublereg(REG_S, REG_P, sp + 2);
        wait_cycles(4);
        // Don't increment pc here
    // } else if (instruction == 0b11011001) { // RETI
    } else if ((instruction & 0b11100111) == 0b11000000) {  // RET cc
        uint8_t cc = (instruction >> 3) & 0b11;
        bool retting = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (retting) {
            uint16_t const sp = get_doublereg(REG_S, REG_P);
            pc = (read_mem8(sp + 1) << 8) | read_mem8(sp);
            set_doublereg(REG_S, REG_P, sp + 2);
            wait_cycles(5);
            // Don't increment pc here
        } else {
            wait_cycles(2);
            pc += 1;
        }
    } else if (top_two == 0b11 && bottom_three == 0b111) {  // RST t
        std::cerr << "RST t\n";
        write_mem8(get_doublereg(REG_S, REG_P) - 1, pc >> 8);
        write_mem8(get_doublereg(REG_S, REG_P) - 2, pc & 0xFF);
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) - 2);
        pc = 8 * r8_1;
        wait_cycles(4);
        pc += 1;
    // } else if (instruction == 0b00100111) { // DAA
    } else if (instruction == 0b00101111) {  // CPL
        std::cerr << "CPL\n";
        set_register8(REG_A, ~get_register8(REG_A));
        set_flag(FL_H, 1);
        set_flag(FL_N, 1);
        pc += 1;
    } else if (instruction == 0b00000000) {  // NOP
        std::cerr << "NOP\n";
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b00111111) {  // CCF
        std::cerr << "CCF\n";
        set_flag(FL_C, !get_flag(FL_C));
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b00110111) {  // SCF
        std::cerr << "SCF\n";
        set_flag(FL_C, 1);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b11110011) {  // DI
        std::cerr << "DI\n";
        write_mem8(INTERRUPT_REGISTER, 0);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b11111011) {  // EI
        std::cerr << "EI\n";
        write_mem8(INTERRUPT_REGISTER, 1);
        wait_cycles(1);
        pc += 1;
    } else if (instruction == 0b01110110) {  // HALT
        std::cerr << "HALT\n";
        wait_cycles(1);
        pc += 1;
        return 1;                                                // We're overloading HALT to be POWEROFF
    } else if (instruction == 0b00010000 && n8 == 0b00000000) {  // STOP
        std::cerr << "STOP\n";
        wait_cycles(1);
        pc += 1;
        return 1;  // We're overloading STOP to be POWEROFF
    } else {
        std::cerr << "Invalid instruction: " << std::bitset<8>{instruction} << "\n";
        return 1;
    }

    return 0;
}


/*
    TO DO:
    Fix all the rotations and shifts. They have subtle differences with what they do to bits 0 and 7.
    Deal with the carrying and all the annoying flag setting that entails
    Interrupts
    IO regs
    bank switching
    cycle delays
    vblank and framerate
    halt and stop
    interfacing with the screen
    graphics modes
*/