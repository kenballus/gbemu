#include "GameBoy.hpp"

#include <bitset>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>

using std::int16_t;
using std::int32_t;
using std::int64_t;
using std::int8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint64_t;
using std::uint8_t;

std::string stringify_reg(Register8 r) {
    switch (r) {  // Don't need breaks if I only return
        case REG_B:
            return "B";
        case REG_C:
            return "C";
        case REG_D:
            return "D";
        case REG_E:
            return "E";
        case REG_H:
            return "H";
        case REG_L:
            return "L";
        case REG_F:
            return "F";
        case REG_A:
            return "A";
        case REG_S:
            return "S";
        case REG_P:
            return "P";
    }
    std::cerr << "Register " << (int)r << " doesn't exist.\n";
    exit(1);
}

std::string stringify_dd_doublereg(uint8_t doublereg_bits) {
    switch (doublereg_bits) {  // Don't need breaks if I only return
        case 0b00:
            return "BC";
        case 0b01:
            return "DE";
        case 0b10:
            return "HL";
        case 0b11:
            return "SP";
    }
    std::cerr << std::bitset<8>{doublereg_bits} << " is not a dd doublereg." << std::endl;
    exit(1);
}

std::string stringify_qq_doublereg(uint8_t doublereg_bits) {
    switch (doublereg_bits) {  // Don't need breaks if I only return
        case 0b00:
            return "BC";
        case 0b01:
            return "DE";
        case 0b10:
            return "HL";
        case 0b11:
            return "AF";
    }
    std::cerr << std::bitset<8>{doublereg_bits} << " is not a dd doublereg." << std::endl;
    exit(1);
}

std::string stringify_flag(Flag flag) {
    switch (flag) {
        case FL_C:
            return "C";
        case FL_N:
            return "N";
        case FL_H:
            return "H";
        case FL_Z:
            return "Z";
    }
    std::cerr << "Flag " << (int)flag << " doesn't exist.\n";
    exit(1);
}

std::string stringify_cc_condition(uint8_t cc) {
    switch (cc) {
        case 0b00:
            return "NZ";
        case 0b01:
            return "Z";
        case 0b10:
            return "NC";
        case 0b11:
            return "C";
    }
    std::cerr << std::bitset<8>{cc} << " is not a cc condition." << std::endl;
    exit(1);
}

bool is_writable(uint16_t addr) {
    if (0xFE00 <= addr && addr <= 0xFEFF) {  // This is a hack. OAM should not be writable (or readable) at all times.
        return true;
    }
    return (addr >= UNSIGNED_TILE_DATA_BASE) && (addr < ECHO_RAM || addr >= OAM) &&
           (addr < UNUSED_ADDRESSES || addr >= IO_REGS);
}

bool detect_carry(uint32_t a, uint32_t b, uint8_t bit) {
    // True if a + b carries from bit
    uint8_t mask = (1 << (bit + 1)) - 1;
    a &= mask;
    b &= mask;

    return a + b >= 1U << (bit + 1);
}

bool detect_borrow(uint32_t a, uint32_t b, uint8_t bit) {
    // True if a - b borrows from bit
    a &= (1 << bit) - 1;  // mask off everything past bit
    b &= (1 << bit) - 1;

    return a < b;
}

void GameBoy::zero_screen(void) {
    for (uint64_t r = 0; r < GB_SCREEN_HEIGHT; r++) {
        for (uint64_t c = 0; c < GB_SCREEN_WIDTH; c++) {
            screen[r][c] = 0;
        }
    }
}

GameBoy::GameBoy(void) {
    DEBUG(1, "Bringin' her up!\n");
    set_doublereg(REG_A, REG_F, 0x01B0);
    set_doublereg(REG_B, REG_C, 0x0013);
    set_doublereg(REG_D, REG_E, 0x00D8);
    set_doublereg(REG_H, REG_L, 0x014D);
    set_doublereg(REG_S, REG_P, 0xFFFE);
    write_mem8(TIMA, 0x00);
    write_mem8(TMA, 0x00);
    write_mem8(TAC, 0x00);
    write_mem8(0xFF10, 0x80);
    write_mem8(0xFF11, 0xBF);
    write_mem8(0xFF12, 0xF3);
    write_mem8(0xFF14, 0xBF);
    write_mem8(0xFF16, 0x3F);
    write_mem8(0xFF17, 0x00);
    write_mem8(0xFF19, 0xBF);
    write_mem8(0xFF1A, 0x7F);
    write_mem8(0xFF1B, 0xFF);
    write_mem8(0xFF1C, 0x9F);
    write_mem8(0xFF1E, 0xBF);
    write_mem8(0xFF20, 0xFF);
    write_mem8(0xFF21, 0x00);
    write_mem8(0xFF22, 0x00);
    write_mem8(0xFF23, 0xBF);
    write_mem8(0xFF24, 0x77);
    write_mem8(0xFF25, 0xF3);
    write_mem8(0xFF26, 0xF1);
    write_mem8(LCD_CONTROL, 0x91);
    ram[LCD_STATUS] = 0x81;  // This is an illegal write otherwise.
    write_mem8(SCY, 0x00);
    write_mem8(SCX, 0x00);
    write_mem8(LY, 0x90);  // TODO: Change this back to 0x91
    write_mem8(LYC, 0x00);
    ram[OAM_DMA_START] = 0xFF;  // Don't want to trigger a DMA right now.
    write_mem8(0xFF47, 0xFC);
    write_mem8(0xFF48, 0xFF);
    write_mem8(0xFF49, 0xFF);
    write_mem8(WY, 0x00);
    write_mem8(WX, 0x00);
    write_mem8(0xFFFF, 0x00);

    zero_screen();
}

void GameBoy::load_rom(std::string romfile) {
    std::streampos size;

    std::ifstream ifs(romfile, std::ios::in | std::ios::binary | std::ios::ate);
    if (ifs.is_open()) {
        size = ifs.tellg();
        ifs.seekg(0, std::ios::beg);
        ifs.read((char*)ram, size);
        ifs.close();
    } else {
        DEBUG(1, "Unable to open ROM.");
    }
}

void GameBoy::wait(void) {
    bool const timer_enabled = (read_mem8(TAC) >> 2) & 0b1;

    while (cycles_to_wait != 0) {
        cycle_count += 1;
        cycles_to_wait -= 1;
        if (cycle_count % CLOCKS_PER_DIVIDER_INCREMENT == 0) {
            DEBUG(2, "\tResetting the divider register!\n");
            write_mem8(DIVIDER_REGISTER, read_mem8(DIVIDER_REGISTER + 1));
        }

        // (our clocks, which are the true clocks / 4)
        uint16_t const clocks_per_timer_increment = 1 << (((((read_mem8(TAC) & 0b11) - 1) % 4) + 1) * 2);
        // This gives the following mapping:
        // 0 -> 256
        // 1 -> 4
        // 2 -> 16
        // 3 -> 64

        if (timer_enabled && cycle_count % clocks_per_timer_increment == 0) {
            if (read_mem8(TIMA) == 0xFF) {
                write_mem8(TIMA, read_mem8(TMA));  // BUG:
                                                   // Technically, if the last instruction was a write to TMA,
                                                   // then we should still copy the old value into TIMA.
                write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b100);  // Request a timer interrupt
                handle_interrupts();
            } else {
                write_mem8(TIMA, read_mem8(TIMA + 1));
            }
        }

        if (read_mem8(LCD_CONTROL) >> 7) {  // If the LCD is enabled (LCDC.7)
            update_screen();
        }

        // You should actually wait some amount of time here.
        // std::this_thread::sleep_for(std::chrono::milliseconds(MS_PER_CYCLE));
    }
}

void GameBoy::press_button(JoypadButton btn) {
    // This is the opposite of what you'd think.
    keys_pressed[btn] = false;
    write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b10000);  // request a joypad interrupt
    handle_interrupts();
}

void GameBoy::release_button(JoypadButton btn) {
    // This is the opposite of what you'd think.
    keys_pressed[btn] = true;
}

uint8_t GameBoy::read_joypad(void) const {
    if (joypad_mode == DIRECTIONS) {
        return (0b0001 << 4) | (keys_pressed[GB_KEY_DOWN] << 3) | (keys_pressed[GB_KEY_UP] << 2) |
               (keys_pressed[GB_KEY_LEFT] << 1) | (keys_pressed[GB_KEY_RIGHT] << 0);
    } else {  // joypad_mode == ACTIONS
        return (0b0010 << 4) | (keys_pressed[GB_KEY_START] << 3) | (keys_pressed[GB_KEY_SELECT] << 2) |
               (keys_pressed[GB_KEY_B] << 1) | (keys_pressed[GB_KEY_A] << 0);
    }
}

uint8_t GameBoy::read_mem8(uint16_t addr) const {
    if (addr >= ECHO_RAM and addr < OAM) {
        addr -= 0x2000;
    }
    if (addr == JOYPAD_PORT) {
        return read_joypad();
    }
    return ram[addr];

    // BUG: OAM+VRAM should not be readable at all times.
}

uint16_t GameBoy::read_mem16(uint16_t addr) const {
    // This could just be a 16-bit read, but we want
    // compatibility with big-endian hosts
    return (read_mem8(addr + 1) << 8) | read_mem8(addr);
}

void GameBoy::handle_interrupts(void) {
    if (!ime) {
        return;
    }

    // The interrupt(s) that are valid to be entered at this time
    uint8_t interrupt_bits = read_mem8(INTERRUPT_FLAGS);

    if (interrupt_bits == 0) {
        return;
    }

    bool const vblank = ((interrupt_bits >> 0) & 0b1);
    bool const lcd_state = ((interrupt_bits >> 1) & 0b1);
    bool const timer = ((interrupt_bits >> 2) & 0b1);
    bool const serial = ((interrupt_bits >> 3) & 0b1);
    bool const joypad = ((interrupt_bits >> 4) & 0b1);

    // The interrupt(s) that the programmer asked for
    bool const vblank_interrupt_enabled = (read_mem8(INTERRUPT_ENABLE) >> 0) & 0b1;
    bool const lcd_state_interrupt_enabled = (read_mem8(INTERRUPT_ENABLE) >> 1) & 0b1;
    bool const timer_interrupt_enabled = (read_mem8(INTERRUPT_ENABLE) >> 2) & 0b1;
    bool const serial_interrupt_enabled = (read_mem8(INTERRUPT_ENABLE) >> 3) & 0b1;
    bool const joypad_interrupt_enabled = (read_mem8(INTERRUPT_ENABLE) >> 4) & 0b1;

    ime = 0;

    uint16_t sp = get_doublereg(REG_S, REG_P);
    write_mem16(get_doublereg(REG_S, REG_P), pc);
    set_doublereg(REG_S, REG_P, sp - 2);

    uint16_t new_pc = 0x9800;  // This would be an invalid place to execute code from, I think.
                               // It at least FEELS like a safe choice. We'll see.
    if (vblank && vblank_interrupt_enabled) {
        new_pc = VBLANK_INTERRUPT_ADDRESS;
        write_mem8(INTERRUPT_FLAGS, interrupt_bits & ~(1 << 0));
    } else if (lcd_state && lcd_state_interrupt_enabled) {
        new_pc = LCD_STAT_INTERRUPT_ADDRESS;
        write_mem8(INTERRUPT_FLAGS, interrupt_bits & ~(1 << 1));
    } else if (timer && timer_interrupt_enabled) {
        new_pc = TIMER_INTERRUPT_ADDRESS;
        write_mem8(INTERRUPT_FLAGS, interrupt_bits & ~(1 << 2));
    } else if (serial && serial_interrupt_enabled) {
        new_pc = SERIAL_INTERRUPT_ADDRESS;
        write_mem8(INTERRUPT_FLAGS, interrupt_bits & ~(1 << 3));
    } else if (joypad && joypad_interrupt_enabled) {
        new_pc = JOYPAD_INTERRUPT_ADDRESS;
        write_mem8(INTERRUPT_FLAGS, interrupt_bits & ~(1 << 4));
    }

    if (new_pc != 0x9800) {
        halted = false;
        // Do the interrupt call
        write_mem16(sp - 2, pc);
        pc = new_pc;
        set_doublereg(REG_S, REG_P, sp - 2);
    }

    cycles_to_wait += 5;
}

void GameBoy::write_mem8(uint16_t addr, uint8_t val) {
    if (read_mem8(addr) != val) {
        DEBUG(2, "\tWriting 0x" << std::hex << std::setw(2) << std::setfill('0') << (int64_t)val << std::setw(0)
                                << " to address 0x" << addr << std::dec << "\n");
    }

    if (addr == DIVIDER_REGISTER) {
        ram[addr] = 0x00;  // See page 25 of the Nintendo programming docs for why
    } else if (addr == JOYPAD_PORT) {
        if ((val >> 4) & 0b1) {
            joypad_mode = DIRECTIONS;
        } else if ((val >> 5) & 0b1) {
            joypad_mode = ACTIONS;
        }
    } else if (addr == INTERRUPT_FLAGS) {
        ram[addr] = val & 0b11111;  // There are only 5 interrupt flags
    } else if (addr == OAM_DMA_START) {
        do_dma(val);
    } else if (addr == SERIAL_DATA) {
        // std::cout << "[SERIAL] 0x" << std::hex << (int64_t)val << std::dec << std::endl;
        // std::cout << (char)val;
    } else if (is_writable(addr)) {
        ram[addr] = val;
    } else if (0x2000 <= addr && addr <= 0x5FFF) {
        std::cerr << "Attempted bank switch, which is not implemented.\n";
        // exit(1);
    } else {
        std::cerr << "Attempted illegal write of 0x" << std::hex << (int64_t)val << " to address 0x" << (int64_t)addr
                  << std::dec << "\n";
        // exit(1);
    }
}

// Little endian
void GameBoy::write_mem16(uint16_t addr, uint16_t val) {
    write_mem8(addr + 1, val >> 8);
    write_mem8(addr, val & 0xFF);  // I think this type conversion would happen automatically anyway.
}

void GameBoy::set_register8(Register8 const r8, uint8_t const u8) {
    registers[r8] = u8;
    DEBUG(2, "\tSetting register " << stringify_reg(r8) << " to 0x" << std::hex << std::setw(2) << std::setfill('0')
                                   << (int64_t)u8 << std::setw(0) << std::dec << std::endl);
}

uint8_t GameBoy::get_register8(Register8 r8) const { return registers[r8]; }

void GameBoy::set_doublereg(Register8 r8_1, Register8 r8_2, uint16_t val) {
    set_register8(r8_1, val >> 8);
    set_register8(r8_2, val & 0xFF);
}

uint16_t GameBoy::get_doublereg(Register8 r8_1, Register8 r8_2) const {
    return (get_register8(r8_1) << 8) + get_register8(r8_2);
}

void GameBoy::set_flag(Flag flag, bool val) {
    DEBUG(2, "\tSetting flag " << stringify_flag(flag) << " to " << val << "\n");
    registers[REG_F] = (get_register8(REG_F) & ~flag) | (flag * val);
}

bool GameBoy::get_flag(Flag flag) const { return get_register8(REG_F) & flag; }

void GameBoy::do_dma(uint8_t const start_address) {
    uint16_t const real_start_address = start_address << 8;
    for (uint16_t i = 0; i < 0xFF; i++) {
        write_mem8(OAM + i, read_mem8(real_start_address + i));
    }
}

void GameBoy::enter_hblank(void) {
    DEBUGPPU("Entering hblank state.\n");
    // Write down the current graphics mode
    ram[LCD_STATUS] = (read_mem8(LCD_STATUS) & 0b11111100) | 0b00;

    graphics_mode = HBLANK;

    if (read_mem8(LCD_STATUS) & 0b00001000) {
        write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b10);  // request a stat interrupt
        handle_interrupts();
    }
}

void GameBoy::enter_vblank(void) {
    DEBUGPPU("Entering vblank state.\n");
    // Write down the current graphics mode
    ram[LCD_STATUS] = (read_mem8(LCD_STATUS) & 0b11111100) | 0b01;

    if (read_mem8(LCD_STATUS) & 0b00010000) {
        write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b10);  // request a stat interrupt
    }

    // If we call this function when LY is stubbed for testing, it shouldn't do an interrupt
    if (read_mem8(LY) >= FIRST_VBLANK_SCANLINE) {
        write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b00000001);  // request a vblank interrupt
        handle_interrupts();
    }

    graphics_mode = VBLANK;
}

void GameBoy::enter_searching(void) {
    DEBUGPPU("Entering searching state.\n");
    // Write down the current graphics mode
    ram[LCD_STATUS] = (read_mem8(LCD_STATUS) & 0b11111100) | 0b10;

    graphics_mode = SEARCHING;

    if (read_mem8(LCD_STATUS) & 0b00100000) {
        write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b10);  // request a stat interrupt
        handle_interrupts();
    }
}

void GameBoy::enter_transferring(void) {
    DEBUGPPU("Entering transferring state.\n");
    // Write down the current graphics mode
    ram[LCD_STATUS] = (read_mem8(LCD_STATUS) & 0b11111100) | 0b11;

    graphics_mode = TRANSFERRING;
}

void GameBoy::write_bg_tile_to_screen(uint8_t r, uint8_t c, uint16_t tile_address) {
    for (uint8_t i = 0; i < 8; i++) {  // For each row in the tile,
        // Read a row from the tile
        // (one row is 2 bytes, interleaved)
        uint8_t const tile_data_hi = read_mem8(tile_address + 2 * i);
        uint8_t const tile_data_lo = read_mem8(tile_address + 2 * i + 1);
        uint16_t const tile_data = (tile_data_hi << 8) | tile_data_lo;

        for (uint8_t j = 0; j < 8; j++) {  // For each pixel in the row,
            int16_t const y = r + i + read_mem8(SCY);
            int16_t const x = c + j + read_mem8(SCX);

            if (0 <= y && y < GB_SCREEN_HEIGHT && 0 <= x && x < GB_SCREEN_WIDTH) {
                uint8_t const palette_index = ((tile_data >> (14 - j)) & 0b10) + ((tile_data >> (7 - j)) & 0b1);
                uint8_t const color = (read_mem8(BGP) >> (2 * palette_index)) & 0b11;
                screen[y][x] = color;
            }
        }
    }
}

void GameBoy::render_background(void) {
    bool const addressing_mode = (read_mem8(LCD_CONTROL) >> 4) & 0b1;
    uint16_t const bg_map_data = (read_mem8(LCD_CONTROL) >> 3) & 0b1 ? BG_MAP_DATA_2 : BG_MAP_DATA_1;

    for (uint16_t i = 0; i < BG_MAP_SIZE; i++) {
        if (addressing_mode) {  // unsigned, 0x8000-based
            write_bg_tile_to_screen((i / BG_MAP_WIDTH) * TILE_WIDTH, (i % BG_MAP_WIDTH) * TILE_WIDTH,
                                    UNSIGNED_TILE_DATA_BASE + read_mem8(bg_map_data + i) * BYTES_PER_TILE);
        } else {  // signed, 0x9000-based
            write_bg_tile_to_screen((i / BG_MAP_WIDTH) * TILE_WIDTH, (i % BG_MAP_WIDTH) * TILE_WIDTH,
                                    SIGNED_TILE_DATA_BASE + (int8_t)read_mem8(bg_map_data + i) * BYTES_PER_TILE);
        }
    }
}

void GameBoy::update_screen(void) {
    DEBUGPPU("Doing 16 dots.\n");
    // Called once per CPU cycle, if the LCD is enabled.

    dot_count += 16;     // Dot clock = 4 * real clock = 4 * 4 * our clock
    dot_count %= 70224;  // It takes 70224 dots to do one frame

    // Update the current line number
    // write_mem8(LY, dot_count / 456); // TODO: UNCOMMENT THIS LINE

    // STAT.2 is set iff LY=LYC, and updated constantly.
    if (read_mem8(LY) == read_mem8(LYC)) {
        write_mem8(LCD_STATUS, read_mem8(LCD_STATUS) | 0b00000100);
        if (read_mem8(LCD_STATUS) & 0b01000000) {
            write_mem8(INTERRUPT_FLAGS, read_mem8(INTERRUPT_FLAGS) | 0b10);  // request a stat interrupt
            handle_interrupts();
        }
    } else {
        write_mem8(LCD_STATUS, read_mem8(LCD_STATUS) & 0b11111011);
    }


    if (dot_count >= 65664) {  // vblank
        if (graphics_mode != VBLANK) {
            // enter_vblank(); // TODO: UNCOMMENT THIS LINE
            // Draw the whole background at once upon entering vblank
            // The real thing does it line by line as it goes, but this is easier
            if (read_mem8(LCD_CONTROL) & 0b1) {
                render_background();
            }
        }
    } else if (dot_count % 456 >= 248) {  // hblank (not yet allowing mode 3 extension)
        if (graphics_mode != HBLANK) {
            enter_hblank();
        }
    } else if (dot_count % 456 >= 80) {  // transferring (not yet allowing mode 3 extension)
        if (graphics_mode != TRANSFERRING) {
            enter_transferring();
        }
    } else if (dot_count % 456 < 80) {  // searching
        if (graphics_mode != SEARCHING) {
            enter_searching();
        }
    }
}

void GameBoy::dump_regs(void) const {
    std::cout << std::setfill('0') << std::uppercase << std::hex << "AF: " << std::setw(4)
              << (int64_t)get_doublereg(REG_A, REG_F) << std::setw(0) << ",  "
              << "BC: " << std::setw(4) << (int64_t)get_doublereg(REG_B, REG_C) << std::setw(0) << ",  "
              << "DE: " << std::setw(4) << (int64_t)get_doublereg(REG_D, REG_E) << std::setw(0) << ",  "
              << "HL: " << std::setw(4) << (int64_t)get_doublereg(REG_H, REG_L) << std::setw(0) << ",  "
              << "SP: " << std::setw(4) << (int64_t)get_doublereg(REG_S, REG_P) << std::setw(0) << ",  "
              << "PC: " << std::setw(4) << pc << std::setw(0) << "  \n"
              << "Z:  " << get_flag(FL_Z) << ",     "
              << "N:  " << get_flag(FL_N) << ",     "
              << "H:  " << get_flag(FL_H) << ",     "
              << "C:  " << get_flag(FL_C) << "\n"
              << std::dec;
}

void GameBoy::dump_mem(void) const {
    std::string prev_line = "";
    std::string line = "";
    bool starring = false;
    std::stringstream ss;
    ss << std::setfill('0') << std::uppercase << std::hex;

    for (uint32_t i = 0; i < TOTAL_ADDRESSABLE_BYTES; i++) {
        ss << std::setw(2) << (int64_t)read_mem8(i) << std::setw(0)
           << (i % 16 == 7 ? "  " : (i % 16 == 15 ? "\n" : " "));
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
    std::cout << std::hex << TOTAL_ADDRESSABLE_BYTES << std::dec << std::setw(0) << std::endl;
}

void GameBoy::dump_screen(void) const {
    for (uint16_t r = 0; r < GB_SCREEN_HEIGHT; r++) {
        for (uint16_t c = 0; c < GB_SCREEN_WIDTH; c++) {
            char px;
            switch (screen[r][c]) {
                case 0b00:
                    px = 'O';
                    break;
                case 0b01:
                    px = '0';
                    break;
                case 0b10:
                    px = 'X';
                    break;
                case 0b11:
                    px = '#';
                    break;
                default:
                    px = 'U';
            }
            std::cout << px;
        }
        std::cout << "\n";
    }
}

int GameBoy::execute_instruction(uint16_t addr) {
    std::cout << std::uppercase << std::hex << "A: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_A)
              << std::setw(0) << " F: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_F)
              << std::setw(0) << " B: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_B)
              << std::setw(0) << " C: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_C)
              << std::setw(0) << " D: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_D)
              << std::setw(0) << " E: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_E)
              << std::setw(0) << " H: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_H)
              << std::setw(0) << " L: " << std::setw(2) << std::setfill('0') << (int)get_register8(REG_L)
              << std::setw(0) << " SP: " << std::setw(4) << std::setfill('0') << (int)get_doublereg(REG_S, REG_P)
              << std::setw(0) << " PC: 00:" << std::setw(4) << std::setfill('0') << pc << std::setw(0) << " ("
              << std::setw(2) << std::setfill('0') << (int)read_mem8(pc) << std::setw(0) << " " << std::setw(2)
              << std::setfill('0') << (int)read_mem8(pc + 1) << std::setw(0) << " " << std::setw(2) << std::setfill('0')
              << (int)read_mem8(pc + 2) << std::setw(0) << " " << std::setw(2) << std::setfill('0')
              << (int)read_mem8(pc + 3) << std::setw(0) << ")\n"
              << std::dec;
    if (halted) {
        return 0;
    }

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
    uint8_t cc = (instruction >> 3) & 0b11;
    uint16_t const n16 = read_mem16(addr + 1);

    DEBUG(1, "0x" << std::hex << (int64_t)pc << ": ");
    // << "Executing instruction 0x" << std::setfill('0') << std::setw(2) << (int64_t)instruction << std::setw(0)
    // << " " << std::dec << "(0b" << std::bitset<8>{instruction} << ")"
    // << ": ");

    if (top_two == 0b01 && r8_1 != DUMMY && r8_2 != DUMMY) {  // LD r, r'
        DEBUG(1, "LD " << stringify_reg(r8_1) << ", " << stringify_reg(r8_2) << " \n");
        set_register8(r8_1, get_register8(r8_2));
        cycles_to_wait += 1;
        pc += 1;
    } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b110) {  // LD r, n
        DEBUG(1, "LD " << stringify_reg(r8_1) << ", 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        set_register8(r8_1, n8);
        cycles_to_wait += 2;
        pc += 2;
    } else if (top_two == 0b01 && r8_1 != DUMMY && bottom_three == 0b110) {  // LD r, (HL)
        DEBUG(1, "LD " << stringify_reg(r8_1) << ", (HL)\n");
        set_register8(r8_1, read_mem8(get_doublereg(REG_H, REG_L)));
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b01110 && r8_2 != DUMMY) {  // LD (HL), r
        DEBUG(1, "LD (HL), " << stringify_reg(r8_2) << "\n");
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(r8_2));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00110110) {  // LD (HL), n
        DEBUG(1, "LD (HL), 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        write_mem8(get_doublereg(REG_H, REG_L), n8);
        cycles_to_wait += 3;
        pc += 2;
    } else if (instruction == 0b00001010) {  // LD A, (BC)
        DEBUG(1, "LD A, (BC)\n");
        set_register8(REG_A, read_mem8(get_doublereg(REG_B, REG_C)));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00011010) {  // LD A, (DE)
        DEBUG(1, "LD A, (DE)\n");
        set_register8(REG_A, read_mem8(get_doublereg(REG_D, REG_E)));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b11110010) {  // LD A, (C)
        DEBUG(1, "LD A, (C)\n");
        set_register8(REG_A, read_mem8(0xFF00 + get_register8(REG_C)));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b11100010) {  // LD (C), A
        DEBUG(1, "LD (C), A\n");
        write_mem8(0xFF00 + get_register8(REG_C), get_register8(REG_A));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b11110000) {  // LD A, (n)
        DEBUG(1, "LD A, (0x" << std::hex << 0xFF00 + n8 << std::dec << ")\n");
        set_register8(REG_A, read_mem8(0xFF00 + n8));
        cycles_to_wait += 3;
        pc += 2;
    } else if (instruction == 0b11100000) {  // LD (n), A
        DEBUG(1, "LD (0x" << std::hex << 0xFF00 + n8 << std::dec << "), A\n");
        write_mem8(0xFF00 + n8, get_register8(REG_A));
        cycles_to_wait += 3;
        pc += 2;
    } else if (instruction == 0b11111010) {  // LD A, (nn)
        DEBUG(1, "LD A, (0x" << std::hex << n16 << std::dec << ")\n");
        set_register8(REG_A, read_mem8(n16));
        cycles_to_wait += 4;
        pc += 3;
    } else if (instruction == 0b11101010) {  // LD (nn), A
        DEBUG(1, "LD (0x" << std::hex << n16 << std::dec << "), A\n");
        write_mem8(n16, get_register8(REG_A));
        cycles_to_wait += 4;
        pc += 3;
    } else if (instruction == 0b00101010) {  // LD A, (HLI)
        DEBUG(1, "LD A, (HLI)\n");
        set_register8(REG_A, read_mem8(get_doublereg(REG_H, REG_L)));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) + 1);  // This should roll over automatically
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00111010) {  // LD A, (HLD)
        DEBUG(1, "LD A, (HLD)\n");
        set_register8(REG_A, read_mem8(get_doublereg(REG_H, REG_L)));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) - 1);  // This should roll over automatically
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00000010) {  // LD (BC), A
        DEBUG(1, "LD (BC), A\n");
        write_mem8(get_doublereg(REG_B, REG_C), get_register8(REG_A));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00010010) {  // LD (DE), A
        DEBUG(1, "LD (DE), A\n");
        write_mem8(get_doublereg(REG_D, REG_E), get_register8(REG_A));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00100010) {  // LD (HLI), A
        DEBUG(1, "LD (HLI), A\n");
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(REG_A));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) + 1);  // This should roll over automatically
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00110010) {  // LD (HLD), A
        DEBUG(1, "LD (HLD), A\n");
        write_mem8(get_doublereg(REG_H, REG_L), get_register8(REG_A));
        set_doublereg(REG_H, REG_L, get_doublereg(REG_H, REG_L) - 1);  // This should roll over automatically
        cycles_to_wait += 2;
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b00000001) {  // LD dd, nn
        DEBUG(1, "LD " << stringify_dd_doublereg(doublereg_bits) << ", 0x" << std::hex << n16 << std::dec << "\n");
        set_doublereg(d1, d2, n16);
        cycles_to_wait += 3;
        pc += 3;
    } else if (instruction == 0b11111001) {  // LD SP, HL
        DEBUG(1, "LD SP, HL\n");
        set_doublereg(REG_S, REG_P, get_doublereg(REG_H, REG_L));
        cycles_to_wait += 2;
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b11000101) {  // PUSH qq
        DEBUG(1, "PUSH " << stringify_qq_doublereg(doublereg_bits) << "\n");
        // These could be doublereg operations but it's easier to not think about it.
        write_mem8(get_doublereg(REG_S, REG_P) - 1, get_register8(q1));
        write_mem8(get_doublereg(REG_S, REG_P) - 2, get_register8(q2));
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) - 2);
        cycles_to_wait += 4;
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b11000001) {  // POP qq
        DEBUG(1, "POP " << stringify_qq_doublereg(doublereg_bits) << "\n");
        // These could be doublereg operations but it's easier to not think about it.
        set_register8(q2, read_mem8(get_doublereg(REG_S, REG_P)));
        set_register8(q1, read_mem8(get_doublereg(REG_S, REG_P) + 1));
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) + 2);
        cycles_to_wait += 3;
        pc += 1;
    } else if (instruction == 0b11111000) {  // LDHL SP, e
        DEBUG(1, "LDHL SP, e\n");
        DEBUG(1, "UNIMPLEMENTED INSTRUCTION\n");
        exit(1);
    } else if (instruction == 0b00001000) {  // LD (nn), SP
        DEBUG(1, "LD (0x" << std::hex << n16 << std::dec << "), SP\n");
        write_mem16(n16, get_doublereg(REG_S, REG_P));
        cycles_to_wait += 5;
        pc += 3;
    } else if (top_five == 0b10000 && r8_2 != DUMMY) {  // ADD A, r
        DEBUG(1, "ADD A, " << stringify_reg(r8_2) << "\n");
        uint8_t const r_val = get_register8(r8_2);
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a + r_val);
        set_flag(FL_C, detect_carry(a, r_val, 7));
        set_flag(FL_H, detect_carry(a, r_val, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11000110) {  // ADD A, n
        DEBUG(1, "ADD A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a + n8);
        set_flag(FL_C, detect_carry(a, n8, 7));
        set_flag(FL_H, detect_carry(a, n8, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10000110) {  // ADD A, (HL)
        DEBUG(1, "ADD A, (HL)\n");
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a + hl_read);
        set_flag(FL_C, detect_carry(a, hl_read, 7));
        set_flag(FL_H, detect_carry(a, hl_read, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10001 && r8_2 != DUMMY) {  // ADC A, r
        DEBUG(1, "ADC A, " << stringify_reg(r8_2) << "\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        uint8_t const r_val = get_register8(r8_2);
        set_register8(REG_A, a + r_val + old_fl_c);
        set_flag(FL_C, detect_carry(a, r_val, 7));
        set_flag(FL_H, detect_carry(a, r_val, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11001110) {  // ADC A, n
        DEBUG(1, "ADC A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a + n8 + old_fl_c);
        set_flag(FL_C, detect_carry(a, n8, 7));
        set_flag(FL_H, detect_carry(a, n8, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10001110) {  // ADC A, (HL)
        DEBUG(1, "ADC A, (HL)\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_register8(REG_A, a + hl_read + old_fl_c);
        set_flag(FL_C, detect_carry(a, hl_read, 7));
        set_flag(FL_H, detect_carry(a, hl_read, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10010 && r8_2 != DUMMY) {  // SUB r
        DEBUG(1, "SUB " << stringify_reg(r8_2) << "\n");
        uint8_t const r = get_register8(r8_2);
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a - r);
        set_flag(FL_C, detect_borrow(a, r, 8));
        set_flag(FL_H, detect_borrow(a, r, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == r);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11010110) {  // SUB n
        DEBUG(1, "SUB A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a - n8);
        set_flag(FL_C, detect_borrow(a, n8, 8));
        set_flag(FL_H, detect_borrow(a, n8, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == n8);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10010110) {  // SUB (HL)
        DEBUG(1, "SUB (HL)\n");
        uint8_t const a = get_register8(REG_A);
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_register8(REG_A, a - hl_read);
        set_flag(FL_C, detect_borrow(a, hl_read, 8));
        set_flag(FL_H, detect_borrow(a, hl_read, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == hl_read);
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10011 && r8_2 != DUMMY) {  // SBC A, r
        DEBUG(1, "SBC A, " << stringify_reg(r8_2) << "\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        uint8_t const r = get_register8(r8_2);
        set_register8(REG_A, a - r - old_fl_c);
        set_flag(FL_C, detect_borrow(a, r, 8));
        set_flag(FL_H, detect_borrow(a, r, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == r + old_fl_c);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11011110) {  // SBC A, n
        DEBUG(1, "SBC A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a - n8 - old_fl_c);
        set_flag(FL_C, detect_borrow(a, n8, 8));
        set_flag(FL_H, detect_borrow(a, n8, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == n8 + old_fl_c);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10011110) {  // SBC A, (HL)
        DEBUG(1, "SBC A, (HL)\n");
        bool const old_fl_c = get_flag(FL_C);
        uint8_t const a = get_register8(REG_A);
        uint8_t hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_register8(REG_A, a - hl_read - old_fl_c);
        set_flag(FL_C, detect_borrow(a, hl_read, 8));
        set_flag(FL_H, detect_borrow(a, hl_read, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == hl_read + old_fl_c);
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10100 && r8_2 != DUMMY) {  // AND r
        DEBUG(1, "AND A, " << stringify_reg(r8_2) << "\n");
        set_register8(REG_A, get_register8(REG_A) & get_register8(r8_2));
        set_flag(FL_C, 0);
        set_flag(FL_H, 1);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11100110) {  // AND n
        DEBUG(1, "AND A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        set_register8(REG_A, get_register8(REG_A) & n8);
        set_flag(FL_C, 0);
        set_flag(FL_H, 1);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10100110) {  // AND (HL)
        DEBUG(1, "AND (HL)\n");
        set_register8(REG_A, get_register8(REG_A) & read_mem8(get_doublereg(REG_H, REG_L)));
        set_flag(FL_C, 0);
        set_flag(FL_H, 1);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (top_five == 0b10110 && r8_2 != DUMMY) {  // OR r
        DEBUG(1, "OR A, " << stringify_reg(r8_2) << "\n");
        uint8_t const a = get_register8(REG_A);
        uint8_t const r_val = get_register8(r8_2);
        set_register8(REG_A, a | r_val);
        set_flag(FL_Z, r_val == 0 && a == 0);  // a | b == 0 only when a == b == 0
        set_flag(FL_N, 0);
        set_flag(FL_H, 0);
        set_flag(FL_C, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11110110) {  // OR n
        DEBUG(1, "OR A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a | n8);
        set_flag(FL_Z, n8 == 0 && a == 0);  // a | b == 0 only when a == b == 0
        set_flag(FL_N, 0);
        set_flag(FL_H, 0);
        set_flag(FL_C, 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10110110) {  // OR (HL)
        DEBUG(1, "OR A, (HL)\n");
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        uint8_t const a = get_register8(REG_A);
        set_register8(REG_A, a | hl_read);
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, hl_read == 0 && a == 0);  // a | b == 0 only when a == b == 0
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10101 && r8_2 != DUMMY) {  // XOR r
        DEBUG(1, "XOR A, " << stringify_reg(r8_2) << "\n");
        set_register8(REG_A, get_register8(REG_A) ^ get_register8(r8_2));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11101110) {  // XOR n
        DEBUG(1, "XOR A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        set_register8(REG_A, get_register8(REG_A) ^ n8);
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10101110) {  // XOR (HL)
        DEBUG(1, "XOR A, (HL)\n");
        set_register8(REG_A, get_register8(REG_A) ^ read_mem8(get_doublereg(REG_H, REG_L)));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 2;
        pc += 1;
    } else if (top_five == 0b10111 && r8_2 != DUMMY) {  // CP r
        DEBUG(1, "CP A, " << stringify_reg(r8_2) << "\n");
        uint8_t const a = get_register8(REG_A);
        uint8_t const r = get_register8(r8_1);
        set_flag(FL_C, detect_borrow(a, r, 8));
        set_flag(FL_H, detect_borrow(a, r, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == r);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11111110) {  // CP n
        DEBUG(1, "CP A, 0x" << std::hex << (int64_t)n8 << std::dec << "\n");
        uint8_t const a = get_register8(REG_A);
        set_flag(FL_C, detect_borrow(a, n8, 8));
        set_flag(FL_H, detect_borrow(a, n8, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == n8);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b10111110) {  // CP (HL)
        DEBUG(1, "CP A, (HL)\n");
        uint8_t const a = get_register8(REG_A);
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_flag(FL_C, detect_borrow(a, hl_read, 8));
        set_flag(FL_H, detect_borrow(a, hl_read, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, a == hl_read);
        cycles_to_wait += 1;
        pc += 1;
    } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b100) {  // INC r
        DEBUG(1, "INC " << stringify_reg(r8_1) << "\n");
        uint8_t const r_val = get_register8(r8_1);
        set_register8(r8_1, r_val + 1);
        set_flag(FL_H, detect_carry(r_val, 1, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r8_1) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00110100) {  // INC (HL)
        DEBUG(1, "INC (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(hl);
        write_mem8(hl, hl_read + 1);
        set_flag(FL_H, detect_carry(hl_read, 1, 3));
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 3;
        pc += 1;
    } else if (top_two == 0b00 && r8_1 != DUMMY && bottom_three == 0b101) {  // DEC r
        DEBUG(1, "DEC " << stringify_reg(r8_1) << "\n");
        uint8_t const prev_r_val = get_register8(r8_1);
        set_register8(r8_1, prev_r_val - 1);
        set_flag(FL_H, detect_borrow(prev_r_val, 1, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, get_register8(r8_1) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00110101) {  // DEC (HL)
        DEBUG(1, "DEC (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(hl);
        write_mem8(hl, hl_read - 1);
        set_flag(FL_H, detect_borrow(hl_read, 1, 4));
        set_flag(FL_N, 1);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b00001001) {  // ADD HL, ss
        DEBUG(1, "ADD HL, " << stringify_dd_doublereg(doublereg_bits) << "\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint16_t const ss = get_doublereg(s1, s2);
        set_doublereg(REG_H, REG_L, hl + ss);
        set_flag(FL_C, detect_carry(hl, ss, 15));
        set_flag(FL_H, detect_carry(hl, ss, 11));
        set_flag(FL_N, 0);
        // For some reason, this doesn't touch the Z flag
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b11101000) {  // ADD SP, e
        DEBUG(1, "ADD SP, e\n");
        DEBUG(1, "UNIMPLEMENTED INSTRUCTION\n");
        exit(1);
    } else if ((instruction & 0b11001111) == 0b00000011) {  // INC ss
        DEBUG(1, "INC " << stringify_dd_doublereg(doublereg_bits) << "\n");
        set_doublereg(s1, s2, get_doublereg(s1, s2) + 1);
        // It's weird, but these instructions don't touch the flags
        cycles_to_wait += 2;
        pc += 1;
    } else if ((instruction & 0b11001111) == 0b00001011) {  // DEC ss
        DEBUG(1, "DEC " << stringify_dd_doublereg(doublereg_bits) << "\n");
        set_doublereg(s1, s2, get_doublereg(s1, s2) - 1);
        // It's weird, but these instructions don't touch the flags
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b00000111) {  // RLCA
        DEBUG(1, "RLCA\n");
        bool const lost_bit = get_register8(REG_A) >> 7;
        set_register8(REG_A, (get_register8(REG_A) << 1) | lost_bit);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00010111) {  // RLA
        DEBUG(1, "RLA\n");
        bool const lost_bit = get_register8(REG_A) >> 7;
        bool const old_fl_c = get_flag(FL_C);
        set_register8(REG_A, (get_register8(REG_A) << 1) | old_fl_c);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00001111) {  // RRCA
        DEBUG(1, "RRCA\n");
        bool const lost_bit = get_register8(REG_A) & 0b1;
        set_register8(REG_A, (get_register8(REG_A) >> 1) + (lost_bit << 7));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00011111) {  // RRA
        DEBUG(1, "RRA\n");
        bool const lost_bit = get_register8(REG_A) & 0b1;
        bool const old_fl_c = get_flag(FL_C);
        set_register8(REG_A, (old_fl_c << 7) | (get_register8(REG_A) >> 1));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00000 && (n8 & 0b111) != DUMMY) {  // RLC r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "RLC " << stringify_reg(r) << "\n");
        bool const lost_bit = get_register8(r) >> 7;
        set_register8(r, (get_register8(r) << 1) | lost_bit);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && n8 == 0b00000110) {  // RLC (HL)
        DEBUG(1, "RLC (HL)\n");
        auto const hl = get_doublereg(REG_H, REG_L);
        bool const lost_bit = read_mem8(hl) >> 7;
        write_mem8(hl, (read_mem8(hl) << 1) | lost_bit);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00010 && (n8 & 0b111) != DUMMY) {  // RL r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "RL " << stringify_reg(r) << "\n");
        bool const old_fl_c = get_flag(FL_C);
        bool const lost_bit = get_register8(r) >> 7;
        set_register8(r, (get_register8(r) << 1) | old_fl_c);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r));
        cycles_to_wait += 2;
        pc += 1;
    } else if (instruction == 0b11001011 && n8 == 0b00010110) {  // RL (HL)
        DEBUG(1, "RL (HL)\n");
        auto const hl = get_doublereg(REG_H, REG_L);
        bool const old_fl_c = get_flag(FL_C);
        bool const lost_bit = read_mem8(hl) >> 7;
        write_mem8(hl, (read_mem8(hl) << 1) | old_fl_c);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00001 && (n8 & 0b111) != DUMMY) {  // RRC r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "RRC " << stringify_reg(r) << "\n");
        bool const lost_bit = get_register8(r) & 0b1;
        set_register8(r, (lost_bit << 7) | (get_register8(r) >> 1));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && n8 == 0b00001110) {  // RRC (HL)
        DEBUG(1, "RRC (HL)\n");
        auto const hl = get_doublereg(REG_H, REG_L);
        bool const lost_bit = read_mem8(hl) & 0b1;
        write_mem8(hl, (lost_bit << 7) | (read_mem8(hl) >> 1));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00011 && (n8 & 0b111) != DUMMY) {  // RR r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "RR " << r << "\n");
        bool const lost_bit = get_register8(r) & 0b1;
        bool const old_fl_c = get_flag(FL_C);
        set_register8(r, (old_fl_c << 7) | (get_register8(r) >> 1));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r));
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11011011 && n8 == 0b00011110) {  // RR (HL)
        DEBUG(1, "RR (HL)\n");
        auto const hl = get_doublereg(REG_H, REG_L);
        bool const lost_bit = read_mem8(hl) & 0b1;
        bool const old_fl_c = get_flag(FL_C);
        write_mem8(hl, (old_fl_c << 7) | (read_mem8(hl) >> 1));
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00100 && (n8 & 0b111) != DUMMY) {  // SLA r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "SLA " << stringify_reg(r) << "\n");
        set_flag(FL_C, get_register8(r) >> 7);
        set_register8(r, get_register8(r) << 1);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11011011 && n8 == 0b00100110) {  // SLA (HL)
        DEBUG(1, "SLA (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_flag(FL_C, hl_read >> 7);
        write_mem8(hl, hl_read << 1);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(get_doublereg(REG_H, REG_L)) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00101 && (n8 & 0b111) != DUMMY) {  // SRA r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "SRA " << stringify_reg(r) << "\n");
        set_flag(FL_C, get_register8(r) & 0b1);
        set_register8(r, (get_register8(r) & 0b10000000) | (get_register8(r) >> 1));
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && n8 == 0b00101110) {  // SRA (HL)
        DEBUG(1, "SRA (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        set_flag(FL_C, hl_read & 0b1);
        write_mem8(hl, (hl_read & 0b10000000) | (hl_read >> 1));
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(get_doublereg(REG_H, REG_L)) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00111 && (n8 & 0b111) != DUMMY) {  // SRL r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "SRL " << stringify_reg(r) << "\n");
        bool const lost_bit = get_register8(r) & 0b1;
        set_register8(r, get_register8(r) >> 1);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, get_register8(r) == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && n8 == 0b00111110) {  // SRL (HL)
        DEBUG(1, "SRL (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(hl);
        bool const lost_bit = hl_read & 0b1;
        write_mem8(hl, hl_read >> 1);
        set_flag(FL_C, lost_bit);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, read_mem8(hl) == 0);
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 3) == 0b00110 && (n8 & 0b111) != DUMMY) {  // SWAP r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "SWAP " << stringify_reg(r) << "\n");
        uint8_t const r_val = get_register8(r);
        set_register8(r, (r_val << 4) | (r_val >> 4));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, r_val == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && n8 == 0b00110110) {  // SWAP (HL)
        DEBUG(1, "SWAP (HL)\n");
        uint16_t const hl = get_doublereg(REG_H, REG_L);
        uint8_t const hl_read = read_mem8(hl);
        write_mem8(hl, (hl_read << 4) | (hl_read >> 4));
        set_flag(FL_C, 0);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        set_flag(FL_Z, hl_read == 0);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b01 && (n8 & 0b111) != DUMMY) {  // BIT b, r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "BIT b, " << stringify_reg(r) << "\n");
        uint8_t const b = (n8 >> 3) & 0b111;
        set_flag(FL_H, 1);
        set_flag(FL_N, 0);
        set_flag(FL_Z, (~get_register8(r) & (1 << b)) >> b);
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b01 && (n8 & 0b111) == DUMMY) {  // BIT b, (HL)
        DEBUG(1, "BIT b, (HL)\n");
        uint8_t const hl_read = read_mem8(get_doublereg(REG_H, REG_L));
        uint8_t const b = (n8 >> 3) & 0b111;
        set_flag(FL_H, 1);
        set_flag(FL_N, 0);
        set_flag(FL_Z, (~hl_read & (1 << b)) >> b);
        cycles_to_wait += 3;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b11 && (n8 & 0b111) != DUMMY) {  // SET b, r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "SET b, " << stringify_reg(r) << "\n");
        uint8_t const b = (n8 >> 3) & 0b111;
        set_register8(r, get_register8(r) | (1 << b));
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b11 && (n8 & 0b111) == DUMMY) {  // SET b, (HL)
        DEBUG(1, "SET b, (HL)\n");
        uint8_t const b = (n8 >> 3) & 0b111;
        auto hl = get_doublereg(REG_H, REG_L);
        auto hl_read = read_mem8(hl);
        write_mem8(hl, hl_read | (1 << b));
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b10 && (n8 & 0b111) != DUMMY) {  // RES b, r
        Register8 const r = (Register8)(n8 & 0b111);
        DEBUG(1, "RES b, " << stringify_reg(r) << "\n");
        uint8_t const b = (n8 >> 3) & 0b111;
        set_register8(r, get_register8(r) & ~(1 << b));
        cycles_to_wait += 2;
        pc += 2;
    } else if (instruction == 0b11001011 && (n8 >> 6) == 0b10 && (n8 & 0b111) == DUMMY) {  // RES b, (HL)
        DEBUG(1, "RES b, (HL)\n");
        uint8_t const b = (n8 >> 3) & 0b111;
        auto hl = get_doublereg(REG_H, REG_L);
        auto hl_read = read_mem8(hl);
        write_mem8(hl, hl_read & ~(1 << b));
        cycles_to_wait += 4;
        pc += 2;
    } else if (instruction == 0b11000011) {  // JP nn
        DEBUG(1, "JP 0x" << std::hex << (int64_t)n16 << std::dec << "\n");
        pc = n16;
        cycles_to_wait += 4;
        // Don't increment pc here.
    } else if ((instruction & 0b11100111) == 0b11000010) {  // JP cc, nn
        DEBUG(1, "JP " << stringify_cc_condition(cc) << ", 0x" << std::hex << n16 << std::dec << "\n");
        bool jumping = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (jumping) {
            pc = n16;
            cycles_to_wait += 4;
        } else {
            cycles_to_wait += 3;
            pc += 3;
        }
    } else if (instruction == 0b00011000) {  // JR e
        DEBUG(1, "JR 0x" << std::hex << (int32_t)pc + e8 + 2 << std::dec << "\n");
        pc += 2;  // because e is actually stored as (e-2) for this instruction
        int32_t pc_signed_32 = pc;
        pc_signed_32 += e8;
        pc = pc_signed_32;  // We trust that the assembler didn't let pc go negative here.
        cycles_to_wait += 3;
        // Don't increment pc here
    } else if ((instruction & 0b11100111) == 0b00100000) {  // JR cc, e
        DEBUG(1, "JR " << stringify_cc_condition(cc) << ", "
                       << "0x" << std::hex << (int32_t)pc + e8 + 2 << std::dec << "\n");
        bool jumping = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (jumping) {
            pc += 2;  // because e is actually stored as (e-2) for this instruction
            int32_t pc_signed_32 = pc;
            pc_signed_32 += e8;
            pc = pc_signed_32;  // We trust that the assembler didn't let pc go negative here.
            cycles_to_wait += 3;
            // Don't increment pc here
        } else {
            cycles_to_wait += 2;
            pc += 2;
        }
    } else if (instruction == 0b11101001) {  // JP (HL)
        DEBUG(1, "JP (HL)\n");
        pc = get_doublereg(REG_H, REG_L);
        cycles_to_wait += 1;
        // Don't increment pc here
    } else if (instruction == 0b11001101) {  // CALL nn
        DEBUG(1, "CALL 0x" << std::hex << n16 << std::dec << "\n");
        uint16_t const sp = get_doublereg(REG_S, REG_P);
        uint16_t const old_pc = pc + 3;
        write_mem16(sp - 2, old_pc);
        pc = n16;
        set_doublereg(REG_S, REG_P, sp - 2);
        cycles_to_wait += 6;
        // Don't increment pc here
    } else if ((instruction & 0b11100111) == 0b11000100) {  // CALL cc, nn
        DEBUG(1, "CALL " << stringify_cc_condition(cc) << ", 0x" << std::hex << (int64_t)n16 << std::dec << "\n");
        bool calling = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (calling) {
            uint16_t const sp = get_doublereg(REG_S, REG_P);
            uint16_t const old_pc = pc + 3;
            write_mem16(sp - 2, old_pc);
            pc = n16;
            set_doublereg(REG_S, REG_P, sp - 2);
            cycles_to_wait += 6;
            // Don't increment pc here
        } else {
            cycles_to_wait += 3;
            pc += 3;
        }
    } else if (instruction == 0b11001001) {  // RET
        DEBUG(1, "RET\n");
        auto const sp = get_doublereg(REG_S, REG_P);
        pc = (read_mem8(sp + 1) << 8) | read_mem8(sp);
        set_doublereg(REG_S, REG_P, sp + 2);
        cycles_to_wait += 4;
        // Don't increment pc here
    } else if (instruction == 0b11011001) {  // RETI
        DEBUG(1, "IRET\n");
        ime = 1;
        auto const sp = get_doublereg(REG_S, REG_P);
        pc = read_mem16(sp);
        set_doublereg(REG_S, REG_P, sp + 2);
        cycles_to_wait += 4;
        // Don't increment pc here
        handle_interrupts();
    } else if ((instruction & 0b11100111) == 0b11000000) {  // RET cc
        DEBUG(1, "RET " << stringify_cc_condition(cc) << "\n");
        bool retting = (cc == 0b00 && get_flag(FL_Z) == 0) || (cc == 0b01 && get_flag(FL_Z) == 1) ||
                       (cc == 0b10 && get_flag(FL_C) == 0) || (cc == 0b11 && get_flag(FL_C) == 1);
        if (retting) {
            uint16_t const sp = get_doublereg(REG_S, REG_P);
            pc = read_mem16(sp);
            set_doublereg(REG_S, REG_P, sp + 2);
            cycles_to_wait += 5;
            // Don't increment pc here
        } else {
            cycles_to_wait += 2;
            pc += 1;
        }
    } else if (top_two == 0b11 && bottom_three == 0b111) {  // RST t
        DEBUG(1, "RST t\n");
        uint16_t const sp = get_doublereg(REG_S, REG_P);
        write_mem16(sp - 2, pc + 1);
        set_doublereg(REG_S, REG_P, get_doublereg(REG_S, REG_P) - 2);
        pc = 8 * r8_1;
        cycles_to_wait += 4;
        pc += 1;
    } else if (instruction == 0b00100111) {  // DAA
        bool const c_contents = get_flag(FL_C);
        bool const h_contents = get_flag(FL_H);
        bool const n_contents = get_flag(FL_N);
        uint8_t const a_contents = get_register8(REG_A);
        uint8_t const a_hi = a_contents >> 4;
        uint8_t const a_lo = a_contents % (1 << 4);

        int addend = -1;
        bool carry = false;
        if (n_contents) {  // subtraction. The invalid states are not detected for subtraction.
            if (!c_contents && !h_contents) {
                addend = 0x00;
                carry = false;
            } else if (!c_contents && h_contents) {
                addend = 0xFA;
                carry = false;
            } else if (c_contents && !h_contents) {
                addend = 0xA0;
                carry = true;
            } else {
                addend = 0x9A;
                carry = true;
            }
        } else {  // addition
            if (!c_contents && !h_contents && a_hi <= 0x9 && a_lo <= 0x9) {
                addend = 0x00;
                carry = false;
            } else if (!c_contents && !h_contents && a_hi <= 0x8 && a_lo >= 0xA) {
                addend = 0x06;
                carry = false;
            } else if (!c_contents && h_contents && a_hi <= 0x9 && a_lo <= 0x3) {
                addend = 0x06;
                carry = false;
            } else if (!c_contents && !h_contents && a_hi >= 0xA && a_lo <= 0x9) {
                addend = 0x60;
                carry = true;
            } else if (!c_contents && !h_contents && a_hi >= 0x9 && a_lo >= 0xA) {
                addend = 0x66;
                carry = true;
            } else if (!c_contents && h_contents && a_hi >= 0xA && a_lo <= 0x3) {
                addend = 0x66;
                carry = true;
            } else if (c_contents && !h_contents && a_hi <= 0x2 && a_lo <= 0x9) {
                addend = 0x60;
                carry = true;
            } else if (c_contents && !h_contents && a_hi <= 0x2 && a_lo >= 0xA) {
                addend = 0x66;
                carry = true;
            } else if (c_contents && h_contents && a_hi <= 0x3 && a_lo <= 0x3) {
                addend = 0x66;
                carry = true;
            } else {
                std::cerr << "Invalid state for DAA." << std::endl;
                // exit(1);
            }
        }
        set_flag(FL_C, carry);
        set_flag(FL_H, 0);
        set_register8(REG_A, a_contents + addend);
        set_flag(FL_Z, get_register8(REG_A) == 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00101111) {  // CPL
        DEBUG(1, "CPL\n");
        set_register8(REG_A, ~get_register8(REG_A));
        set_flag(FL_H, 1);
        set_flag(FL_N, 1);
        pc += 1;
    } else if (instruction == 0b00000000) {  // NOP
        DEBUG(1, "NOP\n");
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00111111) {  // CCF
        DEBUG(1, "CCF\n");
        set_flag(FL_C, !get_flag(FL_C));
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b00110111) {  // SCF
        DEBUG(1, "SCF\n");
        set_flag(FL_C, 1);
        set_flag(FL_H, 0);
        set_flag(FL_N, 0);
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11110011) {  // DI
        DEBUG(1, "DI\n");
        ime = 0;
        cycles_to_wait += 1;
        pc += 1;
    } else if (instruction == 0b11111011) {  // EI
        // BUG:
        // The effect of EI should actually be delayed by one cycle (so EI DI should not allow any interrupts)
        DEBUG(1, "EI\n");
        ime = 1;
        cycles_to_wait += 1;
        pc += 1;
        handle_interrupts();
    } else if (instruction == 0b01110110) {  // HALT
        DEBUG(1, "HALT\n");
        cycles_to_wait += 1;
        pc += 1;  // There is a bug in the DMG s.t. making this a 2 would be more accurate.
                  // Usually, people stick NOPs after HALTs for good measure, so it shouldn't be a problem if we don't
                  // reproduce the bug.
        halted = true;
    } else if (instruction == 0b00010000) {  // STOP
        DEBUG(1, "STOP\n");
        if (n8 != 0) {
            DEBUG(1, "This is a corrupted STOP.\n");
        }
        cycles_to_wait += 1;
        pc += 2;
        halted = true;
    } else {
        DEBUG(1, "Invalid instruction: 0b" << std::bitset<8>{instruction} << "\n");
        pc += 1;
    }

    return 0;
}

/* TO DO:
    ADD SP, e
    LDHL SP, e
    MBCs
    window and sprite layers
*/