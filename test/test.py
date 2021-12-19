import sys
import re
import subprocess

TEMP_ROM_FILE = "/tmp/testrom"
HEADER_OFFSET = 0x100
REG_CODES_8_BIT = {0b111: "reg_a", 0b000: "reg_b", 0b001: "reg_c", 0b101: "reg_d", 0b011: "reg_e", 0b100: "reg_h", 0b101: "reg_l"}
DEFAULT_STATE = {"reg_a": 0, "reg_f": 0, "reg_b": 0, "reg_c": 0, "reg_d": 0, "reg_e": 0, "reg_h": 0, "reg_l": 0, "reg_sp": 0, "reg_pc": 0x100, "fl_c": 0, "fl_h": 0, "fl_n": 0, "fl_z": 0}

def decode_dump(dump: str):
    m = re.match(r"^AF:[\s,]+(\w\w)(\w\w)[\s,]+BC:[\s,]+(\w\w)(\w\w)[\s,]+DE:[\s,]+(\w\w)(\w\w)[\s,]+HL:[\s,]+(\w\w)(\w\w),[\s,]+SP:[\s,]+(\w\w\w\w)[\s,]+PC:[\s,]+(\w\w\w\w)[\s,]+C:[\s,]+([01])[\s,]+H:[\s,]+([01])[\s,]+N:[\s,]+([01])[\s,]+Z:[\s,]+([01])", dump)
    if m is not None:
        return {tuple(DEFAULT_STATE.keys())[i]: int(m.groups()[i], 16) for i in range(len(m.groups()))}
    else:
        print("Can't decode dump:", dump, file=sys.stderr)
        sys.exit(1)

def create_temporary_rom(opcodes):
    f = open(TEMP_ROM_FILE, "wb")
    f.write(b"\x00" * HEADER_OFFSET + bytes(opcodes + [0b01110110])) # Tack on a HALT
    f.close()

def run_gbemu(rom_file = TEMP_ROM_FILE):
    output = subprocess.run(f"./gbemu --headless {rom_file}", shell=True, capture_output=True, encoding="UTF-8")
    print("stderr from gbemu:", repr(output.stderr))

    return output