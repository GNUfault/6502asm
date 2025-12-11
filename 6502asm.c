#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

typedef struct {
    char name[32];
    uint16_t addr;
} Label;

typedef struct {
    const char *mnem;
    uint8_t opcode;
    const char *mode;
} Opcode;

Label *labels = NULL;
int label_count = 0;
int label_capacity = 0;
uint8_t *code = NULL;
int code_capacity = 0;
uint16_t pc = 0;

Opcode opcodes[] = {
    {"ADC", 0x69, "IMM"}, {"ADC", 0x65, "ZP"}, {"ADC", 0x75, "ZPX"},
    {"ADC", 0x6D, "ABS"}, {"ADC", 0x7D, "ABX"}, {"ADC", 0x79, "ABY"},
    {"ADC", 0x61, "INX"}, {"ADC", 0x71, "INY"},
    {"AND", 0x29, "IMM"}, {"AND", 0x25, "ZP"}, {"AND", 0x35, "ZPX"},
    {"AND", 0x2D, "ABS"}, {"AND", 0x3D, "ABX"}, {"AND", 0x39, "ABY"},
    {"AND", 0x21, "INX"}, {"AND", 0x31, "INY"},
    {"ASL", 0x0A, "ACC"}, {"ASL", 0x06, "ZP"}, {"ASL", 0x16, "ZPX"},
    {"ASL", 0x0E, "ABS"}, {"ASL", 0x1E, "ABX"},
    {"BCC", 0x90, "REL"}, {"BCS", 0xB0, "REL"}, {"BEQ", 0xF0, "REL"},
    {"BMI", 0x30, "REL"}, {"BNE", 0xD0, "REL"}, {"BPL", 0x10, "REL"},
    {"BVC", 0x50, "REL"}, {"BVS", 0x70, "REL"},
    {"BIT", 0x24, "ZP"}, {"BIT", 0x2C, "ABS"},
    {"BRK", 0x00, "IMP"},
    {"CLC", 0x18, "IMP"}, {"CLD", 0xD8, "IMP"}, 
    {"CLI", 0x58, "IMP"}, {"CLV", 0xB8, "IMP"},
    {"CMP", 0xC9, "IMM"}, {"CMP", 0xC5, "ZP"}, {"CMP", 0xD5, "ZPX"},
    {"CMP", 0xCD, "ABS"}, {"CMP", 0xDD, "ABX"}, {"CMP", 0xD9, "ABY"},
    {"CMP", 0xC1, "INX"}, {"CMP", 0xD1, "INY"},
    {"CPX", 0xE0, "IMM"}, {"CPX", 0xE4, "ZP"}, {"CPX", 0xEC, "ABS"},
    {"CPY", 0xC0, "IMM"}, {"CPY", 0xC4, "ZP"}, {"CPY", 0xCC, "ABS"},
    {"DEC", 0xC6, "ZP"}, {"DEC", 0xD6, "ZPX"},
    {"DEC", 0xCE, "ABS"}, {"DEC", 0xDE, "ABX"},
    {"DEX", 0xCA, "IMP"}, {"DEY", 0x88, "IMP"},
    {"EOR", 0x49, "IMM"}, {"EOR", 0x45, "ZP"}, {"EOR", 0x55, "ZPX"},
    {"EOR", 0x4D, "ABS"}, {"EOR", 0x5D, "ABX"}, {"EOR", 0x59, "ABY"},
    {"EOR", 0x41, "INX"}, {"EOR", 0x51, "INY"},
    {"INC", 0xE6, "ZP"}, {"INC", 0xF6, "ZPX"},
    {"INC", 0xEE, "ABS"}, {"INC", 0xFE, "ABX"},
    {"INX", 0xE8, "IMP"}, {"INY", 0xC8, "IMP"},
    {"JMP", 0x4C, "ABS"}, {"JMP", 0x6C, "IND"},
    {"JSR", 0x20, "ABS"},
    {"LDA", 0xA9, "IMM"}, {"LDA", 0xA5, "ZP"}, {"LDA", 0xB5, "ZPX"},
    {"LDA", 0xAD, "ABS"}, {"LDA", 0xBD, "ABX"}, {"LDA", 0xB9, "ABY"},
    {"LDA", 0xA1, "INX"}, {"LDA", 0xB1, "INY"},
    {"LDX", 0xA2, "IMM"}, {"LDX", 0xA6, "ZP"}, {"LDX", 0xB6, "ZPY"},
    {"LDX", 0xAE, "ABS"}, {"LDX", 0xBE, "ABY"},
    {"LDY", 0xA0, "IMM"}, {"LDY", 0xA4, "ZP"}, {"LDY", 0xB4, "ZPX"},
    {"LDY", 0xAC, "ABS"}, {"LDY", 0xBC, "ABX"},
    {"LSR", 0x4A, "ACC"}, {"LSR", 0x46, "ZP"}, {"LSR", 0x56, "ZPX"},
    {"LSR", 0x4E, "ABS"}, {"LSR", 0x5E, "ABX"},
    {"NOP", 0xEA, "IMP"},
    {"ORA", 0x09, "IMM"}, {"ORA", 0x05, "ZP"}, {"ORA", 0x15, "ZPX"},
    {"ORA", 0x0D, "ABS"}, {"ORA", 0x1D, "ABX"}, {"ORA", 0x19, "ABY"},
    {"ORA", 0x01, "INX"}, {"ORA", 0x11, "INY"},
    {"PHA", 0x48, "IMP"}, {"PHP", 0x08, "IMP"},
    {"PLA", 0x68, "IMP"}, {"PLP", 0x28, "IMP"},
    {"ROL", 0x2A, "ACC"}, {"ROL", 0x26, "ZP"}, {"ROL", 0x36, "ZPX"},
    {"ROL", 0x2E, "ABS"}, {"ROL", 0x3E, "ABX"},
    {"ROR", 0x6A, "ACC"}, {"ROR", 0x66, "ZP"}, {"ROR", 0x76, "ZPX"},
    {"ROR", 0x6E, "ABS"}, {"ROR", 0x7E, "ABX"},
    {"RTI", 0x40, "IMP"}, {"RTS", 0x60, "IMP"},
    {"SBC", 0xE9, "IMM"}, {"SBC", 0xE5, "ZP"}, {"SBC", 0xF5, "ZPX"},
    {"SBC", 0xED, "ABS"}, {"SBC", 0xFD, "ABX"}, {"SBC", 0xF9, "ABY"},
    {"SBC", 0xE1, "INX"}, {"SBC", 0xF1, "INY"},
    {"SEC", 0x38, "IMP"}, {"SED", 0xF8, "IMP"}, {"SEI", 0x78, "IMP"},
    {"STA", 0x85, "ZP"}, {"STA", 0x95, "ZPX"},
    {"STA", 0x8D, "ABS"}, {"STA", 0x9D, "ABX"}, {"STA", 0x99, "ABY"},
    {"STA", 0x81, "INX"}, {"STA", 0x91, "INY"},
    {"STX", 0x86, "ZP"}, {"STX", 0x96, "ZPY"}, {"STX", 0x8E, "ABS"},
    {"STY", 0x84, "ZP"}, {"STY", 0x94, "ZPX"}, {"STY", 0x8C, "ABS"},
    {"TAX", 0xAA, "IMP"}, {"TAY", 0xA8, "IMP"}, {"TSX", 0xBA, "IMP"},
    {"TXA", 0x8A, "IMP"}, {"TXS", 0x9A, "IMP"}, {"TYA", 0x98, "IMP"},
    {NULL, 0, NULL}
};

void add_label(const char *name, uint16_t addr) {
    if (label_count >= label_capacity) {
        label_capacity = label_capacity == 0 ? 64 : label_capacity * 2;
        labels = realloc(labels, label_capacity * sizeof(Label));
    }
    strcpy(labels[label_count].name, name);
    labels[label_count].addr = addr;
    label_count++;
}

int find_label(const char *name) {
    int i;
    for (i = 0; i < label_count; i++) {
        if (strcmp(labels[i].name, name) == 0)
            return labels[i].addr;
    }
    return -1;
}

void emit_byte(uint8_t byte) {
    if (pc >= code_capacity) {
        code_capacity = code_capacity == 0 ? 1024 : code_capacity * 2;
        code = realloc(code, code_capacity);
    }
    code[pc++] = byte;
}

void emit_word(uint16_t word) {
    emit_byte(word & 0xFF);
    emit_byte((word >> 8) & 0xFF);
}

int parse_number(const char *str) {
    if (str[0] == '$')
        return strtol(str + 1, NULL, 16);
    if (str[0] == '%')
        return strtol(str + 1, NULL, 2);
    return atoi(str);
}

void assemble_line(char *line, int pass) {
    char mnem[16], operand[256];
    char *p = line;
    int i;
    
    while (*p && isspace(*p)) p++;
    if (!*p || *p == ';') return;
    
    if (*p && !isspace(*p) && strchr(p, ':')) {
        char label[32];
        sscanf(p, "%[^:]:", label);
        if (pass == 1) add_label(label, pc);
        p = strchr(p, ':') + 1;
        while (*p && isspace(*p)) p++;
        if (!*p || *p == ';') return;
    }
    
    sscanf(p, "%s", mnem);
    for (i = 0; mnem[i]; i++) mnem[i] = toupper(mnem[i]);
    
    p += strlen(mnem);
    while (*p && isspace(*p)) p++;
    
    operand[0] = '\0';
    if (*p && *p != ';') {
        char *end = strchr(p, ';');
        if (end) *end = '\0';
        strcpy(operand, p);
        int len = strlen(operand) - 1;
        while (len >= 0 && isspace(operand[len])) operand[len--] = '\0';
    }
    
    const char *mode = "IMP";
    int value = 0;
    
    if (strlen(operand) == 0) {
        mode = "IMP";
    } else if (operand[0] == 'A' && operand[1] == '\0') {
        mode = "ACC";
    } else if (operand[0] == '#') {
        mode = "IMM";
        if (pass == 2) {
            if (operand[1] == '<') value = parse_number(operand + 2) & 0xFF;
            else if (operand[1] == '>') value = (parse_number(operand + 2) >> 8) & 0xFF;
            else value = parse_number(operand + 1);
        }
    } else if (operand[0] == '(') {
        if (strstr(operand, ",X)")) {
            mode = "INX";
            operand[strchr(operand, ',') - operand] = '\0';
            if (pass == 2) value = parse_number(operand + 1);
        } else if (strstr(operand, "),Y")) {
            mode = "INY";
            operand[strchr(operand, ')') - operand] = '\0';
            if (pass == 2) value = parse_number(operand + 1);
        } else {
            mode = "IND";
            operand[strlen(operand) - 1] = '\0';
            if (pass == 2) {
                int addr = find_label(operand + 1);
                value = (addr >= 0) ? addr : parse_number(operand + 1);
            }
        }
    } else if (strstr(operand, ",X")) {
        operand[strchr(operand, ',') - operand] = '\0';
        if (pass == 2) {
            int addr = find_label(operand);
            value = (addr >= 0) ? addr : parse_number(operand);
        }
        mode = (value < 256) ? "ZPX" : "ABX";
    } else if (strstr(operand, ",Y")) {
        operand[strchr(operand, ',') - operand] = '\0';
        if (pass == 2) {
            int addr = find_label(operand);
            value = (addr >= 0) ? addr : parse_number(operand);
        }
        mode = (value < 256) ? "ZPY" : "ABY";
    } else {
        if (pass == 2) {
            int addr = find_label(operand);
            value = (addr >= 0) ? addr : parse_number(operand);
        }
        int is_branch = (strcmp(mnem, "BCC") == 0 || strcmp(mnem, "BCS") == 0 ||
                        strcmp(mnem, "BEQ") == 0 || strcmp(mnem, "BMI") == 0 ||
                        strcmp(mnem, "BNE") == 0 || strcmp(mnem, "BPL") == 0 ||
                        strcmp(mnem, "BVC") == 0 || strcmp(mnem, "BVS") == 0);
        if (is_branch) {
            mode = "REL";
            if (pass == 2) value = value - (pc + 2);
        } else {
            mode = (value < 256) ? "ZP" : "ABS";
        }
    }
    
    for (i = 0; opcodes[i].mnem; i++) {
        if (strcmp(opcodes[i].mnem, mnem) == 0 && 
            strcmp(opcodes[i].mode, mode) == 0) {
            if (pass == 2) {
                emit_byte(opcodes[i].opcode);
                if (strcmp(mode, "IMM") == 0 || strcmp(mode, "ZP") == 0 ||
                    strcmp(mode, "ZPX") == 0 || strcmp(mode, "ZPY") == 0 ||
                    strcmp(mode, "INX") == 0 || strcmp(mode, "INY") == 0 ||
                    strcmp(mode, "REL") == 0) {
                    emit_byte(value & 0xFF);
                } else if (strcmp(mode, "ABS") == 0 || strcmp(mode, "ABX") == 0 ||
                          strcmp(mode, "ABY") == 0 || strcmp(mode, "IND") == 0) {
                    emit_word(value);
                }
            } else {
                pc++;
                if (strcmp(mode, "IMM") == 0 || strcmp(mode, "ZP") == 0 ||
                    strcmp(mode, "ZPX") == 0 || strcmp(mode, "ZPY") == 0 ||
                    strcmp(mode, "INX") == 0 || strcmp(mode, "INY") == 0 ||
                    strcmp(mode, "REL") == 0) {
                    pc++;
                } else if (strcmp(mode, "ABS") == 0 || strcmp(mode, "ABX") == 0 ||
                          strcmp(mode, "ABY") == 0 || strcmp(mode, "IND") == 0) {
                    pc += 2;
                }
            }
            return;
        }
    }
    
    if (pass == 2) {
        fprintf(stderr, "Error: Unknown instruction or addressing mode: %s %s\n", mnem, operand);
    }
}

int main(int argc, char **argv) {
    FILE *in, *out;
    char line[1024];
    int i;
    
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <input.asm> <output.bin>\n", argv[0]);
        return 1;
    }
    
    in = fopen(argv[1], "r");
    if (!in) {
        fprintf(stderr, "Error: Cannot open input file\n");
        return 1;
    }
    
    pc = 0;
    while (fgets(line, sizeof(line), in)) {
        assemble_line(line, 1);
    }
    
    rewind(in);
    pc = 0;
    while (fgets(line, sizeof(line), in)) {
        assemble_line(line, 2);
    }
    fclose(in);
    
    out = fopen(argv[2], "wb");
    if (!out) {
        fprintf(stderr, "Error: Cannot open output file\n");
        free(labels);
        free(code);
        return 1;
    }
    
    fwrite(code, 1, pc, out);
    fclose(out);
    
    free(labels);
    free(code);
    return 0;
}
