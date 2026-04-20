#include <stdint.h>
#include <stdio.h>

/**
 * CUSTOM CSR & INSTRUCTION DEFINITIONS
 */
#define VETO_CSR 0x800

// Custom VETO Instruction: .insn r opcode, funct3, funct7, rd, rs1, rs2
// Opcode 0x0b is a standard custom opcode space in RISC-V
#define VETO(rd, rs1, rs2) \
  asm volatile (".insn r 0x0b, 0, 0, %0, %1, %2" : "=r"(rd) : "r"(rs1), "r"(rs2))

/**
    READ PERFORMANCE COUNTERS
 */
static inline uint64_t read_cycles() {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

static inline uint64_t read_instret() {
    uint64_t insts;
    asm volatile ("rdinstret %0" : "=r"(insts));
    return insts;
}

/**
 * THE MOE KERNEL
 * Designed to showcase Speculative Reclamation
 */
void __attribute__((noinline)) benchmark_moe(int32_t* weights, int32_t* data, int32_t threshold, int iterations) {
    //Enable the Speculative Reclamation Hardware
    asm volatile ("csrw %0, 1" : : "i"(VETO_CSR)); 

    for (int i = 0; i < iterations; i++) {
        int32_t w = *((volatile int32_t*)&weights[weights[i] % 1024]); 
        int32_t gate;

        // Marks 'gate' as tainted based on the result of the load 'w'
        VETO(gate, w, threshold);

        // We use ASM blocks to ensure the compiler doesn't optimize these away.
        // These 4 instructions should saturate the 2-wide issue window.
        int32_t payload = data[i];
        asm volatile (
            "addi %0, %0, 291\n\t"  // payload += 0x123
            "xori %0, %0, 1110\n\t" // payload ^= 0x456
            "slli %0, %0, 1\n\t"    // payload *= 2
            "srli %0, %0, 1\n\t"    // payload /= 2
            : "+r"(payload)
        );
        data[i] = payload;

        if (gate > 50) {
            data[i] += 1; 
        }
    }

    asm volatile ("csrw %0, 0" : : "i"(VETO_CSR));
}

int main() {
    int iterations = 500;
    int32_t weights[1024];
    int32_t data[1024];
    
    // Initialize dummy data
    for (int i = 0; i < 1024; i++) {
        weights[i] = (i * 7) % 1024;
        data[i] = i;
    }

    printf("--- Starting MoE Characterization ---\n");

    uint64_t start_cycles = read_cycles();
    uint64_t start_insts  = read_instret();

    benchmark_moe(weights, data, 100, iterations);

    uint64_t end_cycles = read_cycles();
    uint64_t end_insts  = read_instret();

    // PERFORMANCE CALCULATIONS
    uint64_t delta_cycles = end_cycles - start_cycles;
    uint64_t delta_insts  = end_insts - start_insts;
    float ipc = (float)delta_insts / (float)delta_cycles;

    printf("Total Cycles:   %lu\n", delta_cycles);
    printf("Total Insts:    %lu\n", delta_insts);
    printf("Achieved IPC:   %.4f\n", ipc);

    if (ipc >= 0.85) {
        printf("SUCCESS: Speculative Reclamation is operational (IPC > 0.85)\n");
    } else {
        printf("DEBUG: IPC is %.4f. Check VCD for Structural Hazards.\n", ipc);
    }

    return 0;
}
