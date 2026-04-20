#include <stdint.h>
#include <stdio.h>

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

void __attribute__((noinline)) benchmark_moe_baseline(int32_t* weights, int32_t* data, int32_t threshold, int iterations) {
    
    for (int i = 0; i < iterations; i++) {
        int32_t w = *((volatile int32_t*)&weights[weights[i] % 1024]); 
        int32_t gate = w; 
        int32_t payload = data[i];
        asm volatile (
            "addi %0, %0, 291\n\t"
            "xori %0, %0, 1110\n\t"
            "slli %0, %0, 1\n\t"
            "srli %0, %0, 1\n\t"
            : "+r"(payload)
        );
        data[i] = payload;

        if (gate > threshold) {
            data[i] += 1; 
        }
    }
}

int main() {
    int iterations = 500;
    int32_t weights[1024];
    int32_t data[1024];
    
    for (int i = 0; i < 1024; i++) {
        weights[i] = (i * 7) % 1024;
        data[i] = i;
    }

    printf("--- Baseline BOOM Characterization ---\n");

    uint64_t start_cycles = read_cycles();
    uint64_t start_insts  = read_instret();

    benchmark_moe_baseline(weights, data, 100, iterations);

    uint64_t end_cycles = read_cycles();
    uint64_t end_insts  = read_instret();

    uint64_t delta_cycles = end_cycles - start_cycles;
    uint64_t delta_insts  = end_insts - start_insts;
    float ipc = (float)delta_insts / (float)delta_cycles;

    printf("Total Cycles:   %lu\n", delta_cycles);
    printf("Total Insts:    %lu\n", delta_insts);
    printf("Baseline IPC:   %.4f\n", ipc);

    return 0;
}
