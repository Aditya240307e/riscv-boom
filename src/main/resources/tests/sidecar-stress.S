# sidecar-stress.S
.section .text
.global _start

_start:
    li t0, 10
    li t1, 20

    # Execute 20 VETO instructions
    .rept 20
    .word 0x0062838b   # VETO t2, t0, t1
    .endr

    # Exit
    li a0, 0
    li a7, 93
    ecall
