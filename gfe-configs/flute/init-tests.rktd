; The default configuration for Flute RV64ACFDIMSU (SSITH P2)
(
 RV64 SV39
 ; The default config also includes unused flags ISA_PRIV_M and ISA_I
 ISA_PRIV_U ISA_PRIV_S
 ISA_M ISA_A ISA_F ISA_D ISA_FD_DIV ISA_C
 SHIFT_BARREL
 MULT_SYNTH
 Near_Mem_Caches
 FABRIC64
)

(
 RV32
 ; The default config also includes unused flags ISA_PRIV_M and ISA_I
 ISA_PRIV_U
 ISA_M ISA_A ISA_C
 SHIFT_BARREL
 MULT_SYNTH
 Near_Mem_Caches
 FABRIC64
)
