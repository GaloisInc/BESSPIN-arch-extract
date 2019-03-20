; The init-tests file contains a sequence of Racket lists, with each list
; representing a single test input.  The synthesizer will pass each input to
; the oracle, and use the resulting test cases to initialize its search.  This
; file can contain both passing and failing test inputs (and does not need to
; indicate which are which), though passing tests are normally more useful than
; failing ones.

; The default SWERV configuration
(RV_BTB_BTAG_FOLD RV_BUILD_AXI4 RV_DCCM_ENABLE RV_ICACHE_ENABLE VERILATOR)

; Enabling additional DCCM/ICCM/ICACHE features
(RV_BTB_BTAG_FOLD RV_BUILD_AXI4 RV_DCCM_ENABLE
    RV_ICACHE_ECC RV_ICACHE_ENABLE RV_ICCM_ENABLE VERILATOR)
; Disabling all DCCM/ICCM/ICACHE features
(RV_BTB_BTAG_FOLD RV_BUILD_AXI4 VERILATOR)

