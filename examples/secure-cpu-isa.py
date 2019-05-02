import sys

mode, = sys.argv[1:]

features = [
    'riscv',
    'intel',
    'arm',
    'rv32',
    'rv64',
    'rv128',
    'rv_m',
    'rv_a',
    'rv_d',
    'rv_c',
    'rv_f',
    'ia32',
    'x86_64',
    'aarch32',
    'aarch64',
]

if mode == 'list':
    for f in features:
        print(f)

elif mode == 'check':
    for l in sys.stdin:
        val_str, name = l.strip().split(None, 1)
        val = bool(int(val_str))
        locals()[name] = val

    assert riscv + intel + arm == 1

    if rv32 or rv64 or rv128: assert riscv
    if riscv: assert rv32 + rv64 + rv128 == 1
    if rv_m or rv_a or rv_d or rv_c or rv_f: assert riscv
    if rv_d: assert rv_f

    if ia32 or x86_64: assert intel
    if intel: assert ia32 + x86_64 == 1

    if aarch32 or aarch64: assert arm
    if arm: assert aarch32 + aarch64 == 1


