import os
import shutil
import subprocess
import sys
import tempfile
import textwrap

ROCKET_CHIP_HELPER = os.environ.get('BESSPIN_ROCKET_CHIP_HELPER', 'besspin-rocket-chip-helper')

def run_sbt(args, cwd=None):
    subprocess.run((ROCKET_CHIP_HELPER, 'sbt') + tuple(args), check=True, cwd=cwd)

def query(what):
    cp = subprocess.run((ROCKET_CHIP_HELPER, what), stdout=subprocess.PIPE, check=True)
    return cp.stdout.decode('utf-8').strip()

ABS_FILE = os.path.abspath(__file__)
ABS_DIR = os.path.dirname(ABS_FILE)

def open_rel(path, mode='r'):
    return open(os.path.join(ABS_DIR, path), mode)

def gen_configs_equal(fields):
    checks = ''
    for f in sorted(fields):
        checks += 'a.lift(%s) == b.lift(%s) &&\n' % (f, f)
    checks += 'true'

    return textwrap.dedent('''
        def configsEqual(a: Parameters, b: Parameters): Boolean = {
        %s
        }
    ''') % textwrap.indent(checks, '  ')

def gen_dump_config(fields):
    prints = ''
    for f in sorted(fields):
        prints += 'println(s"%s: ${cfg.lift(%s)}")\n' % (f, f)

    return textwrap.dedent('''
        def dumpConfig(cfg: Parameters) {
        %s
        }
    ''') % textwrap.indent(prints, '  ')

def gen_config_parts(configs):
    parts = ',\n'.join('new %s' % c for c in sorted(configs))

    return textwrap.dedent('''
        val configParts: Array[Parameters] = Array(
        %s
        )
    ''') % textwrap.indent(parts, '  ')

def gen_checkconfig_scala(configs, fields):
    configs_equal = gen_configs_equal(fields)
    dump_config = gen_dump_config(fields)
    config_parts = gen_config_parts(configs)

    template = open_rel('checkconfig.scala.tmpl').read()
    return template \
            .replace('//CONFIGS_EQUAL', textwrap.indent(configs_equal, '  ')) \
            .replace('//CONFIG_PARTS', textwrap.indent(config_parts, '  ')) \
            .replace('//DUMP_CONFIG', textwrap.indent(dump_config, '  '))

def list_configs():
    configs = set(l.strip() for l in open(query('configs')).readlines())
    for c in sorted(configs):
        if not c.startswith('galois'):
            continue
        if '.With' not in c:
            continue
        print(c)

def init_project(enabled_configs, build_dir):
    all_configs = set(l.strip() for l in open(query('configs')).readlines())
    fields = set(l.strip() for l in open(query('fields')).readlines())

    ok = True
    for c in enabled_configs:
        if c not in all_configs:
            print('error: unknown config option %s' % c)
            ok = False
    if not ok:
        sys.exit(1)

    with open(os.path.join(build_dir, 'checkconfig.scala'), 'w') as f:
        f.write(gen_checkconfig_scala(enabled_configs, fields))
    # TODO: get package name and version via `query`
    shutil.copy(os.path.join(ABS_DIR, 'build.sbt'),
            os.path.join(build_dir, 'build.sbt'))
    os.mkdir(os.path.join(build_dir, 'project'))

def check_config(enabled_configs):
    with tempfile.TemporaryDirectory() as build_dir:
        init_project(enabled_configs, build_dir)

        run_sbt(('runMain checkconfig.CheckConfig',), cwd=build_dir)

        # Chisel writes output files into ./build
        os.mkdir(os.path.join(build_dir, 'build'))
        run_sbt(('runMain galois.system.Generator build '
            'galois.system TestHarness checkconfig TheConfig',), cwd=build_dir)

def dump_config(enabled_configs):
    with tempfile.TemporaryDirectory() as build_dir:
        init_project(enabled_configs, build_dir)
        run_sbt(('runMain checkconfig.DumpConfig',), cwd=build_dir)

def dump_config_delta(enabled_configs):
    with tempfile.TemporaryDirectory() as build_dir:
        init_project(enabled_configs, build_dir)
        run_sbt(('runMain checkconfig.DumpConfigDelta',), cwd=build_dir)


def usage():
    print('usage: python3 %s <list|check FLAGS...>' % sys.argv[0])
    sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        usage()

    cmd, args = sys.argv[1], sys.argv[2:]
    if cmd == 'list':
        if len(args) > 0:
            usage()
        list_configs()
    elif cmd == 'check':
        check_config(args)
    elif cmd == 'dump':
        dump_config(args)
    elif cmd == 'dump-delta':
        dump_config_delta(args)
    else:
        usage()
