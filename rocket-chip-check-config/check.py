import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap

ROCKET_CHIP_HELPER = os.environ.get('BESSPIN_ROCKET_CHIP_HELPER', 'besspin-rocket-chip-helper')
CACHE_DIR = os.environ.get('BESSPIN_CHECK_CONFIG_CACHE_DIR', 'check-config-cache')

ABS_FILE = os.path.abspath(__file__)
ABS_DIR = os.path.dirname(ABS_FILE)

def open_rel(path, mode='r'):
    '''Open a file with a path relative to this script's parent directory.'''
    return open(os.path.join(ABS_DIR, path), mode)


def run_sbt(args, cwd=None):
    subprocess.run((ROCKET_CHIP_HELPER, 'sbt') + tuple(args), check=True, cwd=cwd)

def query(what):
    cp = subprocess.run((ROCKET_CHIP_HELPER, what), stdout=subprocess.PIPE, check=True)
    return cp.stdout.decode('utf-8').strip()

def query_file(what):
    path = query(what)
    return set(l.strip() for l in open(path).readlines())


# checkconfig.jar code generation & build process

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

def gen_config_parts(base_configs):
    base_parts = ',\n'.join('new %s' % c for c in sorted(base_configs))

    return textwrap.dedent('''
        val baseConfigParts: Array[Parameters] = Array(
        %s
        )
    ''') % (textwrap.indent(base_parts, '  '),)

def gen_checkconfig_scala(base_configs, fields):
    configs_equal = gen_configs_equal(fields)
    dump_config = gen_dump_config(fields)
    config_parts = gen_config_parts(base_configs)

    template = open_rel('checkconfig.scala.tmpl').read()
    return template \
            .replace('//CONFIGS_EQUAL', textwrap.indent(configs_equal, '  ')) \
            .replace('//CONFIG_PARTS', textwrap.indent(config_parts, '  ')) \
            .replace('//DUMP_CONFIG', textwrap.indent(dump_config, '  '))

def gen_checkconfig_jar(base_configs, fields, dest):
    with tempfile.TemporaryDirectory() as build_dir:
        with open(os.path.join(build_dir, 'checkconfig.scala'), 'w') as f:
            f.write(gen_checkconfig_scala(base_configs, fields))
        # TODO: get package name and version via `query`
        shutil.copy(os.path.join(ABS_DIR, 'build.sbt'),
                os.path.join(build_dir, 'build.sbt'))
        os.mkdir(os.path.join(build_dir, 'project'))
        shutil.copy(os.path.join(ABS_DIR, 'project/plugins.sbt'),
                os.path.join(build_dir, 'project/plugins.sbt'))

        run_sbt(('assembly',), cwd=build_dir)

        shutil.copy(
                os.path.join(build_dir, 'target/scala-2.12/checkconfig-assembly-0.1.0-SNAPSHOT.jar'),
                dest)


def cache_key(base_configs, fields):
    lines = [str(len(base_configs)), str(len(fields))] + sorted(base_configs) + sorted(fields)
    bs = b''.join(l.encode('utf-8') + b'\0' for l in lines)
    h = hashlib.sha256(bs)
    return h.hexdigest()

def cached_jar_path(base_configs, fields):
    k = cache_key(base_configs, fields)
    return os.path.join(CACHE_DIR, 'checkconfig-%s.jar' % k)

def ensure_cached(base_configs, fields):
    path = cached_jar_path(base_configs, fields)
    if not os.path.exists(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        gen_checkconfig_jar(base_configs, fields, path)
    return path




def split_configs(configs):
    try:
        idx = configs.index('--')
        return configs[:idx], configs[idx + 1:]
    except:
        return configs, []


def jar_path(base_configs):
    fields = set(l.strip() for l in open(query('fields')).readlines())
    jar_path = ensure_cached(base_configs, fields)
    print(jar_path)

def list_configs():
    configs = set(l.strip() for l in open(query('configs')).readlines())
    for c in sorted(configs):
        if not c.startswith('galois'):
            continue
        if '.With' not in c:
            continue
        print(c)

def check_config_names(chosen_configs):
    all_configs = set(l.strip() for l in open(query('configs')).readlines())

    ok = True
    for c in chosen_configs:
        if c == '--':
            break
        if c not in all_configs:
            print('error: unknown config option %s' % c)
            ok = False
    if not ok:
        sys.exit(1)

def check_config(enabled_configs):
    chosen_configs, base_configs = split_configs(enabled_configs)
    check_config_names(chosen_configs)
    jar_path = ensure_cached(base_configs, query_file('fields'))
    jar_path = os.path.abspath(jar_path)

    with tempfile.TemporaryDirectory() as run_dir:
        # Chisel elaboration writes output to ./build
        os.mkdir(os.path.join(run_dir, 'build'))
        subprocess.run(
                ('java', '-cp', jar_path, 'checkconfig.CheckConfig') + tuple(chosen_configs),
                cwd=run_dir, check=True)

def dump_config(enabled_configs):
    with tempfile.TemporaryDirectory() as build_dir:
        init_project(enabled_configs, build_dir)
        run_sbt(('runMain checkconfig.DumpConfig',), cwd=build_dir)


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
    elif cmd == 'jar':
        jar_path(args)
    elif cmd == 'check':
        check_config(args)
    elif cmd == 'dump':
        dump_config(args)
    else:
        usage()
