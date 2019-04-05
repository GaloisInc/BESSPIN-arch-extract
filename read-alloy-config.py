'''
Convert an Alloy feature model instance to the configuration format used by
featuresynth `oracle-command` scripts.
'''
import re
import sys

INST_RE = re.compile(r'''
    ^
    (?P<var>
        [a-zA-Z0-9_/]+
        (<: [a-zA-Z0-9_/]+)*
    )
    =
    { (?P<vals> .*) }
    $
    ''', re.VERBOSE)

CLAFER_PREFIX_RE = re.compile(r'(r_)?c[0-9]+_')

GROUP_RE = re.compile(r'grp_[0-9]+$')

saw_file_start = False
saw_command_start = False

config = {}

for line in sys.stdin:
    if '=== Parsing+Typechecking' in line:
        assert not saw_file_start, \
                'saw multiple files in Alloy output (expected exactly one)'
        saw_file_start = True

        continue

    if '=== Command' in line:
        assert not saw_command_start, \
                'saw multiple commands in Alloy output (expected exactly one)'
        saw_file_start = True

        kind = line.split()[2]
        assert kind.lower() == 'run', \
                'expected a "run" command, but saw %r' % kind

        continue

    if line.startswith('---'):
        # INSTANCE / OUTCOME marker
        continue

    if line.strip() == 'Unsatisfiable.':
        print('error: model is unsatisfiable', file=sys.stderr)
        sys.exit(1)

    m = INST_RE.match(line.strip())
    if m:
        nonempty = m.group('vals') != ''
        last_var = m.group('var').split('<:')[-1]

        if last_var.startswith('this/'):
            last_var = last_var[len('this/'):]

        # Clafer adds a prefix like `c0_` or `r_c0_` to its generated names.
        prefix_m = CLAFER_PREFIX_RE.match(last_var)
        if prefix_m:
            last_var = last_var[prefix_m.end():]
        else:
            # Non-clafer-generated vars are usually Alloy builtins, which we
            # want to ignore.
            continue

        # featuresynth-generated groups use the `GROUP_RE` naming convention.
        # We don't want to emit anything for generated groups.
        if GROUP_RE.match(last_var):
            continue

        if last_var not in config:
            config[last_var] = False
        config[last_var] |= nonempty

for var, nonempty in config.items():
    print('%d %s' % (1 if nonempty else 0, var))
