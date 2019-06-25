#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("usage: nametag <name> <cmd...>\n");
        printf("exec <cmd...>, with argv[0] prefixed with [<name>]\n");
        return 1;
    }

    size_t len = 2 + strlen(argv[1]) + 2 + strlen(argv[2]) + 1;
    char* new_arg = malloc(len);
    snprintf(new_arg, len, "[%s] %s", argv[1], argv[2]);
    const char* old_argv2 = argv[2];
    argv[2] = new_arg;
    execvp(old_argv2, argv + 2);
}

