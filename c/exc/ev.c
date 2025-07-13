#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <linux/input.h>
#include <string.h>
#include <errno.h>

int main(int argc, char *argv[]) {
    const char *device = "/dev/input/event7";  // replace with actual keyboard device
    struct input_event ev;
    int fd = open(device, O_RDONLY);
    if (fd < 0) {
        perror("Cannot open device");
        return 1;
    }

    while (1) {
        ssize_t n = read(fd, &ev, sizeof(ev));
        if (n == (ssize_t)sizeof(ev)) {
            if (ev.type == EV_KEY && ev.value >= 0 && ev.value <= 2) {
                const char *state = ev.value == 1 ? "PRESS" :
                                    ev.value == 0 ? "RELEASE" : "REPEAT";
                printf("Key %d (%s)\n", ev.code, state);
            }
        }
    }

    close(fd);
    return 0;
}
