#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *filePointer;
    char character;
    const char *filename = "../input.txt";

    filePointer = fopen(filename, "r");

    if (filePointer == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    // TODO: solution

    if (fclose(filePointer) == EOF) {
        perror("Error closing file");
        exit(EXIT_FAILURE);
    }

    printf("\nFile closed successfully and program exiting.\n");

    return EXIT_SUCCESS;
}
