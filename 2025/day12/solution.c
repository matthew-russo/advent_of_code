#include <stdio.h> // Required for file operations (fopen, fclose, fgetc, printf, perror)
#include <stdlib.h> // Required for exit()

int main() {
    FILE *filePointer; // Declare a file pointer
    char character;    // Variable to store each character read from the file
    const char *filename = "/tmp/my_file.txt"; // The name of the file to read

    // Open the file in read mode ("r")
    filePointer = fopen(filename, "r");

    // Check if the file was opened successfully
    if (filePointer == NULL) {
        perror("Error opening file"); // Print an error message if opening fails
        exit(EXIT_FAILURE);           // Exit with a failure status
    }

    printf("Content of '%s':\n", filename);

    // Read the file character by character until the end of the file (EOF) is reached
    while ((character = fgetc(filePointer)) != EOF) {
        printf("%c", character); // Print each character to the console
    }

    // Close the file
    if (fclose(filePointer) == EOF) {
        perror("Error closing file"); // Print an error message if closing fails
        exit(EXIT_FAILURE);           // Exit with a failure status
    }

    printf("\nFile closed successfully and program exiting.\n");

    return EXIT_SUCCESS; // Exit with a success status
}
