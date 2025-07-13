#include "util.h"
#ifndef _WIN32
// Custom strdup implementation
char* strdup(const char *s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1; // +1 for the null terminator
    char *copy = (char *)malloc(len);
    if (!copy) return NULL; // Allocation failed
    strcpy(copy, s);
    return copy;
}
#endif
// Function to get a substring
char* substring(const char *str, int start, int end) {
    if (start < 0 || end > strlen(str) || start > end) {
        return NULL; // Invalid indices
    }

    int len = end - start;
    char *sub = (char *)malloc(len + 1); // +1 for the null terminator
    if (!sub) return NULL; // Allocation failed

    strncpy(sub, str + start, len);
    sub[len] = '\0'; // Null-terminate the substring
    return sub;
}

// Function to replace all occurrences of a substring
char* replace(const char *str, const char *old, const char *replacement) {
    if (!str || !old || !replacement) return NULL;

    const char *pos = str;
    int count = 0;
    int old_len = strlen(old);
    int replacement_len = strlen(replacement);

    // Count occurrences of old substring
    while ((pos = strstr(pos, old)) != NULL) {
        count++;
        pos += old_len;
    }

    // Allocate memory for the replacement string
    char *result = (char *)malloc(strlen(str) + count * (replacement_len - old_len) + 1);
    if (!result) return NULL; // Allocation failed

    char *ptr = result;
    while (*str) {
        if (strstr(str, old) == str) {
            strcpy(ptr, replacement); // Replace old with replacement
            ptr += replacement_len;
            str += old_len; // Move past the old substring
        } else {
            *ptr++ = *str++; // Copy the character
        }
    }
    *ptr = '\0'; // Null-terminate the result
    return result;
}

// Function to find the index of a substring
int indexOf(const char *str, const char *substr) {
    char *pos = strstr(str, substr);
    return (pos) ? (pos - str) : -1; // Return the index or -1 if not found
}

// Function to split a string by a delimiter
char** splitArr(const char *str, const char *delimiter, int *count) {
    // Allocate memory for the array of strings
    char **result = NULL;
    char *temp = strdup(str); // Create a modifiable copy of the original string
    char *token;
    int size = 0;

    // Tokenize the string
    token = strtok(temp, delimiter);
    while (token) {
        result = realloc(result, sizeof(char*) * (size + 1)); // Resize the array
        result[size] = strdup(token); // Duplicate the token into the array
        size++;
        token = strtok(NULL, delimiter); // Get the next token
    }

    free(temp); // Free the temporary string
    *count = size; // Set the number of tokens found
    return result; // Return the array of tokens
}

// Function to free the memory allocated for the split result
void free_split(char **result, int count) {
    for (int i = 0; i < count; i++) {
        free(result[i]); // Free each individual string
    }
    free(result); // Free the array of strings
}

// Function to initialize a StrArray
StrArray* create_array() {
    StrArray *arr = (StrArray*)malloc(sizeof(StrArray));
    arr->size = 0;
    arr->capacity = 4; // Initial capacity
    arr->tokens = (char**)malloc(arr->capacity * sizeof(char*));
    return arr;
}

// Function to free the StrArray
void free_array(StrArray *arr) {
    for (int i = 0; i < arr->size; i++) {
        free(arr->tokens[i]); // Free each individual string
    }
    free(arr->tokens); // Free the array of strings
    free(arr); // Free the StrArray structure
}

// Function to append a string to the dynamic array
void append(StrArray *arr, const char *token) {
    if (arr->size >= arr->capacity) {
        arr->capacity *= 2; // Double the capacity
        arr->tokens = realloc(arr->tokens, arr->capacity * sizeof(char*));
    }
    arr->tokens[arr->size] = strdup(token); // Duplicate the token
    arr->size++;
}

// Function to prepend a string to the dynamic array
void prepend(StrArray *arr, const char *token) {
    if (arr->size >= arr->capacity) {
        arr->capacity *= 2; // Double the capacity
        arr->tokens = realloc(arr->tokens, arr->capacity * sizeof(char*));
    }

    // Shift elements to the right to make space for the new token
    for (size_t i = arr->size; i > 0; i--) {
        arr->tokens[i] = arr->tokens[i - 1];
    }

    arr->tokens[0] = strdup(token); // Duplicate the token at the beginning
    arr->size++;
}

// Function to insert a string at a specific index in the dynamic array
void insert(StrArray *arr, const char *token, size_t index) {
    if (index > arr->size) {
        printf("Index out of bounds\n");
        return; // Handle out-of-bounds index
    }

    if (arr->size >= arr->capacity) {
        arr->capacity *= 2; // Double the capacity
        arr->tokens = realloc(arr->tokens, arr->capacity * sizeof(char*));
    }

    // Shift elements to the right to make space for the new token
    for (size_t i = arr->size; i > index; i--) {
        arr->tokens[i] = arr->tokens[i - 1];
    }

    arr->tokens[index] = strdup(token); // Duplicate the token at the specified index
    arr->size++;
}

// Function to get a string at a specific index
const char* get(StrArray *arr, int index) {
    if (index < 0) {
        index += arr->size; // Handle negative indexing
    }
    if (index < 0 || index >= arr->size) {
        return NULL; // Index out of bounds
    }
    return arr->tokens[index];
}

// Function to set a string at a specific index
void set(StrArray *arr, int index, const char *token) {
    if (index < 0) {
        index += arr->size; // Handle negative indexing
    }
    if (index >= 0 && index < arr->size) {
        free(arr->tokens[index]); // Free the old string
        arr->tokens[index] = strdup(token); // Duplicate the new token
    }
}

// Function to delete a string at a specific index
void del(StrArray *arr, int index) {
    if (index < 0) {
        index += arr->size; // Handle negative indexing
    }
    if (index >= 0 && index < arr->size) {
        free(arr->tokens[index]); // Free the string to be deleted
        for (int i = index; i < arr->size - 1; i++) {
            arr->tokens[i] = arr->tokens[i + 1]; // Shift left
        }
        arr->size--; // Decrease the size
    }
}

// Function to split a string by a delimiter (StrArray version)
StrArray* split(const char *str, const char *delimiter) {
    StrArray *result = create_array();
    char *temp = strdup(str); // Create a modifiable copy of the original string
    char *token;

    // Tokenize the string
    token = strtok(temp, delimiter);
    while (token) {
        append(result, token); // Append each token to the dynamic array
        token = strtok(NULL, delimiter); // Get the next token
    }

    free(temp); // Free the temporary string
    return result; // Return the result dynamic array
}

// Function to join an array of strings into a single string with a delimiter
char* join(StrArray *arr, const char *delimiter) {
    if (arr->size == 0) return strdup(""); // Return an empty string if no tokens

    // Calculate the total length needed for the joined string
    size_t total_length = 0;
    for (int i = 0; i < arr->size; i++) {
        total_length += strlen(arr->tokens[i]);
        if (i < arr->size - 1) { // Add delimiter length for all but the last token
            total_length += strlen(delimiter);
        }
    }

    char *result = (char*)malloc(total_length + 1); // +1 for the null terminator
    if (!result) return NULL; // Allocation failed

    // Construct the joined string
    result[0] = '\0'; // Initialize to an empty string
    for (int i = 0; i < arr->size; i++) {
        strcat(result, arr->tokens[i]); // Append the token
        if (i < arr->size - 1) { // Append delimiter for all but the last token
            strcat(result, delimiter);
        }
    }
    return result; // Return the joined string
}