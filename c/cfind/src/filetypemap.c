#include "filetypemap.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int hash(const char *key) {
    unsigned long int hash_value = 0;
    int i = 0;
    while (key[i] != '\0') {
        hash_value = hash_value * 31 + key[i];
        i++;
    }
    return hash_value % FILE_TYPE_MAP_SIZE;
}

void init_file_type_map(FileTypeMap *map) {
    for (int i = 0; i < FILE_TYPE_MAP_SIZE; i++) {
        map->buckets[i] = NULL;
    }
}

FileTypeNode *new_file_type_node() {
    FileTypeNode *node = malloc(sizeof(FileTypeNode));
    if (node == NULL) {
        return NULL;
    }
    node->key = NULL;
    node->value = UNKNOWN;
    node->next = NULL;
    return node;
}

void add_entry_to_map(FileTypeMap *map, const char *key, FileType file_type) {
    const unsigned int bucket_index = hash(key);
    FileTypeNode *new_node = new_file_type_node();
    new_node->key = strdup(key);
    new_node->value = file_type;
    new_node->next = map->buckets[bucket_index];
    map->buckets[bucket_index] = new_node;
}

FileType get_file_type_for_key(FileTypeMap *map, const char *key) {
    const unsigned int bucket_index = hash(key);
    FileTypeNode *node = map->buckets[bucket_index];

    while (node != NULL) {
        if (strcmp(node->key, key) == 0) {
            return node->value;
        }
        node = node->next;
    }
    return UNKNOWN;
}

void print_file_type_map(const FileTypeMap *map) {
    for (int i = 0; i < FILE_TYPE_MAP_SIZE; i++) {
        FileTypeNode *node = map->buckets[i];
        while (node != NULL) {
            printf("map[%d] = key: %s, value: %d\n", i, node->key, node->value);
            node = node->next;
        }
    }
}

void destroy_file_type_map(FileTypeMap *map) {
    for (int i = 0; i < FILE_TYPE_MAP_SIZE; i++) {
        FileTypeNode *node = map->buckets[i];
        while (node != NULL) {
            FileTypeNode *temp = node;
            node = node->next;
            free(temp->key);
            free(temp);
        }
    }
}
