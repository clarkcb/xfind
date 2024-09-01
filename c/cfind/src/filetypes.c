#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cjson/cJSON.h>

#include "config.h"
#include "fileutil.h"
#include "filetypes.h"

#include <assert.h>

FileTypes *new_file_types(void)
{
    FileTypes *file_types = malloc(sizeof(FileTypes));
    file_types->db = NULL;
    file_types->ext_type_cache = malloc(sizeof(FileTypeMap));
    file_types->name_type_cache = malloc(sizeof(FileTypeMap));
    return file_types;
}

error_t parse_file_types(const char *file_types_json_str, FileTypes *file_types);

void load_name_type_cache(FileTypes *file_types);

error_t init_file_types(FileTypes *file_types)
{
    // Get the database connection
    sqlite3 *db;
    char xfind_db_path[MAX_HOMEPATH_LENGTH + 24] = {0};
    get_xfind_db_path(xfind_db_path);

    assert(xfind_db_path != NULL);

    if (!dir_or_file_exists(xfind_db_path)) {
        return E_FILE_NOT_FOUND;
    }

    const int rc = sqlite3_open_v2(xfind_db_path, &db, SQLITE_OPEN_READONLY, NULL);
    if (rc == SQLITE_OK && db != NULL) {
        file_types->db = db;
    } else {
        fprintf(stderr, "error: %s\n", sqlite3_errmsg(db));
        return E_UNKNOWN_ERROR;
    }

    init_file_type_map(file_types->ext_type_cache);
    init_file_type_map(file_types->name_type_cache);

    load_name_type_cache(file_types);

    return E_OK;
}

void get_file_types_for_query_and_param(const FileTypes *file_types, const char *query, const char *param,
    FileTypeMap *file_type_map) {
    sqlite3_stmt *stmt;
    sqlite3_prepare_v2(file_types->db, query, -1, &stmt, NULL);
    int rc = SQLITE_OK;
    if (param != NULL) {
        rc = sqlite3_bind_text(stmt, 1, param, -1, SQLITE_TRANSIENT);
    }
    if (rc != SQLITE_OK) {
        fprintf(stderr, "error: %s\n", sqlite3_errmsg(file_types->db));
    }
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const unsigned char *key = sqlite3_column_text(stmt, 0);
        const FileType file_type = sqlite3_column_int(stmt, 1) - 1;
        add_entry_to_map(file_type_map, key, file_type);
    }
    sqlite3_finalize(stmt);
}

void load_name_type_cache(FileTypes *file_types) {
    const char *query = "select name, file_type_id from file_name";
    get_file_types_for_query_and_param(file_types, query, NULL, file_types->name_type_cache);
    // printf("name_type_cache:\n");
    // print_file_type_map(file_types->name_type_cache);
    // printf("\n");
}

FileType get_file_type_for_query_and_param(const FileTypes *file_types, const char *query, const char *param) {
    FileType file_type = UNKNOWN;
    sqlite3_stmt *stmt;
    sqlite3_prepare_v2(file_types->db, query, -1, &stmt, NULL);
    const int rc = sqlite3_bind_text(stmt, 1, param, -1, SQLITE_TRANSIENT);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "error: %s\n", sqlite3_errmsg(file_types->db));
    }
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        file_type = sqlite3_column_int(stmt, 0) - 1;
    }
    sqlite3_finalize(stmt);
    return file_type;
}

FileType get_file_type_for_file_name(const FileTypes *file_types, const char *file_name)
{
    // const FileType file_type = get_file_type_for_key(file_types->name_type_cache, file_name);
    // if (file_type != UNKNOWN) {
    //     return file_type;
    // }
    // const char *query = "select file_type_id from file_name where name=?";
    // return get_file_type_for_query_and_param(file_types, query, file_name);
    return get_file_type_for_key(file_types->name_type_cache, file_name);
}

FileType get_file_type_for_ext(const FileTypes *file_types, const char *ext)
{
    if (ext == NULL || strncmp(ext, "", 64) == 0) {
        return UNKNOWN;
    }
    FileType file_type = get_file_type_for_key(file_types->ext_type_cache, ext);
    if (file_type != UNKNOWN) {
        return file_type;
    }
    const char *query = "select file_type_id from file_extension where extension=?";
    file_type = get_file_type_for_query_and_param(file_types, query, ext);
    if (file_type != UNKNOWN) {
        add_entry_to_map(file_types->ext_type_cache, ext, file_type);
    }
    return file_type;
}

FileType get_file_type(const FileTypes *file_types, const char *file_name)
{
    if (file_name == NULL) return UNKNOWN;
    const size_t file_len = strnlen(file_name, 1024);
    if (file_len < 1) return UNKNOWN;
    const FileType file_type = get_file_type_for_file_name(file_types, file_name);
    if (file_type != UNKNOWN) return file_type;
    char ext[file_len];
    ext[0] = '\0';
    get_extension(file_name, ext);
    return get_file_type_for_ext(file_types, ext);
}

unsigned short is_archive_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == ARCHIVE;
}

unsigned short is_archive_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == ARCHIVE;
}

unsigned short is_audio_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == AUDIO;
}

unsigned short is_audio_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == AUDIO;
}

unsigned short is_binary_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == BINARY;
}

unsigned short is_binary_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == BINARY;
}

unsigned short is_code_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == CODE;
}

unsigned short is_code_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == CODE;
}

unsigned short is_font_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == FONT;
}

unsigned short is_font_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == FONT;
}

unsigned short is_image_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == IMAGE;
}

unsigned short is_image_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == IMAGE;
}

unsigned short is_text_ext(const FileTypes *file_types, const char *ext) {
    const FileType file_type = get_file_type_for_ext(file_types, ext);
    return file_type == TEXT || file_type == CODE || file_type == XML;
}

unsigned short is_text_name(const FileTypes *file_types, const char *name) {
    const FileType file_type = get_file_type_for_file_name(file_types, name);
    return file_type == TEXT || file_type == CODE || file_type == XML;
}

unsigned short is_video_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == VIDEO;
}

unsigned short is_video_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == VIDEO;
}

unsigned short is_xml_ext(const FileTypes *file_types, const char *ext) {
    return get_file_type_for_ext(file_types, ext) == XML;
}

unsigned short is_xml_name(const FileTypes *file_types, const char *name) {
    return get_file_type_for_file_name(file_types, name) == XML;
}

FileType file_type_from_name(const char *name)
{
    //printf("name: %s\n", name);
    const size_t max_len = 8;
    const size_t name_len = strnlen(name, max_len);
    const size_t min_len = name_len < max_len ? name_len : max_len;
    char lname[8] = {0};
    strncpy(lname, name, min_len);
    //printf("name_len: %zu\n", name_len);
    //printf("min_len: %zu\n", min_len);
    for (int i = 0; i < min_len; i++) {
        const char c = tolower(name[i]);
        lname[i] = c;
    }
    //printf("lname: %s\n", lname);
    if (strncmp(lname, FILE_TYPE_NAME_ARCHIVE, min_len) == 0) {
        return ARCHIVE;
    }
    if (strncmp(lname, FILE_TYPE_NAME_AUDIO, min_len) == 0) {
        return AUDIO;
    }
    if (strncmp(lname, FILE_TYPE_NAME_BINARY, min_len) == 0) {
        return BINARY;
    }
    if (strncmp(lname, FILE_TYPE_NAME_CODE, min_len) == 0) {
        return CODE;
    }
    if (strncmp(lname, FILE_TYPE_NAME_FONT, min_len) == 0) {
        return FONT;
    }
    if (strncmp(lname, FILE_TYPE_NAME_IMAGE, min_len) == 0) {
        return IMAGE;
    }
    if (strncmp(lname, FILE_TYPE_NAME_TEXT, min_len) == 0) {
        return TEXT;
    }
    if (strncmp(lname, FILE_TYPE_NAME_VIDEO, min_len) == 0) {
        return VIDEO;
    }
    if (strncmp(lname, FILE_TYPE_NAME_XML, min_len) == 0) {
        return XML;
    }
    return UNKNOWN;
}

void file_type_to_name(const FileType file_type, char *name)
{
    switch(file_type) {
        case ARCHIVE:
            strncpy(name, FILE_TYPE_NAME_ARCHIVE, 7);
            name[7] = '\0';
            break;
        case AUDIO:
            strncpy(name, FILE_TYPE_NAME_AUDIO, 5);
            name[5] = '\0';
            break;
        case BINARY:
            strncpy(name, FILE_TYPE_NAME_BINARY, 6);
            name[6] = '\0';
            break;
        case CODE:
            strncpy(name, FILE_TYPE_NAME_CODE, 4);
            name[4] = '\0';
            break;
        case FONT:
            strncpy(name, FILE_TYPE_NAME_FONT, 4);
            name[4] = '\0';
            break;
        case IMAGE:
            strncpy(name, FILE_TYPE_NAME_IMAGE, 5);
            name[5] = '\0';
            break;
        case TEXT:
            strncpy(name, FILE_TYPE_NAME_TEXT, 4);
            name[4] = '\0';
            break;
        case VIDEO:
            strncpy(name, FILE_TYPE_NAME_VIDEO, 5);
            name[5] = '\0';
            break;
        case XML:
            strncpy(name, FILE_TYPE_NAME_XML, 3);
            name[3] = '\0';
            break;
        default:
            strncpy(name, FILE_TYPE_NAME_UNKNOWN, 7);
            name[7] = '\0';
    }
}

size_t file_type_node_strlen(IntNode *file_type_node)
{
    size_t slen = 2; // for '[' and ']'
    IntNode *next_node = file_type_node;
    unsigned int node_count = 0;
    while (next_node != NULL) {
        const FileType *file_type = (const FileType *)next_node->integer;
        char *name = malloc(10 * sizeof(char));
        file_type_to_name(*file_type, name);
        slen += strnlen(name, 10) + 2; // for ""
        next_node = next_node->next;
        node_count++;
        free(name);
    }
    if (node_count > 1) {
        slen += (node_count - 1); // for commas
    }
    return slen;
}

void file_type_node_to_string(IntNode *file_type_node, char *s)
{
    // assumes s has correct allocation size
    // int i = 0;
    strcat(s, "[");

    IntNode *next_node = file_type_node;
    int node_count = 0;

    while (next_node != NULL) {
        if (node_count > 0) {
            strcat(s, ", ");
        }
        //strcat(s, "\"");

        char *name = malloc(10 * sizeof(char));
        name[0] = '\0';
        // file_type_to_name(*(temp->integer), name);
        file_type_to_name((const FileType)(*next_node->integer), name);
        strcat(s, name);
        //strcat(s, "\"");
        next_node = next_node->next;
        node_count++;
    }

    strcat(s, "]");
}

void destroy_file_types(FileTypes *file_types)
{
    if (file_types != NULL) {
        sqlite3_close(file_types->db);
        destroy_file_type_map(file_types->ext_type_cache);
        destroy_file_type_map(file_types->name_type_cache);
        free(file_types);
    }
}
