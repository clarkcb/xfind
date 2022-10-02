#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cjson/cJSON.h>

#include "config.h"
#include "fileutil.h"
#include "filetypes.h"
#include "finderr.h"
#include "intnode.h"
#include "stringarray.h"
#include "stringnode.h"
#include "common.h"

FileTypes *new_filetypes(void)
{
    FileTypes *filetypes = malloc(sizeof(FileTypes));
    filetypes->archive_extensions = NULL;
    filetypes->archive_names = NULL;
    filetypes->binary_extensions = NULL;
    filetypes->binary_names = NULL;
    filetypes->code_extensions = NULL;
    filetypes->code_names = NULL;
    filetypes->text_extensions = NULL;
    filetypes->text_names = NULL;
    filetypes->xml_extensions = NULL;
    filetypes->xml_names = NULL;
    return filetypes;
}

error_t parse_filetypes(const char * const filetypes_json_str, FileTypes *filetypes);

error_t get_filetypes(FileTypes *filetypes)
{
    error_t err = E_OK;

    char *xfindpath = (char *)malloc(MAX_HOMEPATH_LENGTH + 1);
    get_xfindpath(xfindpath);
    // size_t xfind_len = strlen(xfindpath);
    // + 2 because of the '/' and the terminating \0
    char *shared_filetypes_json_path = "shared/filetypes.json";
    char *fullpath = malloc((strlen(xfindpath) + strlen(shared_filetypes_json_path) + 2) * sizeof(char));
    assert(fullpath != NULL);
    join_path(xfindpath, shared_filetypes_json_path, fullpath);

    if (!dir_or_file_exists(fullpath)) {
        err = E_FILE_NOT_FOUND;
        return err;
    }

    // load the file
    // char contents[10450];
    long fsize = file_size(fullpath);
    char contents[fsize];
    contents[0] = '\0';
    FILE *fp = fopen(fullpath, "r");
    int c;
    int i = 0;
    if (fp != NULL) {
        while((c = getc(fp)) != EOF) {
            strcat(contents, (char *)&c);
            i++;
        }
        fclose(fp);
    } else {
        char *errmsg = (char *)malloc((16 + strlen(fullpath)) * sizeof(char));
        sprintf(errmsg, "Unable to load %s", fullpath);
        err = E_UNKNOWN_ERROR;
        return err;
    }

    err = parse_filetypes(contents, filetypes);

    free(fullpath);
    free(xfindpath);

    return err;
}

error_t parse_filetypes(const char * const filetypes_json_str, FileTypes *filetypes)
{
    error_t err = E_OK;

    const cJSON *filetype_json = NULL;
    const cJSON *filetypes_json = NULL;

    StringNode *archive_ext_node = empty_string_node();
    StringNode *archive_name_node = empty_string_node();
    StringNode *binary_ext_node = empty_string_node();
    StringNode *binary_name_node = empty_string_node();
    StringNode *code_ext_node = empty_string_node();
    StringNode *code_name_node = empty_string_node();
    StringNode *text_ext_node = empty_string_node();
    StringNode *text_name_node = empty_string_node();
    StringNode *xml_ext_node = empty_string_node();
    StringNode *xml_name_node = empty_string_node();

    StringNode *nosearch_ext_node = empty_string_node();
    StringNode *nosearch_name_node = empty_string_node();
    StringNode *unknown_ext_node = empty_string_node();
    StringNode *unknown_name_node = empty_string_node();

    cJSON *file_json = cJSON_Parse(filetypes_json_str);
    if (file_json == NULL || cJSON_IsInvalid(file_json)) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Error before: %s\n", error_ptr);
        }
        err = E_UNKNOWN_ERROR;
        goto end;
    }

    filetypes_json = cJSON_GetObjectItemCaseSensitive(file_json, "filetypes");
    cJSON_ArrayForEach(filetype_json, filetypes_json) {
        const cJSON *typename_json = NULL;

        typename_json = cJSON_GetObjectItemCaseSensitive(filetype_json, "type");
        if (cJSON_IsString(typename_json) && (typename_json->valuestring != NULL)) {
            // printf("type: \"%s\"\n", typename_json->valuestring);

            char *name = typename_json->valuestring;

            StringNode *next_ext_node = NULL;
            StringNode *next_name_node = NULL;
            if (strncmp(name, "archive", 7) == 0) {
                next_ext_node = archive_ext_node;
                next_name_node = archive_name_node;
            } else if (strncmp(name, "binary", 6) == 0) {
                next_ext_node = binary_ext_node;
                next_name_node = binary_name_node;
            } else if (strncmp(name, "code", 4) == 0) {
                next_ext_node = code_ext_node;
                next_name_node = code_name_node;
            } else if (strncmp(name, "text", 4) == 0) {
                next_ext_node = text_ext_node;
                next_name_node = text_name_node;
            } else if (strncmp(name, "xml", 3) == 0) {
                next_ext_node = xml_ext_node;
                next_name_node = xml_name_node;
            } else if (strncmp(name, "nosearch", 8) == 0) {
                next_ext_node = nosearch_ext_node;
                next_name_node = nosearch_name_node;
            } else if (strncmp(name, "unknown", 7) == 0) {
                next_ext_node = unknown_ext_node;
                next_name_node = unknown_name_node;
            } else {
                printf("Invalid filetype: \"%s\"\n", name);
                err = E_UNKNOWN_ERROR;
                goto end;
            }

            const cJSON *extension_json = NULL;
            const cJSON *extensions_json = NULL;

            extensions_json = cJSON_GetObjectItemCaseSensitive(filetype_json, "extensions");
            cJSON_ArrayForEach(extension_json, extensions_json) {
                if (cJSON_IsString(extension_json) && (extension_json->valuestring != NULL)) {
                    add_string_to_string_node(extension_json->valuestring, next_ext_node);
                }
            }

            const cJSON *name_json = NULL;
            const cJSON *names_json = NULL;

            names_json = cJSON_GetObjectItemCaseSensitive(filetype_json, "names");
            cJSON_ArrayForEach(name_json, names_json) {
                if (cJSON_IsString(name_json) && (name_json->valuestring != NULL)) {
                    add_string_to_string_node(name_json->valuestring, next_name_node);
                }
            }
        }
    }

    size_t archive_ext_node_len = string_node_count(archive_ext_node);
    filetypes->archive_extensions = new_string_array_with_size(archive_ext_node_len);
    StringNode *temp = archive_ext_node;
    if (archive_ext_node_len > 0) {
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->archive_extensions);
            temp = temp->next;
        }
    }

    size_t archive_name_node_len = string_node_count(archive_name_node);
    filetypes->archive_names = new_string_array_with_size(archive_name_node_len);
    if (archive_name_node_len > 0) {
        temp = archive_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->archive_names);
            temp = temp->next;
        }
    }

    size_t binary_ext_node_len = string_node_count(binary_ext_node);
    filetypes->binary_extensions = new_string_array_with_size(binary_ext_node_len);
    if (binary_ext_node_len > 0) {
        temp = binary_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->binary_extensions);
            temp = temp->next;
        }
    }

    size_t binary_name_node_len = string_node_count(binary_name_node);
    filetypes->binary_names = new_string_array_with_size(binary_name_node_len);
    if (binary_name_node_len > 0) {
        temp = binary_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->binary_names);
            temp = temp->next;
        }
    }

    size_t code_ext_node_len = string_node_count(code_ext_node);
    size_t code_name_node_len = string_node_count(code_name_node);
    size_t text_ext_node_len = string_node_count(text_ext_node);
    size_t text_name_node_len = string_node_count(text_name_node);
    size_t xml_ext_node_len = string_node_count(xml_ext_node);
    size_t xml_name_node_len = string_node_count(xml_name_node);

    filetypes->code_extensions = new_string_array_with_size(code_ext_node_len);
    filetypes->code_names = new_string_array_with_size(code_name_node_len);
    filetypes->xml_extensions = new_string_array_with_size(xml_ext_node_len);
    filetypes->xml_names = new_string_array_with_size(xml_name_node_len);
    filetypes->text_extensions = new_string_array_with_size(text_ext_node_len + code_ext_node_len + xml_ext_node_len);
    filetypes->text_names = new_string_array_with_size(text_name_node_len + code_name_node_len + xml_name_node_len);

    if (code_ext_node_len > 0) {
        temp = code_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->code_extensions);
            add_string_to_string_array(temp->string, filetypes->text_extensions);
            temp = temp->next;
        }
    }

    if (code_name_node_len > 0) {
        temp = code_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->code_names);
            add_string_to_string_array(temp->string, filetypes->text_names);
            temp = temp->next;
        }
    }

    if (text_ext_node_len > 0) {
        temp = text_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->text_extensions);
            temp = temp->next;
        }
    }

    if (text_name_node_len > 0) {
        temp = text_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->text_names);
            temp = temp->next;
        }
    }

    if (xml_ext_node_len > 0) {
        temp = xml_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->xml_extensions);
            add_string_to_string_array(temp->string, filetypes->text_extensions);
            temp = temp->next;
        }
    }

    if (xml_name_node_len > 0) {
        temp = xml_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, filetypes->xml_names);
            add_string_to_string_array(temp->string, filetypes->text_names);
            temp = temp->next;
        }
    }

end:
    destroy_string_node(archive_ext_node);
    destroy_string_node(archive_name_node);
    destroy_string_node(binary_ext_node);
    destroy_string_node(binary_name_node);
    destroy_string_node(code_ext_node);
    destroy_string_node(code_name_node);
    destroy_string_node(text_ext_node);
    destroy_string_node(text_name_node);
    destroy_string_node(xml_ext_node);
    destroy_string_node(xml_name_node);
    destroy_string_node(nosearch_ext_node);
    destroy_string_node(nosearch_name_node);
    destroy_string_node(unknown_ext_node);
    destroy_string_node(unknown_name_node);

    cJSON_Delete(file_json);

    return err;
}

unsigned short is_archive_ext(const char *ext, FileTypes *filetypes) {
    return index_of_string_in_string_array(ext, filetypes->archive_extensions) > -1;
}

unsigned short is_archive_name(const char *name, FileTypes *filetypes) {
    return index_of_string_in_string_array(name, filetypes->archive_names) > -1;
}

unsigned short is_binary_ext(const char *ext, FileTypes *filetypes) {
    return index_of_string_in_string_array(ext, filetypes->binary_extensions) > -1;
}

unsigned short is_binary_name(const char *name, FileTypes *filetypes) {
    return index_of_string_in_string_array(name, filetypes->binary_names) > -1;
}

unsigned short is_code_ext(const char *ext, FileTypes *filetypes) {
    return index_of_string_in_string_array(ext, filetypes->code_extensions) > -1;
}

unsigned short is_code_name(const char *name, FileTypes *filetypes) {
    return index_of_string_in_string_array(name, filetypes->code_names) > -1;
}

unsigned short is_text_ext(const char *ext, FileTypes *filetypes) {
    return index_of_string_in_string_array(ext, filetypes->text_extensions) > -1;
}

unsigned short is_text_name(const char *name, FileTypes *filetypes) {
    return index_of_string_in_string_array(name, filetypes->text_names) > -1;
}

unsigned short is_xml_ext(const char *ext, FileTypes *filetypes) {
    return index_of_string_in_string_array(ext, filetypes->xml_extensions) > -1;
}

unsigned short is_xml_name(const char *name, FileTypes *filetypes) {
    return index_of_string_in_string_array(name, filetypes->xml_names) > -1;
}

FileType get_filetype_for_filename(const char *filename, FileTypes *filetypes)
{
    FileType filetype = UNKNOWN;
    if (strnlen(filename, 10) > 0) {
        if (is_code_name(filename, filetypes)) {
            filetype = CODE;
        } else if (is_xml_name(filename, filetypes)) {
            filetype = XML;
        } else if (is_text_name(filename, filetypes)) {
            filetype = TEXT;
        } else if (is_binary_name(filename, filetypes)) {
            filetype = BINARY;
        } else if (is_archive_name(filename, filetypes)) {
            filetype = ARCHIVE;
        }
    }
    return filetype;
}

FileType get_filetype_for_ext(const char *ext, FileTypes *filetypes)
{
    FileType filetype = UNKNOWN;
    if (strnlen(ext, 10) > 0) {
        if (is_code_ext(ext, filetypes)) {
            filetype = CODE;
        } else if (is_xml_ext(ext, filetypes)) {
            filetype = XML;
        } else if (is_text_ext(ext, filetypes)) {
            filetype = TEXT;
        } else if (is_binary_ext(ext, filetypes)) {
            filetype = BINARY;
        } else if (is_archive_ext(ext, filetypes)) {
            filetype = ARCHIVE;
        }
    }
    return filetype;
}

FileType filetype_from_name(const char *name)
{
    //printf("name: %s\n", name);
    size_t maxlen = 7;
    size_t namelen = strlen(name);
    size_t minlen = maxlen < namelen ? maxlen : namelen;
    char uname[7] = {0};
    strncpy(uname, name, minlen);
    //printf("namelen: %zu\n", namelen);
    //printf("minlen: %zu\n", minlen);
    for (int i = 0; i < minlen; i++) {
        char c = (char)toupper(name[i]);
        uname[i] = c;
    }
    //printf("uname: %s\n", uname);
    if (strncmp(uname, "TEXT", maxlen) == 0) {
        return TEXT;
    }
    if (strncmp(uname, "CODE", maxlen) == 0) {
        return CODE;
    }
    if (strncmp(uname, "XML", maxlen) == 0) {
        return XML;
    }
    if (strncmp(uname, "BINARY", maxlen) == 0) {
        return BINARY;
    }
    if (strncmp(uname, "ARCHIVE", maxlen) == 0) {
        return ARCHIVE;
    }
    return UNKNOWN;
}

void filetype_to_name(const FileType filetype, char *name)
{
    switch(filetype) {
        case ARCHIVE:
            strncpy(name, "ARCHIVE", 7);
            name[7] = '\0';
            break;
        case BINARY:
            strncpy(name, "BINARY", 6);
            name[6] = '\0';
            break;
        case CODE:
            strncpy(name, "CODE", 4);
            name[4] = '\0';
            break;
        case TEXT:
            strncpy(name, "TEXT", 4);
            name[4] = '\0';
            break;
        case XML:
            strncpy(name, "XML", 3);
            name[3] = '\0';
            break;
        default:
            strncpy(name, "UNKNOWN", 7);
            name[7] = '\0';
    }
}

size_t filetype_node_strlen(IntNode *filetype_node)
{
    size_t slen = 2; // for '[' and ']'
    IntNode *temp = filetype_node;
    unsigned int nodecount = 0;
    while (temp != NULL) {
        const FileType *filetype = (const FileType *)temp->integer;
        char *name = malloc(10 * sizeof(char));
        filetype_to_name(*filetype, name);
        slen += strlen(name) + 2; // for ""
        temp = temp->next;
        nodecount++;
    }
    if (nodecount > 1) {
        slen += (nodecount - 1); // for commas
    }
    return slen;
}

void filetype_node_to_string(IntNode *filetype_node, char *s)
{
    // assumes s has correct allocation size
    // int i = 0;
    strcat(s, "[");

    IntNode *temp = filetype_node;
    int nodecount = 0;

    while (temp != NULL) {
        if (nodecount > 0) {
            strcat(s, ",");
        }
        strcat(s, "\"");

        char *name = malloc(10 * sizeof(char));
        name[0] = '\0';
        // filetype_to_name(*(temp->integer), name);
        filetype_to_name((const FileType)(*temp->integer), name);
        strcat(s, name);
        strcat(s, "\"");
        temp = temp->next;
        nodecount++;
    }

    strcat(s, "]");
}

void destroy_filetypes(FileTypes *filetypes)
{
    if (filetypes != NULL) {
        destroy_string_array(filetypes->archive_extensions);
        destroy_string_array(filetypes->binary_extensions);
        destroy_string_array(filetypes->code_extensions);
        destroy_string_array(filetypes->text_extensions);
        destroy_string_array(filetypes->xml_extensions);
        free(filetypes);
    }
}
