#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cjson/cJSON.h>

#include "config.h"
#include "fileutil.h"
#include "filetypes.h"

#include <assert.h>

#include "stringnode.h"

FileTypes *new_file_types(void)
{
    FileTypes *file_types = malloc(sizeof(FileTypes));
    file_types->archive_extensions = NULL;
    file_types->archive_names = NULL;
    file_types->audio_extensions = NULL;
    file_types->audio_names = NULL;
    file_types->binary_extensions = NULL;
    file_types->binary_names = NULL;
    file_types->code_extensions = NULL;
    file_types->code_names = NULL;
    file_types->font_extensions = NULL;
    file_types->font_names = NULL;
    file_types->image_extensions = NULL;
    file_types->image_names = NULL;
    file_types->text_extensions = NULL;
    file_types->text_names = NULL;
    file_types->video_extensions = NULL;
    file_types->video_names = NULL;
    file_types->xml_extensions = NULL;
    file_types->xml_names = NULL;
    return file_types;
}

error_t parse_file_types(const char * const file_types_json_str, FileTypes *file_types);

error_t get_file_types(FileTypes *file_types)
{
    error_t err = E_OK;

    char *full_path = (char *)malloc(MAX_HOMEPATH_LENGTH + 21);
    get_file_types_path(full_path);

    if (!dir_or_file_exists(full_path)) {
        err = E_FILE_NOT_FOUND;
        free(full_path);
        return err;
    }

    // load the file
    const long fsize = file_size(full_path);
    // current size is 11634, make sure it's not dramatically bigger than that
    assert(fsize <= 12000);
    char contents[fsize];
    contents[0] = '\0';
    FILE *fp = fopen(full_path, "r");
    int c;
    if (fp != NULL) {
        while((c = getc(fp)) != EOF) {
            strcat(contents, (char *)&c);
        }
        fclose(fp);
    } else {
        char *errmsg = (char *)malloc((16 + strlen(full_path)) * sizeof(char));
        sprintf(errmsg, "Unable to load %s", full_path);
        err = E_UNKNOWN_ERROR;
        free(errmsg);
        free(full_path);
        return err;
    }

    err = parse_file_types(contents, file_types);

    free(full_path);

    return err;
}

error_t parse_file_types(const char * const file_types_json_str, FileTypes *file_types)
{
    error_t err = E_OK;

    const cJSON *file_type_json = NULL;
    const cJSON *file_types_json = NULL;

    StringNode *archive_ext_node = empty_string_node();
    StringNode *archive_name_node = empty_string_node();
    StringNode *audio_ext_node = empty_string_node();
    StringNode *audio_name_node = empty_string_node();
    StringNode *binary_ext_node = empty_string_node();
    StringNode *binary_name_node = empty_string_node();
    StringNode *code_ext_node = empty_string_node();
    StringNode *code_name_node = empty_string_node();
    StringNode *font_ext_node = empty_string_node();
    StringNode *font_name_node = empty_string_node();
    StringNode *image_ext_node = empty_string_node();
    StringNode *image_name_node = empty_string_node();
    StringNode *text_ext_node = empty_string_node();
    StringNode *text_name_node = empty_string_node();
    StringNode *video_ext_node = empty_string_node();
    StringNode *video_name_node = empty_string_node();
    StringNode *xml_ext_node = empty_string_node();
    StringNode *xml_name_node = empty_string_node();

    StringNode *nosearch_ext_node = empty_string_node();
    StringNode *nosearch_name_node = empty_string_node();
    StringNode *unknown_ext_node = empty_string_node();
    StringNode *unknown_name_node = empty_string_node();

    cJSON *file_json = cJSON_Parse(file_types_json_str);
    if (file_json == NULL || cJSON_IsInvalid(file_json)) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Error before: %s\n", error_ptr);
        }
        err = E_UNKNOWN_ERROR;
        goto end;
    }

    file_types_json = cJSON_GetObjectItemCaseSensitive(file_json, "filetypes");
    cJSON_ArrayForEach(file_type_json, file_types_json) {
        const cJSON *typename_json = NULL;

        typename_json = cJSON_GetObjectItemCaseSensitive(file_type_json, "type");
        if (cJSON_IsString(typename_json) && (typename_json->valuestring != NULL)) {
            // printf("type: \"%s\"\n", typename_json->valuestring);

            char *name = typename_json->valuestring;

            StringNode *next_ext_node = NULL;
            StringNode *next_name_node = NULL;
            if (strncmp(name, FILE_TYPE_NAME_ARCHIVE, 7) == 0) {
                next_ext_node = archive_ext_node;
                next_name_node = archive_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_AUDIO, 5) == 0) {
                next_ext_node = audio_ext_node;
                next_name_node = audio_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_BINARY, 6) == 0) {
                next_ext_node = binary_ext_node;
                next_name_node = binary_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_CODE, 4) == 0) {
                next_ext_node = code_ext_node;
                next_name_node = code_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_FONT, 4) == 0) {
                next_ext_node = font_ext_node;
                next_name_node = font_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_IMAGE, 5) == 0) {
                next_ext_node = image_ext_node;
                next_name_node = image_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_TEXT, 4) == 0) {
                next_ext_node = text_ext_node;
                next_name_node = text_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_VIDEO, 5) == 0) {
                next_ext_node = video_ext_node;
                next_name_node = video_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_XML, 3) == 0) {
                next_ext_node = xml_ext_node;
                next_name_node = xml_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_NOSEARCH, 8) == 0) {
                next_ext_node = nosearch_ext_node;
                next_name_node = nosearch_name_node;
            } else if (strncmp(name, FILE_TYPE_NAME_UNKNOWN, 7) == 0) {
                next_ext_node = unknown_ext_node;
                next_name_node = unknown_name_node;
            } else {
                printf("Invalid filetype: \"%s\"\n", name);
                err = E_UNKNOWN_ERROR;
                goto end;
            }

            const cJSON *extension_json = NULL;
            const cJSON *extensions_json = NULL;

            extensions_json = cJSON_GetObjectItemCaseSensitive(file_type_json, "extensions");
            cJSON_ArrayForEach(extension_json, extensions_json) {
                if (cJSON_IsString(extension_json) && (extension_json->valuestring != NULL)) {
                    add_string_to_string_node(extension_json->valuestring, next_ext_node);
                }
            }

            const cJSON *name_json = NULL;
            const cJSON *names_json = NULL;

            names_json = cJSON_GetObjectItemCaseSensitive(file_type_json, "names");
            cJSON_ArrayForEach(name_json, names_json) {
                if (cJSON_IsString(name_json) && (name_json->valuestring != NULL)) {
                    add_string_to_string_node(name_json->valuestring, next_name_node);
                }
            }
        }
    }

    size_t archive_ext_node_len = string_node_count(archive_ext_node);
    file_types->archive_extensions = new_string_array_with_size(archive_ext_node_len);
    StringNode *temp = archive_ext_node;
    if (archive_ext_node_len > 0) {
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->archive_extensions);
            temp = temp->next;
        }
    }

    size_t archive_name_node_len = string_node_count(archive_name_node);
    file_types->archive_names = new_string_array_with_size(archive_name_node_len);
    if (archive_name_node_len > 0) {
        temp = archive_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->archive_names);
            temp = temp->next;
        }
    }

    size_t audio_ext_node_len = string_node_count(audio_ext_node);
    file_types->audio_extensions = new_string_array_with_size(audio_ext_node_len);
    temp = audio_ext_node;
    if (audio_ext_node_len > 0) {
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->audio_extensions);
            temp = temp->next;
        }
    }

    size_t audio_name_node_len = string_node_count(audio_name_node);
    file_types->audio_names = new_string_array_with_size(audio_name_node_len);
    if (audio_name_node_len > 0) {
        temp = audio_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->audio_names);
            temp = temp->next;
        }
    }

    size_t binary_ext_node_len = string_node_count(binary_ext_node);
    file_types->binary_extensions = new_string_array_with_size(binary_ext_node_len);
    if (binary_ext_node_len > 0) {
        temp = binary_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->binary_extensions);
            temp = temp->next;
        }
    }

    size_t binary_name_node_len = string_node_count(binary_name_node);
    file_types->binary_names = new_string_array_with_size(binary_name_node_len);
    if (binary_name_node_len > 0) {
        temp = binary_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->binary_names);
            temp = temp->next;
        }
    }

    size_t font_ext_node_len = string_node_count(font_ext_node);
    file_types->font_extensions = new_string_array_with_size(font_ext_node_len);
    if (font_ext_node_len > 0) {
        temp = font_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->font_extensions);
            temp = temp->next;
        }
    }

    size_t font_name_node_len = string_node_count(font_name_node);
    file_types->font_names = new_string_array_with_size(font_name_node_len);
    if (font_name_node_len > 0) {
        temp = font_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->font_names);
            temp = temp->next;
        }
    }

    size_t image_ext_node_len = string_node_count(image_ext_node);
    file_types->image_extensions = new_string_array_with_size(image_ext_node_len);
    if (image_ext_node_len > 0) {
        temp = image_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->image_extensions);
            temp = temp->next;
        }
    }

    size_t image_name_node_len = string_node_count(image_name_node);
    file_types->image_names = new_string_array_with_size(image_name_node_len);
    if (image_name_node_len > 0) {
        temp = image_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->image_names);
            temp = temp->next;
        }
    }

    size_t video_ext_node_len = string_node_count(video_ext_node);
    file_types->video_extensions = new_string_array_with_size(video_ext_node_len);
    if (video_ext_node_len > 0) {
        temp = video_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->video_extensions);
            temp = temp->next;
        }
    }

    size_t video_name_node_len = string_node_count(video_name_node);
    file_types->video_names = new_string_array_with_size(video_name_node_len);
    if (video_name_node_len > 0) {
        temp = video_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->video_names);
            temp = temp->next;
        }
    }

    size_t code_ext_node_len = string_node_count(code_ext_node);
    size_t code_name_node_len = string_node_count(code_name_node);
    size_t text_ext_node_len = string_node_count(text_ext_node);
    size_t text_name_node_len = string_node_count(text_name_node);
    size_t xml_ext_node_len = string_node_count(xml_ext_node);
    size_t xml_name_node_len = string_node_count(xml_name_node);

    file_types->code_extensions = new_string_array_with_size(code_ext_node_len);
    file_types->code_names = new_string_array_with_size(code_name_node_len);
    file_types->xml_extensions = new_string_array_with_size(xml_ext_node_len);
    file_types->xml_names = new_string_array_with_size(xml_name_node_len);
    file_types->text_extensions = new_string_array_with_size(text_ext_node_len + code_ext_node_len + xml_ext_node_len);
    file_types->text_names = new_string_array_with_size(text_name_node_len + code_name_node_len + xml_name_node_len);

    if (code_ext_node_len > 0) {
        temp = code_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->code_extensions);
            add_string_to_string_array(temp->string, file_types->text_extensions);
            temp = temp->next;
        }
    }

    if (code_name_node_len > 0) {
        temp = code_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->code_names);
            add_string_to_string_array(temp->string, file_types->text_names);
            temp = temp->next;
        }
    }

    if (text_ext_node_len > 0) {
        temp = text_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->text_extensions);
            temp = temp->next;
        }
    }

    if (text_name_node_len > 0) {
        temp = text_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->text_names);
            temp = temp->next;
        }
    }

    if (xml_ext_node_len > 0) {
        temp = xml_ext_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->xml_extensions);
            add_string_to_string_array(temp->string, file_types->text_extensions);
            temp = temp->next;
        }
    }

    if (xml_name_node_len > 0) {
        temp = xml_name_node;
        while (temp != NULL) {
            add_string_to_string_array(temp->string, file_types->xml_names);
            add_string_to_string_array(temp->string, file_types->text_names);
            temp = temp->next;
        }
    }

end:
    destroy_string_node(archive_ext_node);
    destroy_string_node(archive_name_node);
    destroy_string_node(audio_ext_node);
    destroy_string_node(audio_name_node);
    destroy_string_node(binary_ext_node);
    destroy_string_node(binary_name_node);
    destroy_string_node(code_ext_node);
    destroy_string_node(code_name_node);
    destroy_string_node(font_ext_node);
    destroy_string_node(font_name_node);
    destroy_string_node(image_ext_node);
    destroy_string_node(image_name_node);
    destroy_string_node(text_ext_node);
    destroy_string_node(text_name_node);
    destroy_string_node(video_ext_node);
    destroy_string_node(video_name_node);
    destroy_string_node(xml_ext_node);
    destroy_string_node(xml_name_node);
    destroy_string_node(nosearch_ext_node);
    destroy_string_node(nosearch_name_node);
    destroy_string_node(unknown_ext_node);
    destroy_string_node(unknown_name_node);

    cJSON_Delete(file_json);

    return err;
}

unsigned short is_archive_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->archive_extensions) > -1;
}

unsigned short is_archive_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->archive_names) > -1;
}

unsigned short is_audio_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->audio_extensions) > -1;
}

unsigned short is_audio_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->audio_names) > -1;
}

unsigned short is_binary_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->binary_extensions) > -1;
}

unsigned short is_binary_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->binary_names) > -1;
}

unsigned short is_code_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->code_extensions) > -1;
}

unsigned short is_code_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->code_names) > -1;
}

unsigned short is_font_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->font_extensions) > -1;
}

unsigned short is_font_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->font_names) > -1;
}

unsigned short is_image_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->image_extensions) > -1;
}

unsigned short is_image_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->image_names) > -1;
}

unsigned short is_text_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->text_extensions) > -1;
}

unsigned short is_text_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->text_names) > -1;
}

unsigned short is_video_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->video_extensions) > -1;
}

unsigned short is_video_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->video_names) > -1;
}

unsigned short is_xml_ext(const char *ext, const FileTypes *file_types) {
    return index_of_string_in_string_array(ext, file_types->xml_extensions) > -1;
}

unsigned short is_xml_name(const char *name, const FileTypes *file_types) {
    return index_of_string_in_string_array(name, file_types->xml_names) > -1;
}

FileType get_file_type_for_filename(const char *filename, const FileTypes *file_types)
{
    FileType file_type = UNKNOWN;
    if (strnlen(filename, 10) > 0) {
        // most specific first
        if (is_code_name(filename, file_types)) {
            file_type = CODE;
        } else if (is_archive_name(filename, file_types)) {
            file_type = ARCHIVE;
        } else if (is_audio_name(filename, file_types)) {
            file_type = AUDIO;
        } else if (is_font_name(filename, file_types)) {
            file_type = FONT;
        } else if (is_image_name(filename, file_types)) {
            file_type = IMAGE;
        } else if (is_video_name(filename, file_types)) {
            file_type = VIDEO;

        // most general last
        } else if (is_xml_name(filename, file_types)) {
            file_type = XML;
        } else if (is_text_name(filename, file_types)) {
            file_type = TEXT;
        } else if (is_binary_name(filename, file_types)) {
            file_type = BINARY;
        }
    }
    return file_type;
}

FileType get_file_type_for_ext(const char *ext, const FileTypes *file_types)
{
    FileType file_type = UNKNOWN;
    if (strnlen(ext, 10) > 0) {
        // most specific first
        if (is_code_ext(ext, file_types)) {
            file_type = CODE;
        } else if (is_archive_ext(ext, file_types)) {
            file_type = ARCHIVE;
        } else if (is_audio_ext(ext, file_types)) {
            file_type = AUDIO;
        } else if (is_font_ext(ext, file_types)) {
            file_type = FONT;
        } else if (is_image_ext(ext, file_types)) {
            file_type = IMAGE;
        } else if (is_video_ext(ext, file_types)) {
            file_type = VIDEO;

        // most general last
        } else if (is_xml_ext(ext, file_types)) {
            file_type = XML;
        } else if (is_text_ext(ext, file_types)) {
            file_type = TEXT;
        } else if (is_binary_ext(ext, file_types)) {
            file_type = BINARY;
        }
    }
    return file_type;
}

FileType file_type_from_name(const char *name)
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
        char c = (char)tolower(name[i]);
        uname[i] = c;
    }
    //printf("uname: %s\n", uname);
    if (strncmp(uname, FILE_TYPE_NAME_ARCHIVE, maxlen) == 0) {
        return ARCHIVE;
    }
    if (strncmp(uname, FILE_TYPE_NAME_AUDIO, maxlen) == 0) {
        return AUDIO;
    }
    if (strncmp(uname, FILE_TYPE_NAME_BINARY, maxlen) == 0) {
        return BINARY;
    }
    if (strncmp(uname, FILE_TYPE_NAME_CODE, maxlen) == 0) {
        return CODE;
    }
    if (strncmp(uname, FILE_TYPE_NAME_FONT, maxlen) == 0) {
        return FONT;
    }
    if (strncmp(uname, FILE_TYPE_NAME_IMAGE, maxlen) == 0) {
        return IMAGE;
    }
    if (strncmp(uname, FILE_TYPE_NAME_TEXT, maxlen) == 0) {
        return TEXT;
    }
    if (strncmp(uname, FILE_TYPE_NAME_VIDEO, maxlen) == 0) {
        return VIDEO;
    }
    if (strncmp(uname, FILE_TYPE_NAME_XML, maxlen) == 0) {
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
    IntNode *temp = file_type_node;
    unsigned int nodecount = 0;
    while (temp != NULL) {
        const FileType *file_type = (const FileType *)temp->integer;
        char *name = malloc(10 * sizeof(char));
        file_type_to_name(*file_type, name);
        slen += strlen(name) + 2; // for ""
        temp = temp->next;
        nodecount++;
    }
    if (nodecount > 1) {
        slen += (nodecount - 1); // for commas
    }
    return slen;
}

void file_type_node_to_string(IntNode *file_type_node, char *s)
{
    // assumes s has correct allocation size
    // int i = 0;
    strcat(s, "[");

    IntNode *temp = file_type_node;
    int nodecount = 0;

    while (temp != NULL) {
        if (nodecount > 0) {
            strcat(s, ", ");
        }
        //strcat(s, "\"");

        char *name = malloc(10 * sizeof(char));
        name[0] = '\0';
        // file_type_to_name(*(temp->integer), name);
        file_type_to_name((const FileType)(*temp->integer), name);
        strcat(s, name);
        //strcat(s, "\"");
        temp = temp->next;
        nodecount++;
    }

    strcat(s, "]");
}

void destroy_file_types(FileTypes *file_types)
{
    if (file_types != NULL) {
        destroy_string_array(file_types->archive_extensions);
        destroy_string_array(file_types->archive_names);
        destroy_string_array(file_types->audio_extensions);
        destroy_string_array(file_types->audio_names);
        destroy_string_array(file_types->binary_extensions);
        destroy_string_array(file_types->binary_names);
        destroy_string_array(file_types->code_extensions);
        destroy_string_array(file_types->code_names);
        destroy_string_array(file_types->font_extensions);
        destroy_string_array(file_types->font_names);
        destroy_string_array(file_types->image_extensions);
        destroy_string_array(file_types->image_names);
        destroy_string_array(file_types->text_extensions);
        destroy_string_array(file_types->text_names);
        destroy_string_array(file_types->video_extensions);
        destroy_string_array(file_types->video_names);
        destroy_string_array(file_types->xml_extensions);
        destroy_string_array(file_types->xml_names);
        free(file_types);
    }
}
