/*
 * C interface to JSON-Fortran.
 *
 * Creates, navigates, modifies, and destroys JSON structures. It requires the
 * library's default kinds: 32-bit integers, double precision reals, and
 * non-Unicode strings. See json_c_module.F90.
 *
 * Ownership:
 *   - jsonf_new_* and jsonf_clone return an owned root. Free it with jsonf_destroy.
 *   - Lookups return borrowed nodes, valid until the node or an ancestor is destroyed.
 *   - jsonf_add gives the child to the parent.
 *   - jsonf_detach turns a node into an owned root.
 *   - jsonf_set_* replaces an existing node if the type changes,
 *     so any handle to that node is no longer valid.
 *
 * Errors:
 *   - Each call clears the previous error on the core.
 *   - JSONF_NOT_FOUND is not an error, and jsonf_last_error stays NULL.
 *   - Each core must only be used by one thread at a time.
 *
 * Strings:
 *   - Names, paths, and file names are null-terminated.
 *   - String values are (pointer, length), and the pointer can be NULL if the length is 0.
 *   - Returned strings are null-terminated. Free them with jsonf_free.
 *     The length output can be NULL.
 *
 * License: BSD-style. See the LICENSE file.
 */

#ifndef JSONFORTRAN_H
#define JSONFORTRAN_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct jsonf_core  jsonf_core;
typedef struct jsonf_value jsonf_value;

typedef enum {
    JSONF_OK = 0,
    JSONF_NOT_FOUND = 1,
    JSONF_TYPE_ERROR = 2,
    JSONF_INVALID_ARGUMENT = 3,
    JSONF_ERROR = 4
} jsonf_status;

typedef enum {
    JSONF_NULL = 0,
    JSONF_OBJECT = 1,
    JSONF_ARRAY = 2,
    JSONF_LOGICAL = 3,
    JSONF_INTEGER = 4,
    JSONF_REAL = 5,
    JSONF_STRING = 6
} jsonf_type;

typedef struct {
    int case_sensitive_keys;  /* default: 1 */
    int strict_type_checking; /* if set, get does no type conversions. default: 0 */
    int path_mode;            /* 1 default, 2 RFC 6901, 3 JSONPath bracket. default: 1 */
} jsonf_options;

/* Lifecycle */
void         jsonf_options_default(jsonf_options *opts);
jsonf_core  *jsonf_core_new(const jsonf_options *opts);  /* NULL opts = defaults. NULL if invalid */
void         jsonf_core_free(jsonf_core *core);          /* does not free any trees */
const char  *jsonf_last_error(const jsonf_core *core);   /* NULL if no error */
void         jsonf_free(void *p);
int          jsonf_api_version(void);

/* Create: returns an owned root. The name can be NULL */
jsonf_status jsonf_new_object(jsonf_core *core, const char *name, jsonf_value **out);
jsonf_status jsonf_new_array(jsonf_core *core, const char *name, jsonf_value **out);
jsonf_status jsonf_new_null(jsonf_core *core, const char *name, jsonf_value **out);
jsonf_status jsonf_new_logical(jsonf_core *core, const char *name, int v, jsonf_value **out);
jsonf_status jsonf_new_integer(jsonf_core *core, const char *name, int v, jsonf_value **out);
jsonf_status jsonf_new_real(jsonf_core *core, const char *name, double v, jsonf_value **out);
jsonf_status jsonf_new_string(jsonf_core *core, const char *name,
                              const char *s, size_t n, jsonf_value **out);

/* Tree structure */
jsonf_status jsonf_add(jsonf_core *core, jsonf_value *parent, jsonf_value *child);
jsonf_status jsonf_detach(jsonf_core *core, jsonf_value *node);
jsonf_status jsonf_destroy(jsonf_core *core, jsonf_value *node);
jsonf_status jsonf_clone(jsonf_core *core, const jsonf_value *node, jsonf_value **out);
jsonf_status jsonf_rename(jsonf_core *core, jsonf_value *node, const char *name);

/* Inspect and navigate: results are borrowed, and JSONF_NOT_FOUND ends a walk */
jsonf_status jsonf_info(jsonf_core *core, const jsonf_value *node,
                        jsonf_type *type, int *n_children);
jsonf_status jsonf_name(jsonf_core *core, const jsonf_value *node, char **s, size_t *n);
jsonf_status jsonf_first_child(jsonf_core *core, const jsonf_value *node, jsonf_value **out);
jsonf_status jsonf_next(jsonf_core *core, const jsonf_value *node, jsonf_value **out);
jsonf_status jsonf_parent(jsonf_core *core, const jsonf_value *node, jsonf_value **out);
jsonf_status jsonf_child_by_name(jsonf_core *core, const jsonf_value *node,
                                 const char *name, jsonf_value **out);
jsonf_status jsonf_child_by_index(jsonf_core *core, const jsonf_value *node,
                                  int i0, jsonf_value **out);  /* 0-based */
jsonf_status jsonf_get_by_path(jsonf_core *core, const jsonf_value *node,
                               const char *path, jsonf_value **out);

/* Get: the path can be NULL to read the node itself */
jsonf_status jsonf_get_logical(jsonf_core *core, const jsonf_value *node,
                               const char *path, int *val);
jsonf_status jsonf_get_integer(jsonf_core *core, const jsonf_value *node,
                               const char *path, int *val);
jsonf_status jsonf_get_real(jsonf_core *core, const jsonf_value *node,
                            const char *path, double *val);
jsonf_status jsonf_get_string(jsonf_core *core, const jsonf_value *node,
                              const char *path, char **s, size_t *n);

/*
 * Set: the path is required. Missing variables are created and existing ones
 * are overwritten. Not available with path_mode = 2 (RFC 6901).
 */
jsonf_status jsonf_set_logical(jsonf_core *core, jsonf_value *node, const char *path, int val);
jsonf_status jsonf_set_integer(jsonf_core *core, jsonf_value *node, const char *path, int val);
jsonf_status jsonf_set_real(jsonf_core *core, jsonf_value *node, const char *path, double val);
jsonf_status jsonf_set_string(jsonf_core *core, jsonf_value *node, const char *path,
                              const char *s, size_t n);
jsonf_status jsonf_set_null(jsonf_core *core, jsonf_value *node, const char *path);

/*
 * Vectors: the path can be NULL to read the node itself. The length is always
 * returned in n. Pass buf = NULL to only get the length. If the buffer is too
 * small, nothing is copied and JSONF_INVALID_ARGUMENT is returned.
 */
jsonf_status jsonf_get_real_vec(jsonf_core *core, const jsonf_value *node, const char *path,
                                double *buf, size_t cap, size_t *n);
jsonf_status jsonf_get_integer_vec(jsonf_core *core, const jsonf_value *node, const char *path,
                                   int *buf, size_t cap, size_t *n);

#ifdef __cplusplus
}
#endif

#endif /* JSONFORTRAN_H */
