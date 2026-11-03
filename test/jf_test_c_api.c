/*
 * Test of the JSON-Fortran C interface.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "jsonfortran.h"

static int n_failed = 0;

#define CHECK(cond)                                                \
    do {                                                           \
        if (!(cond)) {                                             \
            printf("FAILED: %s:%d: %s\n", __FILE__, __LINE__, #cond); \
            n_failed++;                                            \
        }                                                          \
    } while (0)

#define CHECK_OK(call) CHECK((call) == JSONF_OK)

/* check a child's type, and check that its name is correct */
static void check_child(jsonf_core *core, jsonf_value *child, const char *name, jsonf_type type)
{
    jsonf_type t = JSONF_NULL;
    char *s = NULL;
    size_t n = 0;

    CHECK_OK(jsonf_info(core, child, &t, NULL));
    CHECK(t == type);
    CHECK_OK(jsonf_name(core, child, &s, &n));
    CHECK(n == strlen(name));
    CHECK(strcmp(s, name) == 0);
    jsonf_free(s);
}

/* build {"i":42,"r":2.5,"l":1,"n":null,"s":"hello","a":[1,2,3]} from nodes */
static jsonf_value *build(jsonf_core *core)
{
    jsonf_value *root = NULL, *p = NULL, *a = NULL;
    int i;

    CHECK_OK(jsonf_new_object(core, NULL, &root));
    CHECK_OK(jsonf_new_integer(core, "i", 42, &p));
    CHECK_OK(jsonf_add(core, root, p));
    CHECK_OK(jsonf_new_real(core, "r", 2.5, &p));
    CHECK_OK(jsonf_add(core, root, p));
    CHECK_OK(jsonf_new_logical(core, "l", 1, &p));
    CHECK_OK(jsonf_add(core, root, p));
    CHECK_OK(jsonf_new_null(core, "n", &p));
    CHECK_OK(jsonf_add(core, root, p));
    CHECK_OK(jsonf_new_string(core, "s", "hello", 5, &p));
    CHECK_OK(jsonf_add(core, root, p));
    CHECK_OK(jsonf_new_array(core, "a", &a));
    CHECK_OK(jsonf_add(core, root, a));
    for (i = 1; i <= 3; i++) {
        CHECK_OK(jsonf_new_integer(core, NULL, i, &p));
        CHECK_OK(jsonf_add(core, a, p));
    }
    return root;
}

static void test_lifecycle(void)
{
    jsonf_options opts;
    jsonf_core *core;

    CHECK(jsonf_api_version() == 1);

    jsonf_options_default(&opts);
    CHECK(opts.case_sensitive_keys == 1);
    CHECK(opts.strict_type_checking == 0);
    CHECK(opts.path_mode == 1);

    core = jsonf_core_new(&opts);
    CHECK(core != NULL);
    CHECK(jsonf_last_error(core) == NULL);
    jsonf_core_free(core);

    opts.path_mode = 99;
    CHECK(jsonf_core_new(&opts) == NULL);

    jsonf_core_free(NULL);
    jsonf_free(NULL);
    CHECK(jsonf_last_error(NULL) == NULL);
    CHECK(jsonf_new_object(NULL, NULL, NULL) == JSONF_INVALID_ARGUMENT);
}

static void test_walk(jsonf_core *core)
{
    static const char *names[] = {"i", "r", "l", "n", "s", "a"};
    static const jsonf_type types[] = {JSONF_INTEGER, JSONF_REAL, JSONF_LOGICAL,
                                       JSONF_NULL, JSONF_STRING, JSONF_ARRAY};
    jsonf_value *root = build(core);
    jsonf_value *p = NULL, *q = NULL;
    jsonf_status st;
    jsonf_type t;
    int n = -1, i = 0, ival = 0;
    double r = 0.0;
    char *s = NULL;
    size_t len = 0;

    CHECK_OK(jsonf_info(core, root, &t, &n));
    CHECK(t == JSONF_OBJECT);
    CHECK(n == 6);

    for (st = jsonf_first_child(core, root, &p); st == JSONF_OK; st = jsonf_next(core, p, &p)) {
        CHECK(i < 6);
        check_child(core, p, names[i], types[i]);
        CHECK_OK(jsonf_parent(core, p, &q));
        CHECK(q == root);
        i++;
    }
    CHECK(st == JSONF_NOT_FOUND);
    CHECK(i == 6);
    CHECK(p == NULL);
    CHECK(jsonf_last_error(core) == NULL);

    CHECK(jsonf_parent(core, root, &q) == JSONF_NOT_FOUND);
    CHECK(q == NULL);

    CHECK_OK(jsonf_child_by_name(core, root, "s", &p));
    CHECK_OK(jsonf_get_string(core, p, NULL, &s, &len));
    CHECK(len == 5);
    CHECK(strcmp(s, "hello") == 0);
    jsonf_free(s);
    CHECK_OK(jsonf_first_child(core, root, &q));
    CHECK(jsonf_first_child(core, q, &q) == JSONF_NOT_FOUND);

    CHECK_OK(jsonf_child_by_index(core, root, 0, &p));
    check_child(core, p, "i", JSONF_INTEGER);
    CHECK_OK(jsonf_child_by_index(core, root, 5, &p));
    check_child(core, p, "a", JSONF_ARRAY);
    CHECK(jsonf_child_by_index(core, root, 6, &q) == JSONF_NOT_FOUND);
    CHECK(jsonf_child_by_index(core, root, -1, &q) == JSONF_NOT_FOUND);
    CHECK(jsonf_child_by_name(core, root, "missing", &q) == JSONF_NOT_FOUND);
    CHECK(jsonf_child_by_name(core, p, "x", &q) == JSONF_NOT_FOUND);
    CHECK(jsonf_last_error(core) == NULL);

    CHECK_OK(jsonf_get_integer(core, root, "i", &ival));
    CHECK(ival == 42);
    CHECK_OK(jsonf_get_real(core, root, "r", &r));
    CHECK(r == 2.5);
    CHECK_OK(jsonf_get_logical(core, root, "l", &ival));
    CHECK(ival == 1);
    CHECK_OK(jsonf_get_integer(core, root, "a(2)", &ival));
    CHECK(ival == 2);
    CHECK_OK(jsonf_get_by_path(core, root, "a(3)", &p));
    CHECK_OK(jsonf_get_integer(core, p, NULL, &ival));
    CHECK(ival == 3);

    CHECK(jsonf_get_integer(core, root, "missing", &ival) == JSONF_NOT_FOUND);
    CHECK(jsonf_get_by_path(core, root, "missing", &p) == JSONF_NOT_FOUND);
    CHECK(p == NULL);
    CHECK(jsonf_last_error(core) == NULL);

    CHECK_OK(jsonf_destroy(core, root));
}

static void test_destroy_detach_clone(jsonf_core *core)
{
    jsonf_value *root = build(core);
    jsonf_value *p = NULL, *copy = NULL, *a = NULL;
    int n = 0, ival = 0;

    /* a middle child: the later siblings must survive */
    CHECK_OK(jsonf_child_by_name(core, root, "r", &p));
    CHECK_OK(jsonf_destroy(core, p));
    CHECK_OK(jsonf_info(core, root, NULL, &n));
    CHECK(n == 5);
    CHECK_OK(jsonf_get_integer(core, root, "i", &ival));
    CHECK(ival == 42);
    CHECK(jsonf_get_real(core, root, "r", NULL) == JSONF_NOT_FOUND);
    CHECK_OK(jsonf_child_by_index(core, root, 1, &p));
    check_child(core, p, "l", JSONF_LOGICAL);

    /* the last child */
    CHECK_OK(jsonf_child_by_name(core, root, "a", &a));
    CHECK_OK(jsonf_detach(core, a));
    CHECK_OK(jsonf_info(core, root, NULL, &n));
    CHECK(n == 4);
    CHECK(jsonf_parent(core, a, &p) == JSONF_NOT_FOUND);
    CHECK(jsonf_next(core, a, &p) == JSONF_NOT_FOUND);

    /* a detached node can be added again */
    CHECK_OK(jsonf_add(core, root, a));
    CHECK_OK(jsonf_info(core, root, NULL, &n));
    CHECK(n == 5);

    /* a clone of a child is independent, and has no parent or siblings */
    CHECK_OK(jsonf_child_by_name(core, root, "i", &p));
    CHECK_OK(jsonf_clone(core, p, &copy));
    CHECK(copy != p);
    CHECK(jsonf_parent(core, copy, &a) == JSONF_NOT_FOUND);
    CHECK(jsonf_next(core, copy, &a) == JSONF_NOT_FOUND);
    CHECK_OK(jsonf_destroy(core, copy));

    CHECK_OK(jsonf_clone(core, root, &copy));
    CHECK_OK(jsonf_set_integer(core, copy, "i", 7));
    CHECK_OK(jsonf_get_integer(core, root, "i", &ival));
    CHECK(ival == 42);
    CHECK_OK(jsonf_get_integer(core, copy, "i", &ival));
    CHECK(ival == 7);
    CHECK_OK(jsonf_get_integer(core, copy, "a(3)", &ival));
    CHECK(ival == 3);
    CHECK_OK(jsonf_destroy(core, root));
    CHECK_OK(jsonf_get_integer(core, copy, "a(1)", &ival));
    CHECK(ival == 1);
    CHECK_OK(jsonf_destroy(core, copy));
}

static void test_add_guards(jsonf_core *core)
{
    jsonf_value *root = build(core);
    jsonf_value *other = NULL, *p = NULL, *a = NULL;
    int n = 0;

    CHECK_OK(jsonf_new_object(core, NULL, &other));

    /* already has a parent */
    CHECK_OK(jsonf_child_by_name(core, root, "i", &p));
    CHECK(jsonf_add(core, other, p) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_last_error(core) != NULL);

    /* self */
    CHECK(jsonf_add(core, other, other) == JSONF_INVALID_ARGUMENT);

    /* cycle: a root added below itself */
    CHECK_OK(jsonf_child_by_name(core, root, "a", &a));
    CHECK(jsonf_add(core, a, root) == JSONF_INVALID_ARGUMENT);

    /* not a container */
    CHECK_OK(jsonf_new_integer(core, NULL, 1, &a));
    CHECK(jsonf_add(core, p, a) == JSONF_TYPE_ERROR);
    CHECK_OK(jsonf_destroy(core, a));

    /* NULL handles */
    CHECK(jsonf_add(core, NULL, other) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_add(core, other, NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_destroy(core, NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_new_object(core, NULL, NULL) == JSONF_INVALID_ARGUMENT);

    /* the structures are unchanged */
    CHECK_OK(jsonf_info(core, root, NULL, &n));
    CHECK(n == 6);
    CHECK_OK(jsonf_info(core, other, NULL, &n));
    CHECK(n == 0);

    CHECK_OK(jsonf_destroy(core, other));
    CHECK_OK(jsonf_destroy(core, root));
}

static void test_errors(jsonf_core *core)
{
    jsonf_value *root = build(core);
    int ival = 0;
    char *s = NULL;

    /* an error has a message, and the next call clears it */
    CHECK(jsonf_get_integer(core, root, "a", &ival) == JSONF_TYPE_ERROR);
    CHECK(jsonf_last_error(core) != NULL);
    CHECK(strlen(jsonf_last_error(core)) > 0);
    CHECK_OK(jsonf_get_integer(core, root, "i", &ival));
    CHECK(ival == 42);
    CHECK(jsonf_last_error(core) == NULL);

    CHECK(jsonf_get_string(core, root, "a", &s, NULL) == JSONF_TYPE_ERROR);
    CHECK(s == NULL);
    CHECK(jsonf_get_integer(core, root, "i", NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_get_string(core, root, "s", NULL, NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_get_by_path(core, root, NULL, NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_set_integer(core, root, NULL, 1) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_rename(core, root, NULL) == JSONF_INVALID_ARGUMENT);
    CHECK(jsonf_set_string(core, root, "x", NULL, 3) == JSONF_INVALID_ARGUMENT);

    CHECK_OK(jsonf_destroy(core, root));
}

static void test_set(jsonf_core *core)
{
    jsonf_value *root = NULL, *p = NULL;
    jsonf_type t;
    int ival = 0;
    double r = 0.0;
    char *s = NULL;
    size_t len = 0;

    CHECK_OK(jsonf_new_object(core, NULL, &root));

    /* creates what is missing */
    CHECK_OK(jsonf_set_integer(core, root, "a.b.c", 5));
    CHECK_OK(jsonf_get_integer(core, root, "a.b.c", &ival));
    CHECK(ival == 5);
    CHECK_OK(jsonf_set_real(core, root, "a.r", 1.5));
    CHECK_OK(jsonf_get_real(core, root, "a.r", &r));
    CHECK(r == 1.5);
    CHECK_OK(jsonf_set_logical(core, root, "a.l", 7));
    CHECK_OK(jsonf_get_logical(core, root, "a.l", &ival));
    CHECK(ival == 1);
    CHECK_OK(jsonf_set_string(core, root, "a.s", "abc", 3));
    CHECK_OK(jsonf_set_null(core, root, "a.n"));
    CHECK_OK(jsonf_get_by_path(core, root, "a.n", &p));
    CHECK_OK(jsonf_info(core, p, &t, NULL));
    CHECK(t == JSONF_NULL);

    /* same type: the node is updated in place */
    CHECK_OK(jsonf_get_by_path(core, root, "a.b.c", &p));
    CHECK_OK(jsonf_set_integer(core, root, "a.b.c", 6));
    CHECK_OK(jsonf_get_integer(core, p, NULL, &ival));
    CHECK(ival == 6);

    /* different type: the node is replaced, but keeps its name and position */
    CHECK_OK(jsonf_set_string(core, root, "a.b.c", "text", 4));
    CHECK_OK(jsonf_get_by_path(core, root, "a.b.c", &p));
    check_child(core, p, "c", JSONF_STRING);
    CHECK_OK(jsonf_get_string(core, root, "a.b.c", &s, &len));
    CHECK(len == 4);
    CHECK(strcmp(s, "text") == 0);
    jsonf_free(s);

    /* strings are kept as is */
    CHECK_OK(jsonf_set_string(core, root, "e", NULL, 0));
    CHECK_OK(jsonf_get_string(core, root, "e", &s, &len));
    CHECK(len == 0);
    CHECK(strcmp(s, "") == 0);
    jsonf_free(s);
    CHECK_OK(jsonf_set_string(core, root, "t", " a b  ", 6));
    CHECK_OK(jsonf_get_string(core, root, "t", &s, &len));
    CHECK(len == 6);
    CHECK(strcmp(s, " a b  ") == 0);
    jsonf_free(s);

    /* only the given length is used */
    CHECK_OK(jsonf_set_string(core, root, "u", "abcdef", 2));
    CHECK_OK(jsonf_get_string(core, root, "u", &s, &len));
    CHECK(len == 2);
    CHECK(strcmp(s, "ab") == 0);
    jsonf_free(s);

    CHECK_OK(jsonf_get_by_path(core, root, "u", &p));
    CHECK_OK(jsonf_rename(core, p, "v"));
    CHECK(jsonf_get_by_path(core, root, "u", &p) == JSONF_NOT_FOUND);
    CHECK_OK(jsonf_get_by_path(core, root, "v", &p));

    CHECK_OK(jsonf_destroy(core, root));
}

static void test_options(void)
{
    jsonf_options opts;
    jsonf_core *core;
    jsonf_value *root = NULL;
    int ival = 0;

    jsonf_options_default(&opts);
    opts.case_sensitive_keys = 0;
    opts.strict_type_checking = 1;
    core = jsonf_core_new(&opts);
    CHECK(core != NULL);
    CHECK_OK(jsonf_new_object(core, NULL, &root));
    CHECK_OK(jsonf_set_string(core, root, "Key", "5", 1));
    CHECK_OK(jsonf_get_by_path(core, root, "KEY", &(jsonf_value *){NULL}));
    CHECK(jsonf_get_integer(core, root, "key", &ival) == JSONF_TYPE_ERROR);
    CHECK_OK(jsonf_destroy(core, root));
    jsonf_core_free(core);

    jsonf_options_default(&opts);
    opts.path_mode = 2;
    core = jsonf_core_new(&opts);
    CHECK(core != NULL);
    CHECK_OK(jsonf_new_object(core, NULL, &root));
    CHECK(jsonf_set_integer(core, root, "/a", 1) != JSONF_OK);
    CHECK(jsonf_last_error(core) != NULL);
    CHECK_OK(jsonf_destroy(core, root));
    jsonf_core_free(core);
}

static void test_vectors(jsonf_core *core)
{
    jsonf_value *root = build(core);
    jsonf_value *p = NULL, *e = NULL;
    int ibuf[3] = {0, 0, 0};
    int small[2] = {-1, -1};
    double rbuf[3] = {0.0, 0.0, 0.0};
    size_t n = 0;

    /* length query */
    CHECK_OK(jsonf_get_integer_vec(core, root, "a", NULL, 0, &n));
    CHECK(n == 3);

    CHECK_OK(jsonf_get_integer_vec(core, root, "a", ibuf, 3, &n));
    CHECK(n == 3);
    CHECK(ibuf[0] == 1 && ibuf[1] == 2 && ibuf[2] == 3);

    /* too small: nothing is copied, and the needed length is returned */
    CHECK(jsonf_get_integer_vec(core, root, "a", small, 2, &n) == JSONF_INVALID_ARGUMENT);
    CHECK(n == 3);
    CHECK(small[0] == -1 && small[1] == -1);
    CHECK(jsonf_last_error(core) != NULL);

    /* integers convert to reals, and the path can be NULL */
    CHECK_OK(jsonf_get_by_path(core, root, "a", &p));
    CHECK_OK(jsonf_get_real_vec(core, p, NULL, rbuf, 3, &n));
    CHECK(rbuf[0] == 1.0 && rbuf[1] == 2.0 && rbuf[2] == 3.0);
    CHECK(jsonf_get_real_vec(core, p, NULL, rbuf, 2, &n) == JSONF_INVALID_ARGUMENT);

    CHECK(jsonf_get_integer_vec(core, root, "missing", NULL, 0, &n) == JSONF_NOT_FOUND);
    CHECK(jsonf_get_integer_vec(core, root, "i", NULL, 0, &n) == JSONF_TYPE_ERROR);
    CHECK(jsonf_get_integer_vec(core, root, "a", ibuf, 3, NULL) == JSONF_INVALID_ARGUMENT);

    CHECK_OK(jsonf_new_array(core, "e", &e));
    CHECK_OK(jsonf_add(core, root, e));
    CHECK_OK(jsonf_get_real_vec(core, root, "e", rbuf, 3, &n));
    CHECK(n == 0);

    CHECK_OK(jsonf_destroy(core, root));
}

static void test_leaks(jsonf_core *core)
{
    int i;

    for (i = 0; i < 200; i++) {
        jsonf_value *root = build(core);
        jsonf_value *copy = NULL, *p = NULL;
        CHECK_OK(jsonf_clone(core, root, &copy));
        CHECK_OK(jsonf_child_by_name(core, root, "a", &p));
        CHECK_OK(jsonf_detach(core, p));
        CHECK_OK(jsonf_destroy(core, p));
        CHECK_OK(jsonf_destroy(core, copy));
        CHECK_OK(jsonf_destroy(core, root));
    }
}

int main(void)
{
    jsonf_core *core;

    test_lifecycle();

    core = jsonf_core_new(NULL);
    CHECK(core != NULL);
    test_walk(core);
    test_destroy_detach_clone(core);
    test_add_guards(core);
    test_errors(core);
    test_set(core);
    test_vectors(core);
    test_leaks(core);
    jsonf_core_free(core);

    test_options();

    if (n_failed > 0) {
        printf("%d checks failed\n", n_failed);
        return EXIT_FAILURE;
    }
    printf("all checks passed\n");
    return EXIT_SUCCESS;
}
