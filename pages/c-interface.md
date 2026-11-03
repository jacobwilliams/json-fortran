title: C Interface

JSON-Fortran has a C interface for creating, navigating, modifying and destroying
JSON structures. The declarations are in `src/jsonfortran.h`.

It is currently built and tested with [fpm](https://fpm.fortran-lang.org) only, and it
requires the default kinds. Builds using `USE_UCS4`, or a non-default real or integer
kind, leave it out.

# Example

```c
#include <stdio.h>
#include "jsonfortran.h"

int main(void)
{
    jsonf_core *core = jsonf_core_new(NULL);
    jsonf_value *root = NULL, *child = NULL;
    jsonf_status st;
    int year = 0;

    jsonf_new_object(core, NULL, &root);
    jsonf_set_integer(core, root, "ship.year", 1805);
    jsonf_set_string(core, root, "ship.name", "Victory", 7);

    if (jsonf_get_integer(core, root, "ship.year", &year) == JSONF_OK) {
        printf("year = %d\n", year);
    }

    /* walk the children of an object or array */
    jsonf_get_by_path(core, root, "ship", &child);
    for (st = jsonf_first_child(core, child, &child); st == JSONF_OK;
         st = jsonf_next(core, child, &child)) {
        char *name = NULL;
        jsonf_name(core, child, &name, NULL);
        printf("%s\n", name);
        jsonf_free(name);
    }

    jsonf_destroy(core, root);
    jsonf_core_free(core);
    return 0;
}
```

# Rules

**Ownership**

* `jsonf_new_*` and `jsonf_clone` return a root that you own. Free it with
  `jsonf_destroy`.
* Lookups return borrowed nodes, which are valid until the node or one of its ancestors
  is destroyed.
* `jsonf_add` gives the child to the parent. It rejects a child that already has a
  parent, and a parent that is inside the child.
* `jsonf_detach` removes a node from its parent, and you then own it.
* `jsonf_destroy` works on any node, and leaves its siblings in place.
* `jsonf_set_*` replaces an existing node if the type changes, so any handle to that
  node is no longer valid.

**Errors**

* Most functions return a `jsonf_status`. Each call clears the previous error.
* `jsonf_last_error` returns the message, or `NULL` if there is no error. The string is
  valid until the next call using that core.
* `JSONF_NOT_FOUND` is not an error. It is also how a walk ends.

**Strings**

* Names and paths are null-terminated.
* String values are passed as a pointer and a length.
* Returned strings are null-terminated. Free them with `jsonf_free`.

**Threads**

Each core must only be used by one thread at a time.

# Limitations

* Integers are `int` and reals are `double`, with no range checks.
* Using a borrowed node after it is destroyed is undefined behavior.
* Text is expected to be ASCII. Other bytes are not validated.
* `jsonf_set_*` is not available with `path_mode = 2` (RFC 6901).
* JSON text can't be parsed or serialized yet, so structures are built and read through
  the API.
