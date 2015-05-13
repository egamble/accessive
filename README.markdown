The jar is available at https://clojars.org/egamble/accessive.

__accessive__ _adjective_ \ak-ˈse-siv\

1. Misspelling of __excessive__.
2. (_archaic_) __additional__.
3. (_rare_) Relating to an act of access.

Fast extraction from JSON strings. For a small number of values extracted from a large JSON, this can be faster than parsing with Cheshire.

### Usage

```clojure
(get-in-json json-string keys)
(get-in-json json-string keys not-found)
```

`get-in-json` is analogous to `clojure.core/get-in`, but extracts a substring from a JSON in string form. `keys` are indices, keywords or strings.

```clojure
(get-tree-in-json json-string key-tree)
(get-tree-in-json json-string key-tree read-fn)
```

`get-tree-in-json` extracts multiple substrings simultaneously from a JSON in string form. `key-tree` is a map of keys, whose values are maps of keys, etc., with nils or empty maps at the leaves. The keys are indices, keywords or strings. `read-fn` is a fn to apply to each extracted string, e.g. `read-string` or `json/parse-string`. `read-fn` defaults to `identity`.

Extracted strings are returned as the leaves of a tree of maps with the same structure as `key-tree`, except that subtrees are omitted when their keys are not found in the JSON.

```clojure
(get-lazy-tree-in-json json-string key-tree)
(get-lazy-tree-in-json json-string key-tree read-fn)
```

`get-lazy-tree-in-json` has almost the same behavior as `get-tree-in-json`, but returns the tree of results immediately. Values in the tree are delays that are dereferenced automatically upon access. This potentially allows quicker access to some values extracted from very large JSONs.

Unlike `get-tree-in-json`, `get-lazy-tree-in-json` doesn't return an incomplete tree when keys are not found in the JSON.

The JSON strings passed to these functions are assumed to be in valid JSON format. Optimized for speed, these functions are not designed to fail particularly gracefully on invalid JSON strings, typically throwing an ArrayIndexOutOfBoundsException.

### History

#### Version 1.0.3:

Fixed whitespace bug introduced in previous version.

#### Version 1.0.2:

Removed unnecessary instantiation.

#### Version 1.0.1:

Added `get-lazy-tree-in-json`.

#### Version 1.0.0:

Initial version.

