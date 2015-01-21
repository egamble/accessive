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

`get-in-json` is analogous to get-in, but extracts a substring from a JSON in string form. `keys` are indices, keywords or strings.

```clojure
(get-tree-in-json json-string key-tree)
(get-tree-in-json json-string key-tree read-fn)
```

`get-tree-in-json` extracts multiple substrings simultaneously from a JSON in string form. `key-tree` is a map of keys, whose values are maps of keys, etc., with nils or empty maps at the leaves. The keys are indices, keywords or strings. `read-fn` is a fn to apply to each extracted string, e.g. `read-string` or `json/parse-string`. `read-fn` defaults to `identity`.

A program using `get-tree-in-json` should probably call `shutdown-agents` before exiting to avoid a long timeout.

### Updates:

#### Version 1.0.0:

Initial version.
