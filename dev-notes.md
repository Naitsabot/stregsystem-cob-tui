# Developer Notes (and other chaos)

This is the drawer where the project dumps all the dev-facing details that do not belong in the user-facing README. It is opinionated and has crumbs.

## COBOL fixed-format rules (yes, the punchcards are real)

This project uses fixed-format COBOL (compiled without the `--free` flag) to stick with traditional punchcard conventions.

### Punchcard column layout

Code must follow the 80-column punchcard format:

- Columns 1-6: sequence numbers (optional, usually blank)
- Column 7: indicator area
  - `*` = comment line
  - `-` = continuation of previous line
  - blank = regular code line
- Columns 8-11: Area A (division headers, section names, paragraph names, level numbers 01-49)
- Columns 12-72: Area B (statements, procedure code)
- Columns 73-80: identification area (ignored by compiler)

If your code is not positioned correctly within these columns, it will not compile.

```text
000000.AAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBIIIIIIII
```

### Procedures and data sections

Reference: [IBM COBOL procedures](https://www.ibm.com/docs/en/db2/11.5.x?topic=routines-cobol-procedures)

## Line endings and formatting

Fixed-format COBOL is picky about whitespace and line endings. The project uses pre-commit hooks to handle this automatically so you do not have to think about it.

```bash
# Install pre-commit
pacman -S pre-commit  # Or pip install pre-commit

# Set up the hooks
pre-commit install

# Run manually if needed
pre-commit run --all-files

# Test on just COBOL files
pre-commit run --files src/*.cob
```

## Testing

Run all tests:

```bash
make test
```

Or pick one:

```bash
make test-json          # Test basic JSON (older)
make test-api           # Test API integration
make test-product-dict  # Test product dictionary
```

API tests: `Status: +0000000000` = good.

### HTTP client logging (verbosity)

Control how much the HTTP client prints with `COB_HTTP_CLIENT_LOG` or via the Makefile variable `TEST_LOG`.

- 0 (none, default): no raw HTTP output; only high-level success/failure messages.
- 1 (minimal): show that requests were sent and whether they succeeded or failed.
- 2 (verbose): show the exact system command executed and the full HTTP response (headers + body).

Examples:

```bash
# default (no logging)
make test-api

# minimal
make test-api TEST_LOG=1

# verbose
make test-api TEST_LOG=2

# or call binary directly
COB_HTTP_CLIENT_LOG=2 ./build/test-api
```

Note: the client reads the env var once when first used; set it before running the program.

## Missions (a.k.a. why this exists)

- TUI
  - COBOLs SCREEN SECTION for defining display layouts
  - ACCEPT/DISPLAY with screen positioning
- HTTP/1.1 communication
  - COBOL is oooooooooold and does not support TCP intrinsically
  - Options:
    - Directly pass into shell and execute with curl or another CLI HTTP client
    - Use sockets via C bindings
    - File-based IPC
    - `CBL_SOCKET_*` with GnuCOBOL if it can be found compiled with socket support
    - Implementing TCP in COBOL sounds interesting
      - [GnuCOBOL contrib thread](https://sourceforge.net/p/gnucobol/discussion/contrib/thread/2b474086/)
      - [OCamlPro GnuCOBOL contrib](https://github.com/OCamlPro/gnucobol-contrib/)
      - [GnuCOBOL socket samples](https://github.com/OCamlPro/gnucobol-contrib/tree/master/samples/socket)
- DATA handling
  - JSON encoding/decoding
    - [Generating JSON with COBOL](https://eklausmeier.goip.de/blog/2021/08-17-generating-json-with-cobol/)
    - [Serialize/deserialize JSON in GnuCOBOL](https://compile7.org/serialize-and-deserialize/how-to-serialize-and-deserialize-json-in-gnucobol-cgi/)
    - [COBOL JSON examples](https://learnxbyexample.com/cobol/json/)
  - Nothing is saved locally; all data is fetched from the API on demand
- Project structure
  - Needs to be expandable for multiple use cases
    - Separate files for different modules
    - Clear interfaces between modules
- Build system
  - Makefile to automate compilation and linking
  - Handling dependencies
- Testing
  - Unit tests for individual modules
  - Integration tests for the whole application
- Documentation
  - Code comments
  - User manual for the TUI application

## Progress

- [x] TUI
  - [x] The bare necessary TUI layout with SCREEN SECTION
  - [x] A better menued TUI layout with SCREEN SECTION
- [x] HTTP/1.1 client integrated with stregsystem API
  - [x] System executable HTTP client implementation
  - [ ] COBOL tcp HTTP client (if feasible)
- [x] JSON parsing and serialization
  - [x] JSON encoding using manual string building
  - [x] JSON decoding using jq.exe external parser
  - [-] JSON generation using JSON GENERATE (optional enhancement)
  - [-] JSON parsing using JSON PARSE (unsupported by gnuCOBOL :((( )
  - [x] Integrate JSON into API

## HTTP client notes

As of now, two implementations have been made: one using netcat and the current using curl. The reason for the shift from nc to curl is that nc operates in the transportation layer while curl operates in the application layer, and the project already uses an out of the box implementation of an HTTP client, so this makes it easier to implement.

## JSON encoding and decoding

- Encoding: simple string concatenation in COBOL (see [src/json.cob](../src/json.cob))
- Decoding: uses `jq` as external JSON parser (see [src/json-decoder.cob](../src/json-decoder.cob))

The implementation is API-specific for the stregsystem endpoints.

## Docs and references

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [GnuCOBOL Programmer’s Guide](https://gnucobol.sourceforge.io/HTML/gnucobolpg.html)
- [DevDocs for GnuCOBOL Programmer’s Guide](https://devdocs.io/gnu_cobol/)
