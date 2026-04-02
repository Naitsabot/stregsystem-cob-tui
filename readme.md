# Stregsystem COBOL TUI

A terminal user interface for the F-Klubben stregsystem, written in GnuCOBOL with those nice punchcard limitations.

**Project Status**: Very early stage

## Writing in COBOL

This project uses fixed-format COBOL (compiled without the `--free` flag) to stick with traditional punchcard conventions.

### Punchcard Column Layout

Code must follow the 80-column punchcard format:

- **Columns 1-6**: Sequence numbers (optional, usually blank)
- **Column 7**: Indicator area
  - `*` = Comment line
  - `-` = Continuation of previous line
  - blank = Regular code line
- **Columns 8-11**: Area A (division headers, section names, paragraph names, level numbers 01-49)
- **Columns 12-72**: Area B (statements, procedure code)
- **Columns 73-80**: Identification area (ignored by compiler)

If your code isn't positioned correctly within these columns, it won't compile.

```text
000000.AAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBIIIIIIII
```

### Some documentation

- GnuCOBOL Manual: [https://gnucobol.sourceforge.io/doc/gnucobol.html](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- GnuCOBOL Programmer’s Guide: [https://gnucobol.sourceforge.io/HTML/gnucobpg.html](https://gnucobol.sourceforge.io/HTML/gnucobpg.html)
- DevDocs of GnuCOBOL Programmer’s Guide: [https://devdocs.io/gnu_cobol/](https://devdocs.io/gnu_cobol/)

### Procedures and data sections

params: [https://www.ibm.com/docs/en/db2/11.5.x?topic=routines-cobol-procedures](https://www.ibm.com/docs/en/db2/11.5.x?topic=routines-cobol-procedures)

### Line Endings and Formatting

Fixed-format COBOL is picky about whitespace and line endings. The project uses pre-commit hooks to handle this automatically so you don't have to think about it.

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

## Missions

- TUI
  - COBOLs SCREEN SECTION for defining display layouts
  - ACCEPT/DISPLAY with screen positioning
- HTTP/1.1 Communication
  - COBOL is oooooooooold and does not support TCP intrinsically
  - Options:
    - Directly pass into shell and execute with curl or another CLI HTTP client
    - Use sockets via C bindings
    - File-based IPC
    - ``CBL_SOCKET_*` with GnuCOBOL if i can find it compiled with socket support
    - Implementing TCP in COBOL sounds interesting [https://sourceforge.net/p/gnucobol/discussion/contrib/thread/2b474086/](https://sourceforge.net/p/gnucobol/discussion/contrib/thread/2b474086/) [https://github.com/OCamlPro/gnucobol-contrib/](https://github.com/OCamlPro/gnucobol-contrib/) [https://github.com/OCamlPro/gnucobol-contrib/tree/master/samples/socket](https://github.com/OCamlPro/gnucobol-contrib/tree/master/samples/socket)
- DATA handling
  - JSON encoding/decoding [https://eklausmeier.goip.de/blog/2021/08-17-generating-json-with-cobol/](https://eklausmeier.goip.de/blog/2021/08-17-generating-json-with-cobol/) [https://compile7.org/serialize-and-deserialize/how-to-serialize-and-deserialize-json-in-gnucobol-cgi/](https://compile7.org/serialize-and-deserialize/how-to-serialize-and-deserialize-json-in-gnucobol-cgi/) [https://learnxbyexample.com/cobol/json/](https://learnxbyexample.com/cobol/json/)
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

### Progress

- [ ] TUI
  - [ ] The bare necessary TUI layout with SCREEN SECTION
  - [ ] A better menued TUI layout with SCREEN SECTION
- [x] HTTP/1.1 client integrated with stregsystem API
  - [x] System executable HTTP client implementation
  - [ ] COBOL tcp HTTP client (if feasible)
- [x] JSON parsing and serialization
  - [x] JSON encoding using manual string building
  - [x] JSON decoding using jq.exe external parser
  - [-] JSON generation using JSON GENERATE (optional enhancement)
  - [-] JSON parsing using JSON PARSE (unsupported by gnuCOBOL :((( )
  - [x] Integrate JSON into API

#### HTTP/1.1 client

As of now, two implementations have been made: One using netcat and the current using curl. The reason for the shift from nc to curl is that nc operates in the transportation layer while curl operates in the application layer, and the project already uses an out of the box implementation of an http client, so this makes it easier to implement.

#### JSON Encoding and Decoding

**Encoding:** Simple string concatenation in COBOL (see [json.cob](src/json.cob))
**Decoding:** Uses `jq` as external JSON parser (see [json-decoder.cob](src/json-decoder.cob))

The implementation is API-specific for the stregsystem endpoints.

## Installation

### Installing GnuCOBOL, curl, jq

You need the GnuCOBOL compiler to build this project and a CLI HTTP client, here `curl`, for the curl-based HTTP client. JQ because im lazy and dont want to build a parser...

#### Arch Linux

```bash
yay -S gnucobol
pacman -S curl jq
```

#### From Source

Download GnuCOBOL compiler from [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/) and follow the build instructions in the tarball.

Download curl from [https://curl.se/](https://curl.se/) and build.

Download jq from [https://jqlang.org/](https://jqlang.org/) and build.

**Verify installation:**

```bash
cobc -v
curl --version
jq --version
```

You should see version information if all tools are installed correctly.

#### Why not the cobol frontend for gcc?

Becuase I didn't know, thats why. Also it targets ANSI COBOL 85, and som IBM and MSV extentions, but i have no idea what I would do.

[https://cobolforgcc.sourceforge.net/cobol_1.html#SEC2](https://cobolforgcc.sourceforge.net/cobol_1.html#SEC2)

### Installing the TUI

Not implemented yet. When it is, there will probably be a package or install script here.

`¯\_(ツ)_/¯`

## Usage

```bash
make        # Compile to build/app
make run    # Run the app
make clean  # Clean build artifacts
```

### Testing

Run tests with:

```bash
make test
```

or individual test files:

```bash
make test-json          # Test basic JSON (older)
make test-api           # Test API integration
make test-product-dict  # Test product dictionary
```

Api tests: `Status: +0000000000` = good

#### HTTP client logging (verbosity)

Control how much the HTTP client prints with the environment variable `COB_HTTP_CLIENT_LOG` or via the Makefile variable `TEST_LOG`.

- 0 (none, default): no raw HTTP output; only high-level success/failure messages.
- 1 (minimal): show that requests were sent and whether they succeeded or failed.
- 2 (verbose): show the exact system command executed and the full HTTP response (headers + body).

Usage examples:

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

## Development Notes

Use an editor that shows column numbers. Getting the column positioning wrong is the most common source of compilation errors in fixed-format COBOL.
