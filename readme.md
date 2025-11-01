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

## Missions

- TUI
  - SCREEN SECTION for defining display layouts
  - ACCEPT/DISPLAY with screen positioning
- HTTP/1.1 Communication
  - COBOL is oooooooooold and does not support TCP intrinsically
  - Options:
    - Directly pass into shell and execute with curl or another CLI HTTP client
    - Use sockets via C bindings
    - File-based IPC
    - ``CBL_SOCKET_*` with GnuCOBOL if i can find it compiled with socket support
  - Implementing tcp sounds interesting [https://sourceforge.net/p/gnucobol/discussion/contrib/thread/2b474086/](https://sourceforge.net/p/gnucobol/discussion/contrib/thread/2b474086/) [https://github.com/OCamlPro/gnucobol-contrib/](https://github.com/OCamlPro/gnucobol-contrib/) [https://github.com/OCamlPro/gnucobol-contrib/tree/master/samples/socket](https://github.com/OCamlPro/gnucobol-contrib/tree/master/samples/socket)
- Project structure
  - The idea is that i can expand the TUI to fit more usecases, so a proper structure for the COBOL application needs to be in order
  - Modularization
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

## Line Endings and Formatting

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

## Installation

### Installing GnuCOBOL and curl

You need the GnuCOBOL compiler to build this project and a CLI HTTP client such as `curl` for the curl-based HTTP client.

#### Arch Linux

```bash
yay -S gnucobol curl
```

#### Debian/Ubuntu

```bash
sudo apt-get update
sudo apt-get install gnucobol curl
```

#### Fedora/RHEL

```bash
sudo dnf install gnucobol curl
```

#### macOS

```bash
brew install gnucobol curl
```

#### From Source

Download from [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/) and follow the build instructions in the tarball.

Download curl from [https://curl.se/](https://curl.se/) or build from source if you need a custom build.

**Verify installation:**

```bash
cobc -v
curl --version
```

You should see version information if GnuCOBOL and curl are installed correctly.

### Installing the TUI

Not implemented yet. When it is, there will probably be a package or install script here.

`¯\_(ツ)_/¯`

## Usage

```bash
make        # Compile to build/app
make run    # Run the app
make clean  # Clean build artifacts
```

### Run temp nim server

`nim c -r .\test_server.nim`

## Development Notes

Use an editor that shows column numbers. Getting the column positioning wrong is the most common source of compilation errors in fixed-format COBOL.

## HTTP client logging (verbosity)

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
