# Stregsystem COBOL TUI

A terminal user interface for the F-Klubben stregsystem, written in GnuCOBOL with those nice punchcard limitations.

**Project Status**: Functional

This thing is a love letter to old constraints with a modern API strapped on. It is supposed to be useful and a little weird.

## Quickstart

If you already have GnuCOBOL, `curl`, and `jq` installed:

```bash
make
make run
```

The UI will fetch everything live from the API. No local state, no mercy.

## Installation

### Installing GnuCOBOL, curl, jq

You need the GnuCOBOL compiler to build this project and a CLI HTTP client, here `curl`, for the curl-based HTTP client. JQ because im lazy and dont want to build a parser...

#### Arch Linux

```bash
yay -S gnucobol
pacman -S curl jq
```

#### Why not the cobol frontend for gcc?

Because I didn't know, that's why. Also it targets ANSI COBOL 85, and some IBM and MSV extensions, but i have no idea what I would do.

I love the quote from the documentation og the frontend, as to "why a COBOL compiler". It starts out with:

> The use of the term 'COBOL' should not be taken as a representation that the COBOL for GCC system is an accurate, functional or complete implementation of the COBOL language

[https://cobolforgcc.sourceforge.net/cobol_1.html#SEC1](https://cobolforgcc.sourceforge.net/cobol_1.html#SEC1)

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

### Installing the TUI

User-local install (no sudo, no drama):

```bash
bash ./install.sh
```

This drops a `stregsystemcobtui` binary in `~/.local/bin`.

Uninstall:

```bash
bash ./uninstall.sh
```

This only removes the binary; your config stays put.

If you want to nuke config too:

```bash
bash ./uninstall.sh --purge
```

## Usage

```bash
make        # Compile to build/app
make run    # Run the app
make clean  # Clean build artifacts
```

### Configuration

Default API: `https://stregsystem.fklub.dk`

Set the API URL if you are not running that:

```bash
export STREGSYSTEM_URL="http://localhost:8000"
```

For HTTP client verbosity during tests:

```bash
export COB_HTTP_CLIENT_LOG=2
```

## Developer Notes (and other chaos)

The punchcard layout rules, formatting hooks, tests, and other weirdness live in [dev-notes.md](dev-notes.md).
