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

## Missions

- TUI
    - SCREEN SECTION for defining display layouts
    - ACCEPT/DISPLAY with screen positioning
- HTTP Communication
    - Needs to be implemented since COBOL is oooooooooold 
    - Socket Programming with GnuCOBOL
    - _Optionanally_: File-based IPC (It is suppositly simpler to implement)
- Async/Await
    - COBOl is synchronous ...
    - To implement it i need some kind of threading or polling. Threading is not standard to COBOL
    - File-based comms is yet again an option
- Project structure
    - The idea is that i can expand the TUI to fit more usecases, so a proper structure for the COBOL application needs to be in order

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

### Installing GnuCOBOL and GNU Netcat

You need the GnuCOBOL compiler to build this project.

#### Arch Linux
```bash
yay -S gnucobol gnu-netcat
```

#### Debian/Ubuntu
```bash
sudo apt-get update
sudo apt-get install gnucobol gnu-netcat
```

#### Fedora/RHEL
```bash
sudo dnf install gnucobol gnu-netcat
```

#### macOS
```bash
brew install gnucobol gnu-netcat
```

#### From Source
Download from [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/) and follow the build instructions in the tarball.

Download from [netcat.sourceforge.io](https://netcat.sourceforge.net/) and follow the build instructions in the tarball.

**Verify installation:**
```bash
cobc -v
nc --version
```

You should see version information if GnuCOBOL is installed correctly.

### Installing the TUI

Not implemented yet. When it is, there will probably be a package or install script here.

`¯\_(ツ)_/¯`

## Usage

```bash
make        # Compile to build/app
make run    # Run the app
make clean  # Clean build artifacts
```

## Development Notes

Use an editor that shows column numbers. Getting the column positioning wrong is the most common source of compilation errors in fixed-format COBOL.