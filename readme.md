# Stregsystem COBOL TUI
The very best TUI for the F-Klubben stregsystem written in GnuCOBOL with those nice punchard limitations.

**Status of project**: VERY early stage

# Writing in COBOL
One can write in two code format modes when comiling. with the --free flag or without it. This proejct tires to do it without.
This means that the code has to adhere to the lengh of punchcards!
- Columns 1-6: Sequence numbers (optional, usually blank)
- Column 7: Indicator area (* for comments, - for continuation, blank for code)
- Columns 8-11: Area A (division, section, paragraph names, level numbers 01-49)
- Columns 12-72: Area B (statements, clauses)
- Columns 73-80: Identification area (ignored, originally for card identification)

## EOL and stuff like that
It is a pain in the ass. So i have used the pre-commit framework to remember it for me.

```bash
pacman -S pre-commit # Or your preferred packagemanager (or pip, its python after all)
pre-commit install
pre-commit run
```

# Installing
## GnuCOBOL

| Source | Link | cmd |
| - | - | - |
| Sourceforge | https://gnucobol.sourceforge.io/ | |
| Arch. AUR | https://aur.archlinux.org/packages/gnucobol | `yay -S gnucobol` |


Verify install with: `cobc -v`

`¯\_(ツ)_/¯`

# Usage
``` bash
make        # compile to build/app
make run    # run the app
make clean  # clean build
```