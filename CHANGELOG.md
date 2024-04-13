# Changelog

### Version 2.0.0
* renamed `argparse` module to `args`, to avoid name clash with OTP 26

### Version 1.2.4:
* minor spec fixes

### Version 1.2.3:
* implemented global default
* minor bugfixes

### Version 1.2.1:
* minor bugfixes, support for choices of atoms

### Version 1.2.0:
* CLI incompatible change: `cli:run/1,2` by default now calls halt(1) in case of a parser error
* bugfixes

### Version 1.1.3:
* added `help => hidden` for commands and options
* changed default formatting for better readability
* fixed bug causing `nargs => all` to be ignored

### Version 1.1.2:
* support for "--arg=value" form

### Version 1.1.1:
* added templates for help text

### Version 1.1.0:
* Handler support for minimal CLI
* cli/1 optional behaviour callback deprecated
* Ability to provide progname as an atom

### Version 1.0.2:
* Improved documentation
* Validation run for CLI commands

### Version 1.0.0:
* First public release

Version 0.1.0:
* First usable version (API is unstable yet)
