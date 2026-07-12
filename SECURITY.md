# Security Policy

`GAMSr` will eventually execute an external GAMS process. Security-sensitive
code must follow these rules:

- never invoke a shell unless there is no safe alternative;
- pass command arguments as a vector;
- validate GAMS identifiers, solver names, and option names before execution;
- isolate runs in dedicated work directories;
- treat imported model files as untrusted;
- do not use `eval(parse())` on user input;
- do not support raw GAMS code injection in the MVP.

Report private security issues to the maintainer address once published. During
early development, avoid sharing secrets or licence material in issues.
