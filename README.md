# 3b-openxr
More-or-less working [OpenXR](https://www.khronos.org/openxr/) bindings + wrappers.

API isn't completely finalized, in particular some functions that
return plists might be changed to return structs or CLOS instances,
and some things may be changed to fill in existing data structures
instead of consing new data on every call. Might also store current
session in a special var and make `session` argument implicit.  See
[issue 1](https://github.com/3b/3b-openxr/issues/1) for discussion,
and add comments for things you would like to see changed (or not
changed) if you intend to use 3b-openxr seriously.

Contains generated low-level binding for (almost) everything in
1.0.27, and wrappers for most of core and some extensions.

Tested with SteamVR's OpenXR runtime on Win32 with OpenGL. Other
graphics/windowing APIs aren't implemented yet, but should be simple
to add.

see https://github.com/3b/3b-openxr-example for usage examples.


Supported API packages:
* `3b-openxr:` is the main high-level API, suggested local nickname `xr:`.
  Ideally shouldn't require use of CFFI or foreign pointers, and tries to be fairly safe and restartable.
* `3b-openxr-bindings:` contains generated low-level bindings, suggested local nickname `%xr:` (3b-openxr uses `%:` internally).
  Direct CFFI bindings with minimal translation (just the default translations for most things, aside from translating some vector types and similar to typed CL vectors instead of keeping the `.x`,`.y`,`.z` etc slots)
* `3b-openxr-mid-level:` contains generated macros for accessing foreign structs more easily.
  Hides most of the CFFI stuff for those structs, adds some defaults, etc.
  For output structs, or when more control is needed, set `:%slots t` and it will bind all the slots with `cffi:with-foreign-slots` as `%xr:some-slot`.
  Used by most of the wrappers, so see those for examples.

Internal package:
* `3b-openxr-wrappers:` contains implementation of the high-level API. Originally separated from `3b-openxr:` due to symbol conflicts, not sure if it actually still needs to be separate or not. Users shouldn't need to use anything from here directly, should all be re-exported from `3b-openxr:`. If some re-exports were missed please file an issue or send a pr.


General usage:
see [example.lisp](./example.lisp)