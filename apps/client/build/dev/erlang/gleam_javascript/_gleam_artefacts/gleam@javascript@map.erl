-module(gleam@javascript@map).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([map_/2]).

-type map_(KWY, KWZ) :: any() | {gleam_phantom, KWY, KWZ}.


