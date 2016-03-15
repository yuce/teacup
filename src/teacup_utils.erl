-module(teacup_utils).

-export([exported_functions/1]).

%% == API

exported_functions(Module) ->
    Exported = Module:module_info(exports),
    sets:from_list(Exported).

