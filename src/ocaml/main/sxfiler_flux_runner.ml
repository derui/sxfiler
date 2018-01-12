module State = Sxfiler_common.Std.State
module F = Flux_frp.Std

include F.Flux.Make.Default_runner(State)
