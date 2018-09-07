-module(eva_test).

-export([test/0]).

test() ->
{ok, PID} = gen_event:start_link(),
gen_event:add_handler(PID, event_machine, []),
gen_event:add_handler(PID, broken_event_machine, []),
gen_event:notify(PID, {substract, 900, 541}),
gen_event:notify(PID, {summ, 491, 606}),
gen_event:notify(PID, {multiply, 14, 89}),
gen_event:notify(PID, {divide, 19854, 45}),
gen_event:notify(PID, {power, 2, 32}),
ok.
