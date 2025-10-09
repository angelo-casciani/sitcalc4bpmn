PROLOG_HEADER = """
:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.

cache(_) :- fail.

id(X) :- ground(X), !.
id(X) :- between(1, 1, X).

"""

FLUENTS_HEADER = "\n% Fluents\n"
CAUSAL_LAWS_HEADER = "\n% Causal Laws\n"
INITIAL_SITUATION = """
% Initial Situation
initially(pool(_P), true).

"""
ACTIONS_PRECONDITIONS_HEADER = "\n% Actions and Preconditions\n%\n"
EXOGENOUS_ACTIONS_HEADER = "\n% Exogenous Action\n%\n"
EXOGENOUS_ACTION_CLAUSE = "prim_action(Act) :- exog_action(Act).\n\n"
ABBREVIATIONS_HEADER = "\n% ABBREVIATIONS\n"
RUNNING_PROC_DECISION_TASKS = """proc(running(A1), and(done(A1), neg(done(A2)))) :-
  member(A1,
    [{decision_tasks}]), !,
  A1 =.. [F|[start|L]], append(L, [_], L2), A2 =.. [F|[end|L2]].

"""
RUNNING_PROC_DEFAULT = """% default running/1 when start and end actions have the same number of arguments
proc(running(A1), and(done(A1), neg(done(A2)))) :-
  A1 =.. [F|[start|L]], A2 =.. [F|[end|L]].
"""
ACTIVE_INSTANCES_CHECK = "proc(active_instances_check, some(id, some(pool, active(id, pool)))).\n\n"
TOP_LEVEL_CONTROLLER_COMMENT = """/* TOP-LEVEL APPLICATION CONTROLLER

The top level BPM process involves two process in priority:

1. At the top level, run concurrently all participant servers/pools.
2. If the servers are "stuck", just wait for exogenous actions to unblock them.
*/
"""
CONTROL_BPMN_PROCESS = """proc(control(bpmn_process), [prioritized_interrupts(
    [
      bpmn_process,
      interrupt(neg(servers_stopped), ?(wait_exog_action))
    ])]).

"""
BPMN_PROCESS_CONC = "proc(bpmn_process, conc({servers})).\n\n\n"
SERVER_COMMENT = """/* SERVER FOR {pool_upper}

This implements a server for the {pool_name} side process.

It uses exogenous actions for the start event and to shut down the servers/pools.
*/
"""
SERVER_PROC = "proc(server_{pool_name}, iconc(pi(id, [acquire(id, {pool_name}), handle_{pool_name}(id)]))).\n\n"
POOL_PROC = "proc({pool_name}(ID),\n  {proc_body}\n).\n\n"
HANDLE_PROC = "proc(handle_{pool_name}(ID),\n  {proc_body}\n).\n\n\n"
FOOTER = """
prim_action(end_bpmn).
poss(end_bpmn, true).

proc(exog_actions,
  if(done(end_bpmn), [], [exog_action, exog_actions])).

proc(exog_action, pi(a, [?(and(exog_action(a), neg(system_action(a)))), a])).

proc(control(property_verification), search(property_verification)).
proc(property_verification,
  [
    conc([bpmn_process, end_bpmn], exog_actions),
    ?(some([id],
          true  % REPLACE WITH PROPERTY
        )
      )
  ]
).

% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"""
STANDARD_FLUENTS = [
    'active(_ID, _POOL)',
    'servers_stopped',
    'waiting(_ID, _POOL)'
]
STANDARD_CAUSES = [
    "causes_true(acquire(ID, POOL), active(ID, POOL), true).",
    "causes_false(acquire(ID, POOL), waiting(ID, POOL), true).",
    "causes_true(shut_down, servers_stopped, true)."
]
ACQUIRE_ACTION = """prim_action(acquire(_ID, _POOL)).
poss(acquire(ID, POOL), and(id(ID), and(waiting(ID, POOL), pool(POOL)))).
"""
SHUTDOWN_EXOG_ACTION = """exog_action(shut_down).
poss(shut_down, and(neg(servers_stopped), neg(active_instances_check))).

"""
