
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


% Domain-dependent Relational Fluents
rel_fluent(active(_ID, _POOL)).
causes_false(application_analysed(ID), active(ID, company), true).
causes_false(application_finalised(ID), active(ID, applicant), true).
causes_false(withdrawal_completed(ID), active(ID, applicant), true).
causes_false(withdrawal_handled(ID), active(ID, company), true).
causes_true(acquire(ID, POOL), active(ID, POOL), true).
rel_fluent(application(_ID)).
causes_true(prepare_application(end, ID), application(ID), true).
rel_fluent(approval(_ID)).
causes_false(validate_job_offer(end, ID, RESULT), approval(ID), RESULT = false).
causes_true(validate_job_offer(end, ID, RESULT), approval(ID), RESULT = true).
rel_fluent(contract(_ID)).
causes_true(produce_contract(end, ID), contract(ID), true).
rel_fluent(documents_ok(_ID)).
causes_false(check_validity(end, ID, RESULT), documents_ok(ID), RESULT = false).
causes_true(check_validity(end, ID, RESULT), documents_ok(ID), RESULT = true).
rel_fluent(interview(_ID)).
causes_false(check_application_for_interview(end, ID, RESULT), interview(ID), RESULT = false).
causes_true(check_application_for_interview(end, ID, RESULT), interview(ID), RESULT = true).
rel_fluent(job_offer(_ID)).
causes_false(execute_interview(end, ID, RESULT), job_offer(ID), RESULT = false).
causes_true(execute_interview(end, ID, RESULT), job_offer(ID), RESULT = true).
rel_fluent(letter_of_refusal(_ID)).
causes_true(produce_letter_of_refusal(end, ID), letter_of_refusal(ID), true).
rel_fluent(servers_stopped).
causes_true(shut_down, servers_stopped, true).
rel_fluent(signed_contract(_ID)).
causes_true(sign_contract(end, ID), signed_contract(ID), true).
rel_fluent(waiting(_ID, _POOL)).
causes_false(acquire(ID, POOL), waiting(ID, POOL), true).
causes_false(application_analysed(ID), waiting(ID, company), true).
causes_false(application_finalised(ID), waiting(ID, applicant), true).
causes_true(application_sent(ID), waiting(ID, company), true).
causes_true(job_needed(ID), waiting(ID, applicant), true).
causes_true(withdrawal_sent(ID), waiting(ID, company), true).
rel_fluent(pool(P)) :- member(P, [applicant, company]).


% Initial Situation
initially(pool(_P), true).


% Actions and Preconditions
%
prim_action(acquire(_ID, _POOL)).
poss(acquire(ID, POOL), and(id(ID), and(waiting(ID, POOL), pool(POOL)))).
prim_action(application_analysed(_ID)).
poss(application_analysed(ID), and(neg(done(withdrawal_by_applicant(ID))), or(done(letter_of_refusal_sent(ID)), done(store_signed_contract(end, ID))))).
prim_action(application_finalised(_ID)).
poss(application_finalised(ID), and(neg(done(withdrawal_by_applicant(ID))), or(done(communicate_recruitment(end, ID)), done(letter_of_refusal_sent(ID))))).
prim_action(application_sent(_ID)).
poss(application_sent(ID), and(done(prepare_application(end, ID)), application(ID))).
prim_action(assign_contact_partner(start, _ID)).
poss(assign_contact_partner(start, ID), done(organize_documents(end, ID))).
prim_action(check_application_for_interview(start, _ID)).
poss(check_application_for_interview(start, ID), and(done(assign_contact_partner(end, ID)), and(done(verify_prerequisites(end, ID)), application(ID)))).
prim_action(check_validity(start, _ID)).
poss(check_validity(start, ID), and(done(application_sent(ID)), application(ID))).
prim_action(communicate_recruitment(start, _ID)).
poss(communicate_recruitment(start, ID), and(done(signed_contract_sent(ID)), signed_contract(ID))).
prim_action(confirm_withdrawal(start, _ID)).
poss(confirm_withdrawal(start, ID), done(withdrawal_by_applicant(ID))).
prim_action(contract_sent(_ID)).
poss(contract_sent(ID), and(done(produce_contract(end, ID)), contract(ID))).
prim_action(execute_interview(start, _ID)).
poss(execute_interview(start, ID), done(plan_interview(end, ID))).
prim_action(letter_of_refusal_sent(_ID)).
poss(letter_of_refusal_sent(ID), and(done(produce_letter_of_refusal(end, ID)), letter_of_refusal(ID))).
prim_action(obtain_approval(start, _ID)).
poss(obtain_approval(start, ID), job_offer(ID)).
prim_action(organize_documents(start, _ID)).
poss(organize_documents(start, ID), documents_ok(ID)).
prim_action(plan_interview(start, _ID)).
poss(plan_interview(start, ID), interview(ID)).
prim_action(prepare_application(start, _ID)).
poss(prepare_application(start, ID), done(job_needed(ID))).
prim_action(process_withdrawal(start, _ID)).
poss(process_withdrawal(start, ID), done(withdrawal_sent(ID))).
prim_action(produce_contract(start, _ID)).
poss(produce_contract(start, ID), approval(ID)).
prim_action(produce_letter_of_refusal(start, _ID)).
poss(produce_letter_of_refusal(start, ID), neg(and(approval(ID), and(documents_ok(ID), and(interview(ID), job_offer(ID)))))).
prim_action(sign_contract(start, _ID)).
poss(sign_contract(start, ID), and(done(contract_sent(ID)), contract(ID))).
prim_action(signed_contract_sent(_ID)).
poss(signed_contract_sent(ID), and(done(sign_contract(end, ID)), signed_contract(ID))).
prim_action(store_signed_contract(start, _ID)).
poss(store_signed_contract(start, ID), and(done(signed_contract_sent(ID)), signed_contract(ID))).
prim_action(validate_job_offer(start, _ID)).
poss(validate_job_offer(start, ID), done(obtain_approval(end, ID))).
prim_action(verify_prerequisites(start, _ID)).
poss(verify_prerequisites(start, ID), done(organize_documents(end, ID))).
prim_action(withdrawal_completed(_ID)).
poss(withdrawal_completed(ID), done(withdrawal_sent(ID))).
prim_action(withdrawal_handled(_ID)).
poss(withdrawal_handled(ID), done(process_withdrawal(end, ID))).
prim_action(withdrawal_sent(_ID)).
poss(withdrawal_sent(ID), done(confirm_withdrawal(end, ID))).

% Exogenous Action
%
prim_action(Act) :- exog_action(Act).

exog_action(assign_contact_partner(end, ID)) :- id(ID).
poss(assign_contact_partner(end, ID), running(assign_contact_partner(start, ID))).

exog_action(check_application_for_interview(end, ID, RESULT)) :- id(ID), member(RESULT, [true, false]).
poss(check_application_for_interview(end, ID, _RESULT), and(running(check_application_for_interview(start, ID)), application(ID))).

exog_action(check_validity(end, ID, RESULT)) :- id(ID), member(RESULT, [true, false]).
poss(check_validity(end, ID, _RESULT), and(running(check_validity(start, ID)), application(ID))).

exog_action(communicate_recruitment(end, ID)) :- id(ID).
poss(communicate_recruitment(end, ID), and(running(communicate_recruitment(start, ID)), signed_contract(ID))).

exog_action(confirm_withdrawal(end, ID)) :- id(ID).
poss(confirm_withdrawal(end, ID), running(confirm_withdrawal(start, ID))).

exog_action(execute_interview(end, ID, RESULT)) :- id(ID), member(RESULT, [true, false]).
poss(execute_interview(end, ID, _RESULT), running(execute_interview(start, ID))).

exog_action(job_needed(ID)) :- id(ID).
poss(job_needed(ID), neg(done(job_needed(ID)))).

exog_action(obtain_approval(end, ID)) :- id(ID).
poss(obtain_approval(end, ID), running(obtain_approval(start, ID))).

exog_action(organize_documents(end, ID)) :- id(ID).
poss(organize_documents(end, ID), running(organize_documents(start, ID))).

exog_action(plan_interview(end, ID)) :- id(ID).
poss(plan_interview(end, ID), running(plan_interview(start, ID))).

exog_action(prepare_application(end, ID)) :- id(ID).
poss(prepare_application(end, ID), running(prepare_application(start, ID))).

exog_action(process_withdrawal(end, ID)) :- id(ID).
poss(process_withdrawal(end, ID), running(process_withdrawal(start, ID))).

exog_action(produce_contract(end, ID)) :- id(ID).
poss(produce_contract(end, ID), running(produce_contract(start, ID))).

exog_action(produce_letter_of_refusal(end, ID)) :- id(ID).
poss(produce_letter_of_refusal(end, ID), running(produce_letter_of_refusal(start, ID))).

exog_action(sign_contract(end, ID)) :- id(ID).
poss(sign_contract(end, ID), and(running(sign_contract(start, ID)), contract(ID))).

exog_action(store_signed_contract(end, ID)) :- id(ID).
poss(store_signed_contract(end, ID), and(running(store_signed_contract(start, ID)), signed_contract(ID))).

exog_action(validate_job_offer(end, ID, RESULT)) :- id(ID), member(RESULT, [true, false]).
poss(validate_job_offer(end, ID, _RESULT), running(validate_job_offer(start, ID))).

exog_action(verify_prerequisites(end, ID)) :- id(ID).
poss(verify_prerequisites(end, ID), running(verify_prerequisites(start, ID))).

exog_action(withdrawal_by_applicant(ID)) :- id(ID).
poss(withdrawal_by_applicant(ID), and(pool(POOL), and(neg(done(application_analysed(ID))), and(neg(done(application_finalised(ID))), and(neg(waiting(ID, POOL)), and(active(ID, POOL), neg(done(withdrawal_by_applicant(ID))))))))).

exog_action(shut_down).
poss(shut_down, and(neg(servers_stopped), neg(active_instances_check))).


% ABBREVIATIONS
proc(running(A1), and(done(A1), neg(done(A2)))) :-
  member(A1,
    [check_application_for_interview(start, _),
      check_validity(start, _),
      execute_interview(start, _),
      validate_job_offer(start, _)]), !,
  A1 =.. [F|[start|L]], append(L, [_], L2), A2 =.. [F|[end|L2]].

% default running/1 when start and end actions have the same number of arguments
proc(running(A1), and(done(A1), neg(done(A2)))) :-
  A1 =.. [F|[start|L]], A2 =.. [F|[end|L]].
proc(active_instances_check, some(id, some(pool, active(id, pool)))).


/* TOP-LEVEL APPLICATION CONTROLLER

The top level BPM process involves two process in priority:

1. At the top level, run concurrently all participant servers/pools.
2. If the servers are "stuck", just wait for exogenous actions to unblock them.
*/
proc(control(bpmn_process), [prioritized_interrupts(
    [
      bpmn_process,
      interrupt(neg(servers_stopped), ?(wait_exog_action))
    ])]).

proc(bpmn_process, conc(server_applicant, server_company)).


/* SERVER FOR APPLICANT

This implements a server for the applicant side process.

It uses exogenous actions for the start event and to shut down the servers/pools.
*/
proc(server_applicant, iconc(pi(id, [acquire(id, applicant), handle_applicant(id)]))).

proc(applicant(ID),
  [prepare_application(start, ID), ?(done(prepare_application(end, ID))), application_sent(ID), ndet([?(done(contract_sent(ID))), [sign_contract(start, ID), ?(done(sign_contract(end, ID))), signed_contract_sent(ID), communicate_recruitment(start, ID), ?(done(communicate_recruitment(end, ID))), application_finalised(ID)]], [?(done(letter_of_refusal_sent(ID))), application_finalised(ID)])]
).

proc(handle_applicant(ID),
  [ gexec(and(active(ID, applicant), neg(done(withdrawal_by_applicant(ID)))), applicant(ID)),
    if(and(active(ID, company), done(withdrawal_by_applicant(ID))),
          [confirm_withdrawal(start, ID), ?(done(confirm_withdrawal(end, ID))), withdrawal_sent(ID), withdrawal_completed(ID)],
          []
      )
  ]
).


/* SERVER FOR COMPANY

This implements a server for the company side process.

It uses exogenous actions for the start event and to shut down the servers/pools.
*/
proc(server_company, iconc(pi(id, [acquire(id, company), handle_company(id)]))).

proc(company(ID),
  [?(done(application_sent(ID))), check_validity(start, ID), ?(some(res, done(check_validity(end, ID, res)))), if(documents_ok(ID), [organize_documents(start, ID), ?(done(organize_documents(end, ID))), conc(assign_contact_partner(start, ID), verify_prerequisites(start, ID)), ?(and(done(assign_contact_partner(end, ID)), done(verify_prerequisites(end, ID)))), [check_application_for_interview(start, ID), ?(some(res, done(check_application_for_interview(end, ID, res)))), if(interview(ID), [plan_interview(start, ID), ?(done(plan_interview(end, ID))), execute_interview(start, ID), ?(some(res, done(execute_interview(end, ID, res)))), if(job_offer(ID), [obtain_approval(start, ID), ?(done(obtain_approval(end, ID))), validate_job_offer(start, ID), ?(some(res, done(validate_job_offer(end, ID, res)))), if(approval(ID), [produce_contract(start, ID), ?(done(produce_contract(end, ID))), contract_sent(ID), [?(done(signed_contract_sent(ID))), store_signed_contract(start, ID), ?(done(store_signed_contract(end, ID))), application_analysed(ID)]], [produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID))), letter_of_refusal_sent(ID), application_analysed(ID)])], [produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID))), letter_of_refusal_sent(ID), application_analysed(ID)])], [produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID))), letter_of_refusal_sent(ID), application_analysed(ID)])]], [produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID))), letter_of_refusal_sent(ID), application_analysed(ID)])]
).

proc(handle_company(ID),
  [ gexec(and(active(ID, company), neg(done(withdrawal_sent(ID)))), company(ID)),
    if(and(active(ID, company), done(withdrawal_sent(ID))),
          [process_withdrawal(start, ID), ?(done(process_withdrawal(end, ID))), withdrawal_handled(ID)],
          []
      )
  ]
).



prim_action(end_bpmn).
poss(end_bpmn, true).

proc(exog_actions,
  if(done(end_bpmn), [], [exog_action, exog_actions])).

proc(exog_action, pi(a, [?(and(exog_action(a), neg(system_action(a)))), a])).

% simulate process BP under exogenous events
proc(sim(BP), conc([BP, end_bpmn], exog_actions)).

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
