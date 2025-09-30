
:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3,
    poss/2,
    prim_action/1,
    exog_action/1.

cache(_) :- fail.

id(X) :- ground(X), !.
id(X) :- between(1, 1, X).


% Domain-dependent Relational Fluents
rel_fluent(active(_ID)).
rel_fluent(application(_ID)).
rel_fluent(approval(_ID)).
rel_fluent(contract(_ID)).
rel_fluent(documents_ok(_ID)).
rel_fluent(interview(_ID)).
rel_fluent(job_offer(_ID)).
rel_fluent(letter_of_refusal(_ID)).
rel_fluent(processed(_ID)).
rel_fluent(servers_stopped).
rel_fluent(signed_contract(_ID)).
rel_fluent(waiting(_ID, _POOL)).
pool(P) :- member(P, [applicant, company]).


% Causal Laws
causes_false(acquire(ID, POOL), waiting(ID, POOL), true).
causes_false(application_analysed(ID), active(ID), true).
causes_false(application_analysed(ID), waiting(ID, company), true).
causes_false(application_finalised(ID), active(ID), true).
causes_false(application_finalised(ID), waiting(ID, applicant), true).
causes_false(check_application_for_interview(end, ID, RESULT), interview(ID), RESULT = false).
causes_false(check_validity(end, ID, RESULT), documents_ok(ID), RESULT = false).
causes_false(execute_interview(end, ID, RESULT), job_offer(ID), RESULT = false).
causes_false(validate_job_offer(end, ID, RESULT), approval(ID), RESULT = false).
causes_false(withdrawal_completed(ID), active(ID), true).
causes_false(withdrawal_completed(ID), waiting(ID, applicant), true).
causes_false(withdrawal_handled(ID), active(ID), true).
causes_false(withdrawal_handled(ID), waiting(ID, company), true).
causes_true(application_analysed(ID), processed(ID), true).
causes_true(application_received(ID), active(ID), true).
causes_true(application_received(ID), waiting(ID, company), true).
causes_true(application_received(end, ID), application(ID), true).
causes_true(application_sent(ID), waiting(ID, company), true).
causes_true(check_application_for_interview(end, ID, RESULT), interview(ID), RESULT = true).
causes_true(check_validity(end, ID, RESULT), documents_ok(ID), RESULT = true).
causes_true(contract_received(end, ID), contract(ID), true).
causes_true(execute_interview(end, ID, RESULT), job_offer(ID), RESULT = true).
causes_true(job_needed(ID), active(ID), true).
causes_true(job_needed(ID), waiting(ID, applicant), true).
causes_true(prepare_application(end, ID), application(ID), true).
causes_true(produce_contract(end, ID), contract(ID), true).
causes_true(produce_letter_of_refusal(end, ID), letter_of_refusal(ID), true).
causes_true(send_contract(ID), waiting(ID, applicant), true).
causes_true(send_letter_of_refusal(ID), waiting(ID, applicant), true).
causes_true(send_signed_contract(ID), waiting(ID, company), true).
causes_true(shut_down, servers_stopped, true).
causes_true(sign_contract(end, ID), signed_contract(ID), true).
causes_true(signed_contract_received(end, ID), signed_contract(ID), true).
causes_true(validate_job_offer(end, ID, RESULT), approval(ID), RESULT = true).
causes_true(withdrawal_handled(ID), processed(ID), true).
causes_true(withdrawal_sent(ID), waiting(ID, company), true).

% Initial Situation
initially(active(_ID), false).
initially(application(_ID), false).
initially(approval(_ID), false).
initially(contract(_ID), false).
initially(documents_ok(_ID), false).
initially(interview(_ID), false).
initially(job_offer(_ID), false).
initially(letter_of_refusal(_ID), false).
initially(processed(_ID), false).
initially(servers_stopped, false).
initially(signed_contract(_ID), false).
initially(waiting(_ID, _POOL), false).
initially(pool(_P), true).
initially(servers_stopped, false).

% Actions and Preconditions
prim_action(application_analysed(_ID)).
poss(application_analysed(ID), or(done(store_signed_contract(end, ID)), done(letter_of_refusal_sent(ID)))).
prim_action(application_finalised(_ID)).
poss(application_finalised(ID), done((ID))).
prim_action(application_sent(_ID)).
poss(application_sent(ID), and(application(ID), done(prepare_application(end, ID)))).
prim_action(assign_contact_partner(start, _ID)).
poss(assign_contact_partner(start, ID), done((ID))).
prim_action(check_application_for_interview(start, _ID)).
poss(check_application_for_interview(start, ID), and(application(ID), done((ID)))).
prim_action(check_validity(start, _ID)).
poss(check_validity(start, ID), and(application(ID), done(application_received(ID)))).
prim_action(communicate_recruitment(start, _ID)).
poss(communicate_recruitment(start, ID), done(send_signed_contract(ID))).
prim_action(confirm_withdrawal(start, _ID)).
poss(confirm_withdrawal(start, ID), done(withdrawal_by_applicant(ID))).
prim_action(contract_received(_ID)).
poss(contract_received(ID), done((ID))).
prim_action(execute_interview(start, _ID)).
poss(execute_interview(start, ID), done(plan_interview(end, ID))).
prim_action(letter_of_refusal_received(_ID)).
poss(letter_of_refusal_received(ID), done((ID))).
prim_action(obtain_approval(start, _ID)).
poss(obtain_approval(start, ID), done(job_offer(ID))).
prim_action(organize_documents(start, _ID)).
poss(organize_documents(start, ID), done(documents_ok(ID))).
prim_action(plan_interview(start, _ID)).
poss(plan_interview(start, ID), done(interview(ID))).
prim_action(prepare_application(start, _ID)).
poss(prepare_application(start, ID), done(job_needed(ID))).
prim_action(process_withdrawal(start, _ID)).
poss(process_withdrawal(start, ID), done(withdrawal_by_applicant_received(ID))).
prim_action(produce_contract(start, _ID)).
poss(produce_contract(start, ID), done(approval(ID))).
prim_action(produce_letter_of_refusal(start, _ID)).
poss(produce_letter_of_refusal(start, ID), done((ID))).
prim_action(send_contract(_ID)).
poss(send_contract(ID), and(contract(ID), done(produce_contract(end, ID)))).
prim_action(send_letter_of_refusal(_ID)).
poss(send_letter_of_refusal(ID), and(done(produce_letter_of_refusal(end, ID)), letter_of_refusal(ID))).
prim_action(send_signed_contract(_ID)).
poss(send_signed_contract(ID), and(done(sign_contract(end, ID)), signed_contract(ID))).
prim_action(sign_contract(start, _ID)).
poss(sign_contract(start, ID), and(contract(ID), done(contract_received(ID)))).
prim_action(signed_contract_received(_ID)).
poss(signed_contract_received(ID), done(send_contract(ID))).
prim_action(store_signed_contract(start, _ID)).
poss(store_signed_contract(start, ID), and(done(signed_contract_received(ID)), signed_contract(ID))).
prim_action(validate_job_offer(start, _ID)).
poss(validate_job_offer(start, ID), done(obtain_approval(end, ID))).
prim_action(verify_prerequisites(start, _ID)).
poss(verify_prerequisites(start, ID), done((ID))).
prim_action(withdrawal_completed(_ID)).
poss(withdrawal_completed(ID), done(withdrawal_sent(ID))).
prim_action(withdrawal_handled(_ID)).
poss(withdrawal_handled(ID), and(done(withdrawal_completed(ID)), done(process_withdrawal(end, ID)))).
prim_action(withdrawal_sent(_ID)).
poss(withdrawal_sent(ID), done(confirm_withdrawal(end, ID))).

% Exogenous Actions
prim_action(Act) :- exog_action(Act).
exog_action(application_received(_ID)).
poss(application_received(ID), neg(done(application_received(ID)))).
exog_action(assign_contact_partner(end, _ID)).
poss(assign_contact_partner(end, ID), running(assign_contact_partner(start, ID))).
exog_action(check_application_for_interview(end, _ID, _)).
poss(check_application_for_interview(end, ID, _), running(check_application_for_interview(start, ID))).
exog_action(check_validity(end, _ID, _)).
poss(check_validity(end, ID, _), running(check_validity(start, ID))).
exog_action(communicate_recruitment(end, _ID)).
poss(communicate_recruitment(end, ID), running(communicate_recruitment(start, ID))).
exog_action(confirm_withdrawal(end, _ID)).
poss(confirm_withdrawal(end, ID), running(confirm_withdrawal(start, ID))).
exog_action(execute_interview(end, _ID, _)).
poss(execute_interview(end, ID, _), running(execute_interview(start, ID))).
exog_action(job_needed(_ID)).
poss(job_needed(ID), neg(done(job_needed(ID)))).
exog_action(obtain_approval(end, _ID)).
poss(obtain_approval(end, ID), running(obtain_approval(start, ID))).
exog_action(organize_documents(end, _ID)).
poss(organize_documents(end, ID), running(organize_documents(start, ID))).
exog_action(plan_interview(end, _ID)).
poss(plan_interview(end, ID), running(plan_interview(start, ID))).
exog_action(prepare_application(end, _ID)).
poss(prepare_application(end, ID), running(prepare_application(start, ID))).
exog_action(process_withdrawal(end, _ID)).
poss(process_withdrawal(end, ID), running(process_withdrawal(start, ID))).
exog_action(produce_contract(end, _ID)).
poss(produce_contract(end, ID), running(produce_contract(start, ID))).
exog_action(produce_letter_of_refusal(end, _ID)).
poss(produce_letter_of_refusal(end, ID), running(produce_letter_of_refusal(start, ID))).
exog_action(shut_down).
exog_action(sign_contract(end, _ID)).
poss(sign_contract(end, ID), running(sign_contract(start, ID))).
exog_action(store_signed_contract(end, _ID)).
poss(store_signed_contract(end, ID), running(store_signed_contract(start, ID))).
exog_action(validate_job_offer(end, _ID, _)).
poss(validate_job_offer(end, ID, _), running(validate_job_offer(start, ID))).
exog_action(verify_prerequisites(end, _ID)).
poss(verify_prerequisites(end, ID), running(verify_prerequisites(start, ID))).
exog_action(withdrawal_by_applicant(_ID)).
poss(withdrawal_by_applicant(ID), neg(done(withdrawal_by_applicant(ID)))).
exog_action(withdrawal_by_applicant_received(_ID)).
poss(withdrawal_by_applicant_received(ID), neg(done(withdrawal_by_applicant_received(ID)))).
poss(shut_down, neg(servers_stopped)).

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

proc(active_instances_check, some(id, active(id))).

% HIGH-LEVEL PROCESS PROCEDURES
proc(control(bpmn_process), conc(server_applicant, server_company)).

proc(applicant_procedure(ID), [?(done(job_needed(ID))), [[prepare_application(start, ID), ?(done(prepare_application(end, ID)))], [application_sent(ID), ndet([?(done(send_contract(ID))), [[sign_contract(start, ID), ?(done(sign_contract(end, ID)))], [send_signed_contract(ID), [[communicate_recruitment(start, ID), ?(done(communicate_recruitment(end, ID)))], if((ID), application_finalised(ID), [])]]]], [?(done(send_letter_of_refusal(ID))), if((ID), application_finalised(ID), [])])]]]).
proc(handle_applicant(ID), [ gexec(and(active(ID), neg(done(withdrawal_by_applicant(ID)))), applicant_procedure(ID)), if(and(neg(processed(ID)), done(withdrawal_by_applicant(ID))), [?(done(withdrawal_by_applicant(ID))), [[confirm_withdrawal(start, ID), ?(done(confirm_withdrawal(end, ID)))], [withdrawal_sent(ID), withdrawal_completed(ID)]]], []) ]).
proc(server_applicant, pi(app, [acquire(app, applicant), handle_applicant(app)])).

proc(company_procedure(ID), [?(done(application_received(ID))), [[check_validity(start, ID), ?(some(res, done(check_validity(end, ID, res))))], if(documents_ok(ID), [[organize_documents(start, ID), ?(done(organize_documents(end, ID)))], [conc([], []), ?(and(done(assign_contact_partner(end, ID)), done(verify_prerequisites(end, ID)))), [[check_application_for_interview(start, ID), ?(some(res, done(check_application_for_interview(end, ID, res))))], if(interview(ID), [[plan_interview(start, ID), ?(done(plan_interview(end, ID)))], [[execute_interview(start, ID), ?(some(res, done(execute_interview(end, ID, res))))], if(job_offer(ID), [[obtain_approval(start, ID), ?(done(obtain_approval(end, ID)))], [[validate_job_offer(start, ID), ?(some(res, done(validate_job_offer(end, ID, res))))], if(approval(ID), [[produce_contract(start, ID), ?(done(produce_contract(end, ID)))], [send_contract(ID), [?(done(send_signed_contract(ID))), [[store_signed_contract(start, ID), ?(done(store_signed_contract(end, ID)))], if((ID), application_analysed(ID), [])]]]], if((ID), [[produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID)))], [send_letter_of_refusal(ID), if((ID), application_analysed(ID), [])]], []))]], if((ID), [[produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID)))], [send_letter_of_refusal(ID), if((ID), application_analysed(ID), [])]], []))]], if((ID), [[produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID)))], [send_letter_of_refusal(ID), if((ID), application_analysed(ID), [])]], []))]]], if((ID), [[produce_letter_of_refusal(start, ID), ?(done(produce_letter_of_refusal(end, ID)))], [send_letter_of_refusal(ID), if((ID), application_analysed(ID), [])]], []))]]).
proc(handle_company(ID), [ gexec(and(neg(processed(ID)), neg(done(withdrawal_sent(ID)))), company_procedure(ID)), if(and(neg(processed(ID)), done(withdrawal_sent(ID))), [?(done(withdrawal_by_applicant_received(ID))), [[process_withdrawal(start, ID), ?(done(process_withdrawal(end, ID)))], withdrawal_handled(ID)]], []) ]).
proc(server_company, pi(app, [acquire(app, company), handle_company(app)])).


prim_action(end_bpm).
poss(end_bpm, true).

proc(exog_actions,
  if(done(end_bpm), [], [exog_action, exog_actions])).

proc(exog_action, pi(a, [?(and(exog_action(a), neg(system_action(a)))), a])).

actionNum(X, X).
