# Case Study Formalization

This is the formalization of the case study proposed in the paper "Enabling Formal Reasoning in Business Processes: A Situation Calculus and ConGolog Semantics for BPMN". It features a BPMN business process for a job application, formalized using Situation Calculus and ConGolog, and implemented utilizing IndiGolog.

The process executes in a simulated environment via the simulator device manager `dev/env_sim.pl`. The simulator includes a simple TCLK/TK where the user can issue exogenous actions, following the structure below:
- `job_needed(<id>)`: start event for the application process by applicant <id>.
- `<task_name>(end,<id>,...)`: exogenous action to conclude a durative task, which can take data objects as additional parameters.
- `withdrawal_by_applicant(<id>)`: withdrawal of the application by applicant <id>.
- `shut_down`: used to stop the pools' servers.


The complete list of exogenous events that can be issued is reported in section [Case Study: Job Application BPMN](#case-study-job-application-bpmn).

The `end_indi` exogenous actions will finish the execution smoothly.

## Setup

First, install the SWI-Prolog system. In Ubuntu-based system, use PPA: https://www.swi-prolog.org/build/PPA.html

We can develop using VSCODE using extension [New-VSC-Prolog](https://marketplace.visualstudio.com/items?itemName=AmauryRabouan.new-vsc-prolog). Seems to be the best extension today for VSCODE; check [post discussion here](https://swi-prolog.discourse.group/t/new-prolog-mode-for-vs-code/7286/12). Once installed, changed the `prolog.executablePath` to `/bin//swipl` in Settings (`Control + ,`).

Clone the [INDIGOLOG repo](https://github.com/ai-krml-uoft/indigolog):

```shell
$ git clone git@github.com:ai-krml-uoft/indigolog.git
```

Move the files in the src folder within this repository in the examples folder of Indigolog.

Then, from the main Indigolog folder, consult the application as follows:

```shell
$ swipl config.pl examples/case_study/main.pl
```

This will load the domain specification, but also the whole INDIGOLOG architecture (interpreter, temporal projector, devices, libraries, etc.)

Test the most simple elevator example:

```shell
$ swipl indigolog/config.pl indigolog/examples/elevator_simple/main_01.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.8)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- main.
Controllers available: [basic,dumb,smart]
Select controller: basic.

Executing controller: *basic*
down
open
close
off(2)
up
up
up
open
close
off(5)
up
up
up
up
open
close
off(9)
down
down
down
down
down
down
down
down
open

26 actions.
true .
```

Here both SWI-Prolog and INDIGOLOG are working! 👍

## Case Study: Job Application BPMN

We are assuming the INDIGOLOG system has been cloned into `indigolog/` folder.

To run online the Job Application BPMN case study:

```shell
$ swipl indigolog/config.pl examples/case_study/main.pl
SYSTEM(0): Debug level set to 5
SYSTEM(0): Debug level for module em set to 1
INFO(0): Set wait-at-action enable to: 1 seconds.
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.8)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- main.
Controllers available: [bpmn_process]
1. bpmn_process


Select controller: 1.
```

To execute the process, the following exogenous events are needed for the happy path in the case of application `1`:
```
job_needed(1)
prepare_application(end,1)
check_validity(end,1,true)
organize_documents(end,1)
assign_contact_partner(end,1)
verify_prerequisites(end,1)
check_application_for_interview(end,1,true)
plan_interview(end,1)
execute_interview(end,1,true)
obtain_approval(end,1)
validate_job_offer(end,1,true)
produce_contract(end,1)
sign_contract(end,1)
communicate_recruitment(end,1)
store_signed_contract(end,1)
```

These are the exogenous events needed for the application's refusal:
```
job_needed(1)
prepare_application(end,1,application)
check_validity(end,1,false)
produce_letter_of_refusal(end,1,letter_of_refusal)
```

And these are the exogenous events needed for the global exception handling:
```
withdrawal_by_applicant(1)
confirm_withdrawal(end,1)
process_withdrawal(end,1)
```


### Reasoning Tasks

These are the queries for the reasoning tasks:

First, initialize the evaluator:
```prolog
$ swipl indigolog/config.pl examples/case_study/main.pl

?- initialize(evaluator).
true.
```

#### Conformance Checking
```prolog
?- time((H1 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(start, 1), assign_contact_partner(start, 1), organize_documents(end, 1), organize_documents(start, 1), check_validity(end, 1, true), check_validity(start, 1), acquire(1, company), application_sent(1), prepare_application(end, 1), prepare_application(start, 1), acquire(1, applicant), job_needed(1)], proc(sim(bpmn_process), E), trans_star(E, [], _, H1))).
% 203,521 inferences, 0.023 CPU in 0.023 seconds (100% CPU, 8910073 Lips)
H1 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(..., ...)|...],
E = conc([bpmn_process, end_bpm], exog_actions) .

?- time((H2 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(start, 1), assign_contact_partner(start, 1), organize_documents(end, 1), organize_documents(start, 1), check_validity(end, 1, false), check_validity(start, 1), acquire(1, company), application_sent(1), prepare_application(end, 1), prepare_application(start, 1), acquire(1, applicant), job_needed(1)], proc(sim(bpmn_process), E), trans_star(E, [], _, H2))).
% 6,826,598 inferences, 0.315 CPU in 0.315 seconds (100% CPU, 21667041 Lips)
false.

?- time((H2 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(start, 1), assign_contact_partner(start, 1), organize_documents(end, 1), organize_documents(start, 1), check_validity(end, 1, false), check_validity(start, 1), acquire(1, company), application_sent(1), prepare_application(end, 1), prepare_application(start, 1), acquire(1, applicant), job_needed(1)], proc(sim(bpmn_process), E), append(H1, H3, H2), trans_star(E, [], _, H3))), last(H1, A), \+ trans_star(E, [], _, [A|H3]).
% 81,662,937 inferences, 3.648 CPU in 3.648 seconds (100% CPU, 22385459 Lips)
H2 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(..., ...)|...],
E = conc([bpmn_process, end_bpm], exog_actions),
H1 = [execute_interview(end, 1, true), execute_interview(start, 1), plan_interview(end, 1), plan_interview(start, 1), check_application_for_interview(end, 1, true), check_application_for_interview(start, 1), assign_contact_partner(end, 1), verify_prerequisites(end, 1), verify_prerequisites(..., ...)|...],
H3 = [check_validity(end, 1, false), check_validity(start, 1), acquire(1, company), application_sent(1), prepare_application(end, 1), prepare_application(start, 1), acquire(1, applicant), job_needed(1)],
A = organize_documents(start, 1)
```

#### Property Verification
```prolog
?- time(do(property_verification, [], H)).
% 21,914,474 inferences, 0.812 CPU in 0.813 seconds (100% CPU, 26971717 Lips)
H = [end_bpm, withdrawal_handled(1), process_withdrawal(end, 1), process_withdrawal(start, 1), withdrawal_completed(1), withdrawal_sent(1), confirm_withdrawal(end, 1), confirm_withdrawal(start, 1), withdrawal_by_applicant(...)|...] .
```

