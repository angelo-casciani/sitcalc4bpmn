# sitcalc4bpmn

A software suite for translating [BPMN](https://www.bpmn.org/) (Business Process Model and Notation) models to [IndiGolog](https://github.com/ai-krml-uoft/indigolog) processes and performing formal reasoning tasks over them using the *Situation Calculus* and *ConGolog*.

## Overview

This project provides a complete pipeline from BPMN models to formal reasoning:
- **Translation**: Convert BPMN XML files to IndiGolog representations.
- **Reasoning**: Perform various reasoning tasks (projection, legality checking, conformance, property verification, execution) over the translated models.
- **Execution**: Run BPMN processes with exogenous event handling.
- **Web UI**: User-friendly Gradio interface for translation and reasoning.

## Project Structure

```
sitcalc4bpmn/
├── src/
│   ├── bpmn_parser.py         # BPMN XML parser
│   ├── bpmn2indi_cli.py       # BPMN to IndiGolog CLI translator
│   ├── prolog_templates.py    # Template for the IndiGolog code generation
│   ├── prolog_translator.py   # Prolog translator to generate IndiGolog code from the parsed BPMN information
│   ├── reason.py              # Reasoning task executor
│   ├── reasoning_service.py   # Support code for reasoning
│   ├── translator_service.py  # Support code for translation
│   └── ui.py                  # Entry point for the GUI
├── models/                    # Input BPMN files
├── pl_models/                 # Generated IndiGolog translations
|   ├── case_study/            # BPMN and IndiGolog encoding of the case study
│   └── <model_name>/
│       ├── <model_name>.pl    # Process encoding
│       └── main.pl            # Main IndiGolog loader
├── indigolog/                 # IndiGolog (submodule)
└── README.md                  # This file
```

## Features

### Web-Based User Interface (`src/ui.py`)
An intuitive web interface for:
- *Upload & Translate*: Drag-and-drop BPMN files and translate them to IndiGolog
- *View Translation*: Display generated Prolog code with syntax highlighting
- *Load Models*: Access previously translated models
- *Reasoning Tasks*: Execute all reasoning tasks with dynamic parameter inputs
  - Projection, Legality, Execution, Conformance, Property Verification
  - Clear parameter descriptions and validation
  - Output display

### Translation (`src/bpmn2indi_cli.py`)
Parse BPMN XML files and generate IndiGolog Prolog code with:
- Fluent definitions (state variables)
- Action definitions and preconditions
- Causal laws (successor state axioms)
- Process control procedures (ConGolog programs)

### Reasoning (`src/reason.py`)
Six powerful reasoning capabilities:

1. *Projection Task*: Determine what fluents hold after a sequence of actions
2. *Legality/Executability Check*: Verify if an action sequence is executable (all preconditions satisfied)
3. *Process Execution*: Execute the whole BPMN process with exogenous events and capture execution history
4. *Conformance Checking*: Verify if an execution history conforms to the process specification
5. *Property Verification*: Execute custom reasoning tasks to verify process properties
6. *Interactive Mode*: Start a Prolog REPL with the model loaded for manual queries

## Quick Start

### Prerequisites
- Python (>=v.3.10.12)
- SWI-Prolog (>=v.8.4.2) (`sudo apt-get install swi-prolog` on Ubuntu/Debian)
- [IndiGolog](https://github.com/ai-krml-uoft/indigolog) (included as submodule)

### 1. Clone the repository:
```bash
git clone https://github.com/angelo-casciani/sitcalc4bpmn
cd sitcalc4bpmn
```

### 2. Create a Virtual Environment

Assuming a working version of Python installed on the machine, create a virtual environment in the root folder of the project.

```bash
python3 -m venv .venv
source .venv/bin/activate
```

### 3. Install Python Dependencies

Run the following command to install the necessary packages along with their dependencies in the `requirements.txt` file using `pip`:

```bash
pip install -r requirements.txt
```

## Using the Web UI (Recommended)

The easiest way to use the suite is through the web interface:

```bash
python src/ui.py
```

This will start a local web server (default: http://127.0.0.1:7860) and open the interface in your browser.

### Web UI Features:

1. **Translation Tab**: Upload BPMN files and translate them to IndiGolog.
   - Drag and drop .bpmn or .xml files.
   - Specify a model name.
   - View the generated Prolog code with syntax highlighting.

2. **Load Existing Model Tab**: Access previously translated models.
   - Select from a dropdown of available models.
   - View the translated code.

3. **Reasoning Tasks Tab**: Execute reasoning over your models.
   - Select a reasoning task (projection, legality, etc.).
   - Input parameters dynamically adapted on the selected task.
   - View the results.

## Command-Line Usage

### 1. Translate a BPMN Model (CLI)

```bash
python src/bpmn2indi_cli.py <model_name>
```

Example:
```bash
python src/bpmn2indi_cli.py job_application
```

This reads `models/job_application.bpmn` and generates:
- `pl_models/job_application/job_application.pl` (translated process)
- `pl_models/job_application/main.pl` (main IndiGolog file)

### 2. Perform Reasoning Tasks

#### Projection Task
Determine if a fluent has a specific value after executing a certain sequence of actions.

```bash
python src/reason.py <model_name> projection \
    --fluent <fluent_name> \
    --actions <action1,action2,...> \
    --expected <true|false>
```

Examples:
Check if `application(1)` is true after preparing application (i.e., if an application object was created for process instance `1`):
```bash
python src/reason.py job_application projection \
    --fluent "application(1)" \
    --actions "job_needed(1),prepare_application(end,1)" \
    --expected true
```

Check if waiting(1,applicant) is true after job_needed:
```bash
python src/reason.py job_application projection \
    --fluent "waiting(1,applicant)" \
    --actions "job_needed(1)" \
    --expected true
```

#### Legality/Executability Check
Verify if an action sequence can be executed (all preconditions satisfied).

```bash
python src/reason.py <model_name> legality \
    --actions <action1,action2,...> \
    [--proc-name <procedure_name>]
```

Examples:
```bash
# Legal action
python src/reason.py job_application legality \
    --actions "job_needed(1)"

# Illegal action - fails because preconditions aren't met
python src/reason.py job_application legality \
    --actions "application_sent(1)"
```

#### Process Execution
Execute the complete BPMN process with exogenous events.

```bash
python src/reason.py <model_name> execute [--controller <number>]
```

This opens the IndiGolog Tkinter window for issuing exogenous events.
The execution history is printed at the end.

Examples:
```bash
python src/reason.py job_application execute --controller 1
```

**Exogenous events** can be sent via the GUI (e.g., `job_needed(1)`, `prepare_application(end,1)`, `end_indi` to finish).  
For the specific exogenous events available in the case study, refer to the [README](pl_models/case_study/README.md) in the `pl_models/case_study` folder.

#### Conformance Checking
Verify if an execution history conforms to the process specification.

```bash
python src/reason.py <model_name> conformance \
    --history <action1,action2,...>
```

Examples:
```bash
python src/reason.py job_application conformance \
    --history "job_needed(1),acquire(1,applicant),prepare_application(start,1),prepare_application(end,1),application_sent(1),acquire(1,company)"
```

**Output includes:**
- Conformance result (CONFORMANT/NON-CONFORMANT)
- Number of inferences
- CPU time

#### Property Verification
Verify process properties over the whole execution.

```bash
python src/reason.py <model_name> verify \
    [--proc-name <procedure_name>]
    --property <property>
```

Examples:
```bash
python src/reason.py job_application verify --proc-name property_verification --property "signed_contract(id), neg(done(application_finalised(id)))"
```

#### Interactive Mode
Start an interactive SWI-Prolog session for manual queries.

```bash
python src/reason.py job_application interactive
```

**Example queries:**
```prolog
?- initialize(evaluator).
?- eval(application(1), [prepare_application(end,1), job_needed(1)], true).
?- indigolog(test_sequence).
?- halt.
```

## Command Reference

```bash
# Translation
python src/main.py <model_name>

# Reasoning tasks
python src/reason.py <model_name> projection --fluent <F> --actions <A> --expected <true|false>
python src/reason.py <model_name> legality --actions <A> [--proc-name <P>]
python src/reason.py <model_name> execute [--controller <N>]
python src/reason.py <model_name> conformance --history <H>
python src/reason.py <model_name> verify [--property <P>]
python src/reason.py <model_name> interactive

# Get help
python src/reason.py --help
python src/reason.py <model_name> <task> --help
```

## Example Workflow

Here's a complete workflow from BPMN to reasoning:

```bash
# 1. Translate BPMN to IndiGolog
python src/main.py job_application

# 2. Test projection queries
python src/reason.py job_application projection \
    --fluent "application(1)" \
    --actions "job_needed(1),prepare_application(end,1)" \
    --expected true

# 3. Check action legality
python src/reason.py job_application legality \
    --actions "job_needed(1)"

# 4. Execute the process (opens GUI for exogenous events)
python src/reason.py job_application execute --controller 1
# Send events via GUI: job_needed(1), prepare_application(end,1), end_indi

# 5. Check conformance of an execution trace
python src/reason.py job_application conformance \
    --history "job_needed(1),acquire(1,applicant),prepare_application(start,1)"

# 6. Verify custom properties
python src/reason.py job_application verify --property "signed_contract(id), neg(done(application_finalised(id)))"
```

## License

See [LICENSE](LICENSE) for details.