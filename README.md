# sitcalc4bpmn

A comprehensive tool for translating BPMN (Business Process Model and Notation) models to IndiGolog processes and performing formal reasoning tasks over them using the Situation Calculus.

## Overview

This project provides a complete pipeline from BPMN models to formal reasoning:
- **Translation**: Convert BPMN XML files to IndiGolog/Prolog representations
- **Reasoning**: Perform various reasoning tasks (projection, legality checking, conformance, property verification, execution) over the translated models
- **Execution**: Run BPMN processes with exogenous event handling
- **Web UI**: User-friendly Gradio interface for translation and reasoning

## Features

### Web-Based User Interface (`src/ui.py`)
**NEW!** A simple and intuitive web interface for:
- **Upload & Translate**: Drag-and-drop BPMN files and translate them to IndiGolog
- **View Translation**: Display generated Prolog code with syntax highlighting
- **Load Models**: Access previously translated models
- **Reasoning Tasks**: Execute all reasoning tasks with dynamic parameter inputs
  - Projection, Legality, Execution, Conformance, Property Verification
  - Clear parameter descriptions and validation
  - Real-time output display

### Translation (`src/bpmn_translator.py`)
Parse BPMN XML files and generate IndiGolog Prolog code with:
- Fluent definitions (state variables)
- Action definitions and preconditions
- Causal laws (successor state axioms)
- Process control procedures (ConGolog programs)

### Reasoning (`src/reason.py`)
Six powerful reasoning capabilities:

1. **Projection Task**: Determine what fluents hold after a sequence of actions
2. **Legality/Executability Check**: Verify if an action sequence is executable (all preconditions satisfied)
3. **Process Execution**: Execute the whole BPMN process with exogenous events and capture execution history
4. **Conformance Checking**: Verify if an execution history conforms to the process specification
5. **Property Verification**: Execute custom reasoning tasks to verify process properties
6. **Interactive Mode**: Start a Prolog REPL with the model loaded for manual queries

**Additional Features:**
- Smart action parsing that handles complex action arguments with nested parentheses
- Timing statistics (inferences and CPU time) for all reasoning tasks
- Proper exit codes for CI/CD integration

## Quick Start

### Prerequisites
- Python 3.x
- SWI-Prolog 8.x+ (`sudo apt-get install swi-prolog` on Ubuntu/Debian)
- IndiGolog framework (included as submodule)

### Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd sitcalc4bpmn
```

2. Install Python dependencies:
```bash
pip install -r requirements.txt
```

## Using the Web UI (Recommended)

The easiest way to use the tool is through the web interface:

```bash
python src/ui.py
```

This will start a local web server (default: http://127.0.0.1:7860) and open the interface in your browser.

### Web UI Features:

1. **Translation Tab**: Upload BPMN files and translate them to IndiGolog
   - Drag and drop .bpmn or .xml files
   - Specify a model name
   - View the generated Prolog code with syntax highlighting

2. **Load Existing Model Tab**: Access previously translated models
   - Select from a dropdown of available models
   - View the translated code

3. **Reasoning Tasks Tab**: Execute reasoning over your models
   - Select a reasoning task (projection, legality, etc.)
   - Input parameters dynamically based on the selected task
   - View results in real-time

## Command-Line Usage

### 1. Translate a BPMN Model (CLI)

```bash
python src/bpmn_translator.py <model_name>
```

Example:
```bash
python src/bpmn_translator.py job_application
```

This reads `models/job_application.bpmn` and generates:
- `pl_models/job_application/job_application.pl` (translated process)
- `pl_models/job_application/main.pl` (main IndiGolog file)

### 2. Perform Reasoning Tasks

#### Projection Task
Determine if a fluent has a specific value after executing actions.

```bash
python src/reason.py <model_name> projection \
    --fluent <fluent_name> \
    --actions <action1,action2,...> \
    --expected <true|false>
```

**Examples:**
```bash
# Check if application(1) is true after preparing application
python src/reason.py job_application projection \
    --fluent "application(1)" \
    --actions "job_needed(1),prepare_application(end,1)" \
    --expected true

# Check if waiting(1,applicant) is true after job_needed
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

**Examples:**
```bash
# Legal action (returns exit code 0)
python src/reason.py job_application legality \
    --actions "job_needed(1)"

# Illegal action - fails because preconditions aren't met (returns exit code 1)
python src/reason.py job_application legality \
    --actions "application_sent(1)"
```

#### Process Execution
Execute the complete BPMN process with exogenous events.

```bash
python src/reason.py <model_name> execute [--controller <number>]
```

**Example:**
```bash
# Opens Tkinter window for exogenous events
# Execution history is printed at the end
python src/reason.py job_application execute --controller 1
```

**Exogenous events** can be sent via the GUI (e.g., `job_needed(1)`, `prepare_application(end,1)`, `end_indi` to finish).

#### Conformance Checking
Verify if an execution history conforms to the process specification using `trans_star/4`.

```bash
python src/reason.py <model_name> conformance \
    --history <action1,action2,...>
```

**Example:**
```bash
python src/reason.py job_application conformance \
    --history "job_needed(1),acquire(1,applicant),prepare_application(start,1),prepare_application(end,1),application_sent(1),acquire(1,company)"
```

**Output includes:**
- Conformance result (CONFORMANT/NON-CONFORMANT)
- Number of inferences
- CPU time

#### Property Verification
Execute a custom reasoning task to verify process properties using `do/3`.

```bash
python src/reason.py <model_name> verify \
    [--proc-name <procedure_name>]
```

**Example:**
```bash
# Uses proc(reasoning_task, ...) defined in the model
python src/reason.py job_application verify --proc-name reasoning_task
```

**Note:** The procedure must be defined in your model's Prolog file as `proc(<proc_name>, ...)`.

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
python src/reason.py <model_name> verify [--proc-name <P>]
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
python src/reason.py job_application verify --proc-name reasoning_task
```

## Project Structure

```
sitcalc4bpmn/
├── src/
│   ├── main.py              # BPMN to IndiGolog translator
│   ├── reason.py            # Reasoning task executor (6 tasks)
│   ├── bpmn_parser.py       # BPMN XML parser
│   ├── prolog_translator.py # Prolog code generator
│   └── prolog_templates.py  # Prolog code templates
├── models/                   # Input BPMN files (.bpmn)
├── pl_models/               # Generated IndiGolog translations
│   └── <model_name>/
│       ├── <model_name>.pl  # Translated process (fluents, actions, BAT)
│       └── main.pl          # Main IndiGolog loader
├── indigolog/               # IndiGolog framework (submodule)
│   ├── config.pl            # Framework configuration
│   ├── interpreters/        # ConGolog interpreters
│   ├── eval/                # Reasoning engines (eval, trans_star, do)
│   ├── devices/             # Device managers (simulator with Tkinter GUI)
│   └── lib/                 # Utility libraries
└── README.md                # This file
```

## Advanced Usage

### Exit Codes
All reasoning tasks return proper exit codes for CI/CD integration:
- `0`: Success (query succeeded, sequence is legal, conformant, etc.)
- `1`: Failure (query failed, sequence is illegal, non-conformant, etc.)

### Action Parsing
The script intelligently handles complex action arguments:
```bash
# Actions with multiple parameters
python src/reason.py job_application projection \
    --fluent "documents_ok(1)" \
    --actions "check_validity(start,1),check_validity(end,1,true)" \
    --expected true

# Actions with nested structures are properly parsed
```

### Performance Monitoring
All reasoning tasks report:
- **Inferences**: Number of logical inferences performed
- **CPU time**: Time spent in computation (seconds)

Example output:
```
203,521 inferences, 0.023 CPU in 0.023 seconds
```

## IndiGolog Framework

This project uses the [IndiGolog framework](https://github.com/ai-krml-uoft/indigolog) for high-level agent programming in the Situation Calculus. IndiGolog provides:

- **Basic Action Theory (BAT)**: Formal representation of actions and their effects
- **Temporal Projection** (`eval/3`): Query fluent values after action sequences
- **Transition Semantics** (`trans_star/4`): Verify execution traces
- **Planning** (`do/3`): Search for action sequences achieving goals
- **Execution**: Run processes with exogenous event handling
- **Environment Management**: Device simulators with GUI interfaces

### Key Predicates

- `eval(Fluent, Actions, Value)`: Evaluate fluent after actions
- `trans_star(Program, S, _, History)`: Check if history is valid execution
- `do(Program, S, History)`: Execute program and return history
- `indigolog(Program)`: Execute program with interpreter
- `initialize(evaluator)`: Initialize reasoning engine

## References

- **IndiGolog**: High-level programming language based on Situation Calculus ([GitHub](https://github.com/ai-krml-uoft/indigolog))
- **Situation Calculus**: Logic formalism for reasoning about dynamic systems
- **BPMN**: Business Process Model and Notation ([OMG Standard](https://www.omg.org/spec/BPMN/))
- **ConGolog**: Concurrent Golog language for agent programming

## License

See [LICENSE](LICENSE) for details.