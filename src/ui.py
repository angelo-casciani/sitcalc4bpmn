#!/usr/bin/env python3
"""
BPMN to IndiGolog Translation and Reasoning UI

This is a web-based interface using Gradio for:
1. Uploading BPMN files and translating them to IndiGolog
2. Viewing the translated Prolog code
3. Performing reasoning tasks over the translated models

Usage:
    python src/ui.py
"""

import gradio as gr
import os
import sys
from pathlib import Path
import tempfile

# Add src directory to path
sys.path.insert(0, str(Path(__file__).parent))

from translator_service import TranslatorService
from reasoning_service import ReasoningService

# Import pm4py for BPMN visualization
try:
    from pm4py.objects.bpmn.importer import importer as bpmn_importer
    from pm4py.visualization.bpmn import visualizer as bpmn_visualizer
    PM4PY_AVAILABLE = True
except ImportError:
    PM4PY_AVAILABLE = False
    print("Warning: pm4py not available. BPMN visualization will be disabled.")


class BPMNIndiGologUI:
    """Main UI class for BPMN to IndiGolog translation and reasoning."""
    
    def __init__(self):
        """Initialize the UI."""
        self.translator = TranslatorService()
        self.current_model = None
        self.reasoner = None
        self.current_bpmn_path = None
    
    def visualize_bpmn(self, bpmn_path):
        """
        Visualize a BPMN file using pm4py.
        
        Args:
            bpmn_path: Path to the BPMN file
        
        Returns:
            str: Path to the generated visualization image, or None if failed
        """
        if not PM4PY_AVAILABLE:
            return None
        
        if not bpmn_path or not os.path.exists(bpmn_path):
            return None
        
        try:
            # Import BPMN model
            bpmn_graph = bpmn_importer.apply(bpmn_path)
            
            # Generate visualization
            gviz = bpmn_visualizer.apply(bpmn_graph)
            
            # Save to temporary file
            temp_dir = tempfile.gettempdir()
            output_path = os.path.join(temp_dir, "bpmn_visualization.png")
            bpmn_visualizer.save(gviz, output_path)
            
            return output_path
        except Exception as e:
            print(f"Error visualizing BPMN: {e}")
            return None
    
    def translate_bpmn(self, bpmn_file, model_name):
        """
        Handle BPMN file upload and translation.
        
        Args:
            bpmn_file: Uploaded BPMN file from Gradio
            model_name: Name for the model
        
        Returns:
            tuple: (status_message, prolog_code, model_name_for_state, inspect_button_visible, viz_button_visible)
        """
        if bpmn_file is None:
            return "⚠️ Please upload a BPMN file", "", "", gr.update(visible=False), gr.update(visible=False)
        
        if not model_name or not model_name.strip():
            return "⚠️ Please provide a model name", "", "", gr.update(visible=False), gr.update(visible=False)
        
        model_name = model_name.strip()
        
        # Store BPMN path for visualization
        self.current_bpmn_path = bpmn_file.name
        
        # Perform translation
        success, message, pl_path, prolog_code = self.translator.translate_bpmn_file(
            bpmn_file.name, model_name
        )
        
        if success:
            self.current_model = model_name
            success_msg = f"✅ Translation completed successfully!\n\nModel: {model_name}\nReady for reasoning tasks."
            viz_button_visible = PM4PY_AVAILABLE
            return success_msg, prolog_code, model_name, gr.update(visible=True), gr.update(visible=viz_button_visible)
        else:
            return f"❌ Translation failed:\n{message}", "", "", gr.update(visible=False), gr.update(visible=False)
    
    def load_existing_model(self, model_name):
        """
        Load an existing translated model.
        
        Args:
            model_name: Name of the model to load
        
        Returns:
            tuple: (status_message, prolog_code, model_name_for_state, inspect_button_visible, viz_button_visible)
        """
        if not model_name or model_name == "Select a model...":
            return "⚠️ Please select a model", "", "", gr.update(visible=False), gr.update(visible=False)
        
        success, content = self.translator.get_translated_prolog(model_name)
        
        if success:
            self.current_model = model_name
            
            # Try to find the corresponding BPMN file
            bpmn_path = os.path.join("models", f"{model_name}.bpmn")
            if os.path.exists(bpmn_path):
                self.current_bpmn_path = bpmn_path
                viz_button_visible = PM4PY_AVAILABLE
            else:
                self.current_bpmn_path = None
                viz_button_visible = False
            
            success_msg = f"✅ Model loaded successfully!\n\nModel: {model_name}\nReady for reasoning tasks."
            return success_msg, content, model_name, gr.update(visible=True), gr.update(visible=viz_button_visible)
        else:
            return f"❌ Error loading model:\n{content}", "", "", gr.update(visible=False), gr.update(visible=False)
    
    def get_reasoning_interface(self, model_name, task_id):
        """
        Generate the parameter inputs for a specific reasoning task.
        
        Args:
            model_name: Name of the model
            task_id: ID of the reasoning task
        
        Returns:
            dict: Updates for Gradio components
        """
        if not model_name:
            return {
                param_row: gr.update(visible=False) for param_row in [
                    'param_row_1', 'param_row_2', 'param_row_3'
                ]
            } | {
                'param_label_1': gr.update(value=""),
                'param_label_2': gr.update(value=""),
                'param_label_3': gr.update(value=""),
                'reasoning_output': gr.update(value="⚠️ Please translate or load a model first")
            }
        
        if not task_id or task_id == "Select a reasoning task...":
            return {
                param_row: gr.update(visible=False) for param_row in [
                    'param_row_1', 'param_row_2', 'param_row_3'
                ]
            } | {
                'param_label_1': gr.update(value=""),
                'param_label_2': gr.update(value=""),
                'param_label_3': gr.update(value=""),
                'reasoning_output': gr.update(value="")
            }
        
        # Initialize reasoner for the current model
        self.reasoner = ReasoningService(model_name)
        tasks = self.reasoner.get_available_tasks()
        
        if task_id not in tasks:
            return {
                param_row: gr.update(visible=False) for param_row in [
                    'param_row_1', 'param_row_2', 'param_row_3'
                ]
            } | {
                'param_label_1': gr.update(value=""),
                'param_label_2': gr.update(value=""),
                'param_label_3': gr.update(value=""),
                'reasoning_output': gr.update(value=f"❌ Unknown task: {task_id}")
            }
        
        task_info = tasks[task_id]
        parameters = task_info['parameters']
        
        # Build updates for parameter rows
        updates = {'reasoning_output': gr.update(value="")}
        
        for i in range(3):
            param_row_name = f'param_row_{i+1}'
            label_name = f'param_label_{i+1}'
            input_name = f'param_input_{i+1}'
            
            if i < len(parameters):
                param = parameters[i]
                updates[param_row_name] = gr.update(visible=True)
                
                # Build label with description
                label_text = f"**{param['label']}**"
                if 'placeholder' in param and param['placeholder']:
                    label_text += f"\n\n*Example: {param['placeholder']}*"
                if param['type'] == 'dropdown':
                    label_text += f"\n\n*Options: {', '.join(param['choices'])}*"
                
                updates[label_name] = gr.update(value=label_text)
                
                # Set default value
                default_val = param.get('default', '')
                if param['type'] == 'dropdown' and not default_val:
                    default_val = param['choices'][0]
                
                updates[input_name] = gr.update(
                    value=str(default_val),
                    placeholder=param.get('placeholder', ''),
                    visible=True
                )
            else:
                updates[param_row_name] = gr.update(visible=False)
                updates[label_name] = gr.update(value="")
        
        return updates
    
    def execute_reasoning_task(self, model_name, task_id, param1, param2, param3):
        """
        Execute a reasoning task with the provided parameters.
        
        Args:
            model_name: Name of the model
            task_id: ID of the reasoning task
            param1, param2, param3: Parameter values
        
        Returns:
            str: Output from the reasoning task
        """
        if not model_name:
            return "⚠️ Please translate or load a model first"
        
        if not task_id or task_id == "Select a reasoning task...":
            return "⚠️ Please select a reasoning task"
        
        # Initialize reasoner if needed
        if not self.reasoner or self.reasoner.model_name != model_name:
            self.reasoner = ReasoningService(model_name)
        
        tasks = self.reasoner.get_available_tasks()
        if task_id not in tasks:
            return f"❌ Unknown task: {task_id}"
        
        # Build parameters dictionary
        task_info = tasks[task_id]
        parameters = {}
        param_values = [param1, param2, param3]
        
        for i, param_def in enumerate(task_info['parameters']):
            if i < len(param_values):
                param_name = param_def['name']
                parameters[param_name] = param_values[i]
        
        # Execute the task
        try:
            success, output = self.reasoner.execute_task(task_id, parameters)
            
            if success:
                result = f"✓ Task completed successfully\n\n{output}"
            else:
                result = f"❌ Task failed:\n{output}"
            
            return result
        
        except Exception as e:
            return f"❌ Error executing task:\n{str(e)}"
    
    def create_interface(self):
        """Create and return the Gradio interface."""
        
        with gr.Blocks(title="BPMN to IndiGolog Translator & Reasoner", theme=gr.themes.Soft()) as interface:
            gr.Markdown("""
            # 🔄 BPMN to IndiGolog Translation & Reasoning
            
            This tool allows you to:
            1. **Translate** BPMN models to IndiGolog Prolog code
            2. **View** the generated Prolog translation
            3. **Perform** various reasoning tasks over the translated models
            """)
            
            # State to track current model
            current_model_state = gr.State("")
            
            with gr.Tabs():
                # ===== TAB 1: Translation =====
                with gr.Tab("📤 Translation"):
                    gr.Markdown("### Upload and Translate BPMN Model")
                    
                    with gr.Row():
                        with gr.Column(scale=1):
                            bpmn_file_input = gr.File(
                                label="Upload BPMN File",
                                file_types=[".bpmn", ".xml"],
                                type="filepath"
                            )
                            model_name_input = gr.Textbox(
                                label="Model Name",
                                placeholder="e.g., job_application",
                                info="Name for the translated model"
                            )
                            translate_btn = gr.Button("🔄 Translate", variant="primary", size="lg")
                        
                        with gr.Column(scale=1):
                            translation_status = gr.Textbox(
                                label="Status",
                                lines=5,
                                interactive=False
                            )
                    
                    with gr.Row():
                        inspect_code_btn = gr.Button("🔍 Inspect Generated IndiGolog Code", visible=False, size="sm")
                        visualize_bpmn_btn = gr.Button("📊 Show BPMN Visualization", visible=False, size="sm")
                    
                    prolog_output = gr.Code(
                        label="Generated IndiGolog Code",
                        language="python",  # Using python for syntax highlighting (similar to prolog)
                        lines=20,
                        visible=False
                    )
                    
                    bpmn_visualization = gr.Image(
                        label="BPMN Visualization",
                        visible=False,
                        type="filepath"
                    )
                    
                    # Wire up translation
                    translate_result = translate_btn.click(
                        fn=self.translate_bpmn,
                        inputs=[bpmn_file_input, model_name_input],
                        outputs=[translation_status, prolog_output, current_model_state, inspect_code_btn, visualize_bpmn_btn]
                    )
                    
                    # Toggle code visibility
                    def toggle_code_visibility(current_visible):
                        """Toggle between showing and hiding code."""
                        new_visible = not current_visible
                        button_text = "🔍 Inspect Generated IndiGolog Code" if not new_visible else "❌ Hide IndiGolog Code"
                        return gr.update(visible=new_visible), gr.update(value=button_text)
                    
                    # Track visibility state for translation tab
                    code_visible_state = gr.State(False)
                    
                    inspect_code_btn.click(
                        fn=toggle_code_visibility,
                        inputs=[code_visible_state],
                        outputs=[prolog_output, inspect_code_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[code_visible_state],
                        outputs=[code_visible_state]
                    )
                    
                    # Toggle BPMN visualization
                    def toggle_bpmn_visualization(current_visible):
                        """Toggle between showing and hiding BPMN visualization."""
                        new_visible = not current_visible
                        button_text = "📊 Show BPMN Visualization" if not new_visible else "❌ Hide BPMN Visualization"
                        
                        # Generate visualization if showing
                        if new_visible and self.current_bpmn_path:
                            viz_path = self.visualize_bpmn(self.current_bpmn_path)
                            return gr.update(visible=new_visible, value=viz_path), gr.update(value=button_text)
                        else:
                            return gr.update(visible=new_visible), gr.update(value=button_text)
                    
                    # Track visibility state for BPMN visualization
                    bpmn_viz_visible_state = gr.State(False)
                    
                    visualize_bpmn_btn.click(
                        fn=toggle_bpmn_visualization,
                        inputs=[bpmn_viz_visible_state],
                        outputs=[bpmn_visualization, visualize_bpmn_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[bpmn_viz_visible_state],
                        outputs=[bpmn_viz_visible_state]
                    )
                
                # ===== TAB 2: Load Existing Model =====
                with gr.Tab("📂 Load Existing Model"):
                    gr.Markdown("### Load Previously Translated Model")
                    
                    with gr.Row():
                        with gr.Column(scale=1):
                            model_list = gr.Dropdown(
                                label="Select Model",
                                choices=["Select a model..."] + self.translator.list_available_models(),
                                value="Select a model...",
                                interactive=True
                            )
                            load_btn = gr.Button("📂 Load Model", variant="primary", size="lg")
                        
                        with gr.Column(scale=1):
                            load_status = gr.Textbox(
                                label="Status",
                                lines=5,
                                interactive=False
                            )
                    
                    with gr.Row():
                        inspect_loaded_code_btn = gr.Button("🔍 Inspect IndiGolog Code", visible=False, size="sm")
                        visualize_loaded_bpmn_btn = gr.Button("📊 Show BPMN Visualization", visible=False, size="sm")
                    
                    loaded_prolog_output = gr.Code(
                        label="IndiGolog Code",
                        language="python",  # Using python for syntax highlighting (similar to prolog)
                        lines=20,
                        visible=False
                    )
                    
                    loaded_bpmn_visualization = gr.Image(
                        label="BPMN Visualization",
                        visible=False,
                        type="filepath"
                    )
                    
                    # Wire up loading
                    load_btn.click(
                        fn=self.load_existing_model,
                        inputs=[model_list],
                        outputs=[load_status, loaded_prolog_output, current_model_state, inspect_loaded_code_btn, visualize_loaded_bpmn_btn]
                    )
                    
                    # Toggle code visibility for loaded models
                    def toggle_loaded_code_visibility(current_visible):
                        """Toggle between showing and hiding loaded code."""
                        new_visible = not current_visible
                        button_text = "🔍 Inspect IndiGolog Code" if not new_visible else "❌ Hide IndiGolog Code"
                        return gr.update(visible=new_visible), gr.update(value=button_text)
                    
                    # Track visibility state for load tab
                    loaded_code_visible_state = gr.State(False)
                    
                    inspect_loaded_code_btn.click(
                        fn=toggle_loaded_code_visibility,
                        inputs=[loaded_code_visible_state],
                        outputs=[loaded_prolog_output, inspect_loaded_code_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[loaded_code_visible_state],
                        outputs=[loaded_code_visible_state]
                    )
                    
                    # Toggle BPMN visualization for loaded models
                    def toggle_loaded_bpmn_visualization(current_visible):
                        """Toggle between showing and hiding BPMN visualization for loaded models."""
                        new_visible = not current_visible
                        button_text = "📊 Show BPMN Visualization" if not new_visible else "❌ Hide BPMN Visualization"
                        
                        # Generate visualization if showing
                        if new_visible and self.current_bpmn_path:
                            viz_path = self.visualize_bpmn(self.current_bpmn_path)
                            return gr.update(visible=new_visible, value=viz_path), gr.update(value=button_text)
                        else:
                            return gr.update(visible=new_visible), gr.update(value=button_text)
                    
                    # Track visibility state for loaded BPMN visualization
                    loaded_bpmn_viz_visible_state = gr.State(False)
                    
                    visualize_loaded_bpmn_btn.click(
                        fn=toggle_loaded_bpmn_visualization,
                        inputs=[loaded_bpmn_viz_visible_state],
                        outputs=[loaded_bpmn_visualization, visualize_loaded_bpmn_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[loaded_bpmn_viz_visible_state],
                        outputs=[loaded_bpmn_viz_visible_state]
                    )
                
                # ===== TAB 3: Reasoning =====
                with gr.Tab("🧠 Reasoning Tasks"):
                    gr.Markdown("### Perform Reasoning Tasks")
                    
                    with gr.Row():
                        with gr.Column(scale=1):
                            current_model_display = gr.Textbox(
                                label="Current Model",
                                value="No model loaded",
                                interactive=False
                            )
                            
                            reasoning_task_dropdown = gr.Dropdown(
                                label="Select Reasoning Task",
                                choices=["Select a reasoning task..."] + [
                                    f"{task_id}: {info['name']}" 
                                    for task_id, info in ReasoningService.REASONING_TASKS.items()
                                ],
                                value="Select a reasoning task...",
                                info="Choose the type of reasoning to perform"
                            )
                            
                            gr.Markdown("### Task Parameters")
                            
                            # Dynamic parameter inputs
                            with gr.Row(visible=False, elem_id="param_row_1") as param_row_1:
                                param_label_1 = gr.Markdown("Parameter 1")
                                param_input_1 = gr.Textbox(label="", placeholder="", scale=2)
                            
                            with gr.Row(visible=False, elem_id="param_row_2") as param_row_2:
                                param_label_2 = gr.Markdown("Parameter 2")
                                param_input_2 = gr.Textbox(label="", placeholder="", scale=2)
                            
                            with gr.Row(visible=False, elem_id="param_row_3") as param_row_3:
                                param_label_3 = gr.Markdown("Parameter 3")
                                param_input_3 = gr.Textbox(label="", placeholder="", scale=2)
                            
                            execute_btn = gr.Button("▶️ Execute Task", variant="primary", size="lg")
                    
                        with gr.Column(scale=2):
                            reasoning_output = gr.Textbox(
                                label="Reasoning Output",
                                lines=25,
                                interactive=False,
                                show_label=True
                            )
                    
                    # Update current model display when state changes
                    current_model_state.change(
                        fn=lambda m: f"Current Model: {m}" if m else "No model loaded",
                        inputs=[current_model_state],
                        outputs=[current_model_display]
                    )
                    
                    # Update parameter inputs when task is selected
                    def update_interface(model, task):
                        task_id = task.split(':')[0].strip() if ':' in task else task
                        updates = self.get_reasoning_interface(model, task_id)
                        return [
                            updates.get('param_row_1', gr.update()),
                            updates.get('param_label_1', gr.update()),
                            updates.get('param_input_1', gr.update()),
                            updates.get('param_row_2', gr.update()),
                            updates.get('param_label_2', gr.update()),
                            updates.get('param_input_2', gr.update()),
                            updates.get('param_row_3', gr.update()),
                            updates.get('param_label_3', gr.update()),
                            updates.get('param_input_3', gr.update()),
                            updates.get('reasoning_output', gr.update())
                        ]
                    
                    reasoning_task_dropdown.change(
                        fn=update_interface,
                        inputs=[current_model_state, reasoning_task_dropdown],
                        outputs=[
                            param_row_1, param_label_1, param_input_1,
                            param_row_2, param_label_2, param_input_2,
                            param_row_3, param_label_3, param_input_3,
                            reasoning_output
                        ]
                    )
                    
                    # Execute reasoning task
                    execute_btn.click(
                        fn=lambda model, task, p1, p2, p3: self.execute_reasoning_task(
                            model,
                            task.split(':')[0].strip() if ':' in task else task,
                            p1, p2, p3
                        ),
                        inputs=[
                            current_model_state,
                            reasoning_task_dropdown,
                            param_input_1,
                            param_input_2,
                            param_input_3
                        ],
                        outputs=[reasoning_output]
                    )
                
                # ===== TAB 4: Quick Guide =====
                with gr.Tab("📚 Quick Guide"):
                    gr.Markdown("""
                    # 📚 Quick Guide to BPMN Translation & Reasoning
                    
                    ## Getting Started
                    
                    ### 1️⃣ Translation Tab
                    **Upload and translate BPMN models to IndiGolog**
                    
                    - 📤 **Upload** a BPMN file (.bpmn or .xml format)
                    - ✏️ **Provide** a name for your model (e.g., "job_application")
                    - 🔄 **Click** "Translate" to generate IndiGolog Prolog code
                    - ✅ You'll see a success message when translation completes
                    - 🔍 **Optional**: Click "Inspect Generated IndiGolog Code" to view the Prolog translation
                    
                    ---
                    
                    ### 2️⃣ Load Existing Model Tab
                    **Access previously translated models**
                    
                    - 📂 **Select** from the dropdown of available models
                    - 📥 **Click** "Load Model" to load it
                    - 🔍 **Optional**: Click "Inspect IndiGolog Code" to view the Prolog code
                    
                    ---
                    
                    ### 3️⃣ Reasoning Tasks Tab
                    **Perform formal reasoning over your BPMN models**
                    
                    First, ensure you have a model loaded (via Translation or Load Existing Model tabs).
                    
                    #### Available Reasoning Tasks:
                    
                    **🔮 Projection**
                    - **Purpose**: Check what fluents (state variables) would be true/false after executing a sequence of actions
                    - **Parameters**:
                      - *Fluent Name*: The state variable to check (e.g., `door_open`, `application(1)`)
                      - *Action Sequence*: Comma-separated actions (e.g., `open,close,open`)
                      - *Expected Value*: Choose `true` or `false`
                    - **Example**: Check if `application(1)` is true after `job_needed(1),prepare_application(end,1)`
                    
                    **✓ Legality Check**
                    - **Purpose**: Verify if a sequence of actions is executable (all preconditions are satisfied)
                    - **Parameters**:
                      - *Action Sequence*: Comma-separated actions to check
                      - *Procedure Name*: Optional name for the check (default: legality_check)
                    - **Example**: Verify `job_needed(1),prepare_application(end,1)`
                    
                    **▶️ Process Execution**
                    - **Purpose**: Execute the full BPMN process (automatically uses controller 1 for bpmn_process)
                    - **Parameters**: None (just start the execution)
                    - **Note**: This opens an interactive window for exogenous events
                    
                    **📋 Conformance Checking**
                    - **Purpose**: Verify if an execution history conforms to the process specification
                    - **Parameters**:
                      - *History Actions*: Comma-separated list of actions that were executed
                    - **Example**: Check if `job_needed(1),prepare_application(end,1),check_validity(end,1,true)` is valid
                    
                    **🔍 Property Verification**
                    - **Purpose**: Execute a custom reasoning task defined in your model to verify specific properties
                    - **Parameters**:
                      - *Procedure Name*: Name of the verification procedure (default: reasoning_task)
                    - **Note**: Requires a `proc(control(reasoning_task), ...)` defined in your model
                    
                    ---
                    
                    ## Tips
                    
                    - 💡 **Action Format**: Actions can have parameters, e.g., `job_needed(1)`, `prepare_application(end,1)`
                    - 💡 **Comma Separation**: Always separate multiple actions with commas, no spaces
                    - 💡 **Case Sensitive**: Action and fluent names are case-sensitive
                    - 💡 **Results**: Clean, user-friendly results are shown after each reasoning task
                    
                    ---
                    
                    ## Example Workflow
                    
                    1. **Upload** `job_application.bpmn` in the Translation tab
                    2. **Name** it "job_application" and click Translate
                    3. Go to **Reasoning Tasks** tab
                    4. Select **Legality Check**
                    5. Enter action sequence: `job_needed(1),prepare_application(end,1)`
                    6. Click **Execute Task**
                    7. See if the sequence is executable! ✅
                    """)
        
        return interface


def main():
    """Main function to launch the UI."""
    ui = BPMNIndiGologUI()
    interface = ui.create_interface()
    
    print("\n" + "="*70)
    print("🚀 Starting BPMN to IndiGolog Translation & Reasoning UI")
    print("="*70)
    print("\n📝 The web interface will open in your browser.")
    print("   If it doesn't open automatically, use the URL shown below.\n")
    
    interface.launch(
        server_name="127.0.0.1",
        server_port=7860,
        share=False,
        show_error=True
    )


if __name__ == "__main__":
    main()
