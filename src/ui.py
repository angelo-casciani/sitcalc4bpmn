import os
import sys
from pathlib import Path
import tempfile

import gradio as gr
from pm4py.objects.bpmn.importer import importer as bpmn_importer
from pm4py.visualization.bpmn import visualizer as bpmn_visualizer
from translator_service import TranslatorService
from reasoning_service import ReasoningService

sys.path.insert(0, str(Path(__file__).parent))

class BPMNIndiGologUI:
    def __init__(self):
        self.translator = TranslatorService()
        self.current_model = None
        self.reasoner = None
        self.current_bpmn_path = None
    
    def visualize_bpmn(self, bpmn_path):        
        if not bpmn_path or not os.path.exists(bpmn_path):
            return None
        try:
            bpmn_graph = bpmn_importer.apply(bpmn_path)
            gviz = bpmn_visualizer.apply(bpmn_graph)
            temp_dir = tempfile.gettempdir()
            output_path = os.path.join(temp_dir, "bpmn_visualization.png")
            bpmn_visualizer.save(gviz, output_path)
            return output_path
        except Exception as e:
            print(f"Error visualizing BPMN: {e}")
            return None
    
    def translate_bpmn(self, bpmn_file, model_name):
        if bpmn_file is None:
            return "⚠️ Please upload a BPMN file", "", "", gr.update(visible=False), gr.update(visible=False)
        if not model_name or not model_name.strip():
            return "⚠️ Please provide a model name", "", "", gr.update(visible=False), gr.update(visible=False)

        model_name = model_name.strip()
        self.current_bpmn_path = bpmn_file.name
        success, message, pl_path, prolog_code = self.translator.translate_bpmn_file(bpmn_file.name, model_name)
        
        if success:
            self.current_model = model_name
            success_msg = f"✅ Translation completed successfully!\n\nModel: {model_name}\nReady for reasoning tasks."
            return success_msg, prolog_code, model_name, gr.update(visible=True), gr.update(visible=True)
        else:
            return f"❌ Translation failed:\n{message}", "", "", gr.update(visible=False), gr.update(visible=False)
    
    def load_existing_model(self, model_name):
        if not model_name or model_name == "Select a model...":
            return "⚠️ Please select a model", "", "", gr.update(visible=False), gr.update(visible=False)
        
        success, content = self.translator.get_translated_prolog(model_name)
        if success:
            self.current_model = model_name
            # Try multiple file extensions for BPMN visualization
            possible_extensions = ['.bpmn', '.bpmn2.xml', '.xml']
            self.current_bpmn_path = None
            
            for ext in possible_extensions:
                bpmn_path = os.path.join("bpmn", f"{model_name}{ext}")
                if os.path.exists(bpmn_path):
                    self.current_bpmn_path = bpmn_path
                    break
            
            success_msg = f"✅ Model loaded successfully!\n\nModel: {model_name}\nReady for reasoning tasks."
            return success_msg, content, model_name, gr.update(visible=True), gr.update(visible=True)
        else:
            return f"❌ Error loading model:\n{content}", "", "", gr.update(visible=False), gr.update(visible=False)
    
    def get_reasoning_interface(self, model_name, task_id):
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
        updates = {'reasoning_output': gr.update(value="")}
        
        for i in range(3):
            param_row_name = f'param_row_{i+1}'
            label_name = f'param_label_{i+1}'
            input_name = f'param_input_{i+1}'
            
            if i < len(parameters):
                param = parameters[i]
                updates[param_row_name] = gr.update(visible=True)
                label_text = f"**{param['label']}**"
                if 'placeholder' in param and param['placeholder']:
                    label_text += f"\n\n*Example: {param['placeholder']}*"
                if param['type'] == 'dropdown':
                    label_text += f"\n\n*Options: {', '.join(param['choices'])}*"
                
                updates[label_name] = gr.update(value=label_text)
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
        if not model_name:
            return "⚠️ Please translate or load a model first"
        
        if not task_id or task_id == "Select a reasoning task...":
            return "⚠️ Please select a reasoning task"
        
        if not self.reasoner or self.reasoner.model_name != model_name:
            self.reasoner = ReasoningService(model_name)
        
        tasks = self.reasoner.get_available_tasks()
        if task_id not in tasks:
            return f"❌ Unknown task: {task_id}"
        
        task_info = tasks[task_id]
        parameters = {}
        param_values = [param1, param2, param3]
        
        for i, param_def in enumerate(task_info['parameters']):
            if i < len(param_values):
                param_name = param_def['name']
                parameters[param_name] = param_values[i]
        
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
        with gr.Blocks(title="BPMN to IndiGolog Translator & Reasoner", theme=gr.themes.Soft()) as interface:
            gr.Markdown("""
            # 🔄 BPMN to IndiGolog Translation & Reasoning
            
            This tool allows you to:
            1. **Translate** BPMN models to IndiGolog Prolog code
            2. **Inspect** the generated Prolog translation
            3. **Perform** various reasoning tasks over the translated models
            """)
            
            current_model_state = gr.State("")
            with gr.Tabs():
                # TAB 1: Translation
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
                        language="python",
                        lines=20,
                        visible=False
                    )
                    bpmn_visualization = gr.Image(
                        label="BPMN Visualization",
                        visible=False,
                        type="filepath"
                    )
                    translate_result = translate_btn.click(
                        fn=self.translate_bpmn,
                        inputs=[bpmn_file_input, model_name_input],
                        outputs=[translation_status, prolog_output, current_model_state, inspect_code_btn, visualize_bpmn_btn]
                    )
                    
                    def toggle_code_visibility(current_visible):
                        new_visible = not current_visible
                        button_text = "🔍 Inspect Generated IndiGolog Code" if not new_visible else "❌ Hide IndiGolog Code"
                        return gr.update(visible=new_visible), gr.update(value=button_text)
                    code_visible_state = gr.State(False)
                    inspect_code_btn.click(
                        fn=toggle_code_visibility,
                        inputs=[code_visible_state],
                        outputs=[prolog_output, inspect_code_btn]
                    ).then(fn=lambda v: not v,
                           inputs=[code_visible_state],
                           outputs=[code_visible_state])
                    
                    def toggle_bpmn_visualization(current_visible):
                        new_visible = not current_visible
                        button_text = "📊 Show BPMN Visualization" if not new_visible else "❌ Hide BPMN Visualization"    
                        if new_visible and self.current_bpmn_path:
                            viz_path = self.visualize_bpmn(self.current_bpmn_path)
                            return gr.update(visible=new_visible, value=viz_path), gr.update(value=button_text)
                        else:
                            return gr.update(visible=new_visible), gr.update(value=button_text)
                    bpmn_viz_visible_state = gr.State(False)
                    
                    visualize_bpmn_btn.click(
                        fn=toggle_bpmn_visualization,
                        inputs=[bpmn_viz_visible_state],
                        outputs=[bpmn_visualization, visualize_bpmn_btn]
                    ).then(fn=lambda v: not v,
                           inputs=[bpmn_viz_visible_state],
                           outputs=[bpmn_viz_visible_state])
                
                # TAB 2: Load Existing Model
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
                        language="python",
                        lines=20,
                        visible=False
                    )
                    
                    loaded_bpmn_visualization = gr.Image(
                        label="BPMN Visualization",
                        visible=False,
                        type="filepath"
                    )
                    
                    load_btn.click(
                        fn=self.load_existing_model,
                        inputs=[model_list],
                        outputs=[load_status, loaded_prolog_output, current_model_state, inspect_loaded_code_btn, visualize_loaded_bpmn_btn]
                    )
                    
                    def toggle_loaded_code_visibility(current_visible):
                        """Toggle between showing and hiding loaded code."""
                        new_visible = not current_visible
                        button_text = "🔍 Inspect IndiGolog Code" if not new_visible else "❌ Hide IndiGolog Code"
                        return gr.update(visible=new_visible), gr.update(value=button_text)
                    loaded_code_visible_state = gr.State(False)
                    
                    inspect_loaded_code_btn.click(
                        fn=toggle_loaded_code_visibility,
                        inputs=[loaded_code_visible_state],
                        outputs=[loaded_prolog_output, inspect_loaded_code_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[loaded_code_visible_state],
                        outputs=[loaded_code_visible_state])
                    
                    def toggle_loaded_bpmn_visualization(current_visible):
                        new_visible = not current_visible
                        button_text = "📊 Show BPMN Visualization" if not new_visible else "❌ Hide BPMN Visualization"
                        
                        if new_visible and self.current_bpmn_path:
                            viz_path = self.visualize_bpmn(self.current_bpmn_path)
                            return gr.update(visible=new_visible, value=viz_path), gr.update(value=button_text)
                        else:
                            return gr.update(visible=new_visible), gr.update(value=button_text)
                    
                    loaded_bpmn_viz_visible_state = gr.State(False)
                    visualize_loaded_bpmn_btn.click(
                        fn=toggle_loaded_bpmn_visualization,
                        inputs=[loaded_bpmn_viz_visible_state],
                        outputs=[loaded_bpmn_visualization, visualize_loaded_bpmn_btn]
                    ).then(
                        fn=lambda v: not v,
                        inputs=[loaded_bpmn_viz_visible_state],
                        outputs=[loaded_bpmn_viz_visible_state])
                
                # TAB 3: Reasoning
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
                                show_label=True)
                    
                    current_model_state.change(
                        fn=lambda m: f"Current Model: {m}" if m else "No model loaded",
                        inputs=[current_model_state],
                        outputs=[current_model_display]
                    )
                    
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
                
                # TAB 4: Quick Guide
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
                    - **Purpose**: Check what fluents (state variables) would be true/false after executing a sequence of actions.
                    - **Parameters**:
                      - *Fluent Name*: The state variable to check (e.g., `active(1,applicant)`, `application(1)`).
                      - *Action Sequence*: Comma-separated actions (e.g., `job_needed(1),acquire(1,applicant)`).
                      - *Expected Truth Value*: Choose `true` or `false`.
                    - **Example**: Check if `active(1,applicant)` is true after `job_needed(1),acquire(1,applicant)`.
                    
                    **✓ Legality Check**
                    - **Purpose**: Verify if a sequence of actions is executable (all preconditions are satisfied).
                    - **Parameters**:
                      - *Action Sequence*: Comma-separated actions to check.
                    - **Example**: Verify `job_needed(1),prepare_application(end,1)`.
                    
                    **▶️ Process Execution**
                    - **Purpose**: Execute the BPMN process using the IndiGolog interpreter.
                    - **Parameters**: None (just start the execution).
                    - **Note**: This opens the IndiGolog interactive window for issuing exogenous events.
                    
                    **📋 Conformance Checking**
                    - **Purpose**: Verify if an execution history conforms to the process specification.
                    - **Parameters**:
                      - *History Actions*: Comma-separated list of actions that were executed.
                    - **Example**: Check if `job_needed(1),prepare_application(end,1),check_validity(end,1,true)` conforms to the model.
                    
                    **🔍 Property Verification**
                    - **Purpose**: Verify specific properties over the entire process execution.
                    - **Parameters**:
                      - *Property Conditions*: Comma-separated properties to verify.
                    - **Example**: Verify whether `signed_contract(id), not(done(application_finalised(id)))` holds for some instance id.
                    
                    ---
                    
                    ## Tips
                    
                    - 💡 **Action Format**: Actions can have parameters, e.g., `job_needed(1)`, `prepare_application(end,1)`.
                    - 💡 **Comma Separation**: Always separate multiple actions with commas, no spaces.
                    - 💡 **Case Sensitive**: Action and fluent names are case-sensitive, always use lower case.
                    
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
