import shutil
from pathlib import Path


def reorganize_bpmn_files(base_dir):
    base_path = Path(base_dir)
    bpmn_files = []
    subdirs = []
    
    for item in sorted(base_path.iterdir()):
        if item.is_dir():
            subdirs.append(item)
            # Find all .bpmn files in this subdirectory
            for bpmn_file in sorted(item.glob('*.bpmn')):
                bpmn_files.append(bpmn_file)
    
    print(f"Found {len(bpmn_files)} BPMN files in {len(subdirs)} subdirectories")
    counter = 1
    moved_files = []
    
    for bpmn_file in bpmn_files:
        new_name = f"process_{counter}.bpmn"
        new_path = base_path / new_name
        
        print(f"Moving {bpmn_file.relative_to(base_path)} -> {new_name}")
        shutil.move(str(bpmn_file), str(new_path))
        moved_files.append(new_path)
        counter += 1
    
    print(f"\nMoved {len(moved_files)} files")
    print("\nDeleting subdirectories...")
    for subdir in subdirs:
        try:
            shutil.rmtree(subdir)
            print(f"Deleted: {subdir.name}")
        except Exception as e:
            print(f"Error deleting {subdir.name}: {e}")
    
    print("\nReorganization complete!")
    print(f"Total BPMN files in {base_path}: {len(moved_files)}")


if __name__ == "__main__":
    script_dir = Path(__file__).parent

    print(f"This script will reorganize BPMN files in: {script_dir}")
    print("All BPMN files from subdirectories will be moved to the processed folder")
    print("with names like process_1.bpmn, process_2.bpmn, etc.")
    print("All subdirectories will be deleted.\n")
    
    response = input("Do you want to continue? (yes/no): ").strip().lower()
    
    if response in ['yes', 'y']:
        reorganize_bpmn_files(script_dir)
    else:
        print("Operation cancelled.")
