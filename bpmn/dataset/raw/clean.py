import os

def delete_xml_by_quality(root_folder='.'):
    """
    Iterates through folders and deletes XML files where corresponding
    quality.txt doesn't contain the value 5.
    Also moves .txt files from root to matching folder names.
    """
    deleted_count = 0
    processed_count = 0
    moved_count = 0
    
    print("\n" + "="*50)
    print("Moving .txt files to matching folders...")
    print("="*50)
    
    root_files = [f for f in os.listdir(root_folder) if os.path.isfile(os.path.join(root_folder, f)) and f.endswith('.txt')]
    
    for txt_file in root_files:
        folder_name = txt_file.replace('.txt', '')
        folder_path = os.path.join(root_folder, folder_name)
        
        if os.path.isdir(folder_path):
            src_path = os.path.join(root_folder, txt_file)
            dst_path = os.path.join(folder_path, txt_file)
            
            try:
                os.rename(src_path, dst_path)
                moved_count += 1
                print(f"  ✓ Moved {txt_file} to {folder_name}/")
            except Exception as e:
                print(f"  ✗ Error moving {txt_file}: {str(e)}")
        else:
            print(f"  ⚠ No matching folder for {txt_file}")
    
    print(f"\nMoved {moved_count} .txt files")
    
    print("\n" + "="*50)
    print("Processing XML files based on quality...")
    print("="*50)
    
    for folder_name in os.listdir(root_folder):
        folder_path = os.path.join(root_folder, folder_name)
        
        # Skip if not a directory
        if not os.path.isdir(folder_path):
            continue
        
        print(f"\nProcessing folder: {folder_name}")
        
        files = os.listdir(folder_path)
        quality_files = [f for f in files if f.endswith('.quality.txt')]
        for quality_file in quality_files:
            prefix = quality_file.replace('.quality.txt', '')
            quality_path = os.path.join(folder_path, quality_file)
            xml_file = f"{prefix}.bpmn2.xml"
            xml_path = os.path.join(folder_path, xml_file)
    
            if not os.path.exists(xml_path):
                print(f"  ⚠ XML file not found: {xml_file}")
                continue
            
            processed_count += 1
            try:
                with open(quality_path, 'r') as f:
                    quality_content = f.read().strip()
                if quality_content != '5':
                    os.remove(xml_path)
                    deleted_count += 1
                    print(f"  ✓ Deleted {xml_file} (quality: {quality_content})")
                else:
                    print(f"  ○ Kept {xml_file} (quality: {quality_content})")
                os.remove(quality_path)
                    
            except Exception as e:
                print(f"  ✗ Error processing {quality_file}: {str(e)}")
    
    print(f"\n{'='*50}")
    print(f"Summary:")
    print(f"  Moved: {moved_count} .txt files")
    print(f"  Processed: {processed_count} XML files")
    print(f"  Deleted: {deleted_count} XML files")
    print(f"  Kept: {processed_count - deleted_count} XML files")
    print(f"{'='*50}")

if __name__ == "__main__":
    root_path = '.'    
    print("Starting XML cleanup based on quality values...")
    print(f"Root folder: {os.path.abspath(root_path)}")
    response = input("\nThis will delete XML files. Continue? (yes/no): ")
    
    if response.lower() in ['yes', 'y']:
        delete_xml_by_quality(root_path)
    else:
        print("Operation cancelled.")
