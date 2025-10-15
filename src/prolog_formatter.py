"""
Utility functions for formatting Prolog code
"""

def format_proc_body(proc_body, indent_level=2):
    """
    Format a Prolog procedure body with proper indentation.
    
    Args:
        proc_body: The procedure body string
        indent_level: Starting indentation level (default 2 for inside proc(...))
        
    Returns:
        Formatted string with newlines and indentation
    """
    if not proc_body or len(proc_body) < 100:
        return proc_body
    
    result = []
    current_line = ""
    depth = 0
    indent = " " * indent_level
    i = 0
    
    while i < len(proc_body):
        char = proc_body[i]
        
        if char == '[':
            current_line += char
            result.append(indent * depth + current_line)
            depth += 1
            current_line = ""
            
        elif char == ']':
            if current_line.strip():
                result.append(indent * depth + current_line.strip())
                current_line = ""
            depth = max(0, depth - 1)
            result.append(indent * depth + ']')
            
        elif char == ',' and depth > 0:
            current_line += char
            # Check if this is a major separator
            if i + 1 < len(proc_body) and proc_body[i + 1] == ' ':
                # Look ahead to see if next element is significant
                next_significant = proc_body[i + 1:i + 15].strip()
                if next_significant.startswith(('if(', 'while(', 'conc(', '[', '?(')):
                    result.append(indent * depth + current_line.strip())
                    current_line = ""
                else:
                    current_line += ' '
            else:
                current_line += ' '
                
        else:
            current_line += char
        
        i += 1
    
    if current_line.strip():
        result.append(indent * depth + current_line.strip())
    
    return '\n'.join(result)


def format_proc_clause(proc_head, proc_body):
    """
    Format a complete proc(...) clause.
    
    Args:
        proc_head: The proc head like "main_process(ID)"
        proc_body: The proc body
        
    Returns:
        Formatted proc clause
    """
    formatted_body = format_proc_body(proc_body, indent_level=2)
    
    # If body is simple (one line), keep it simple
    if '\n' not in formatted_body and len(formatted_body) < 80:
        return f"proc({proc_head},\n  {formatted_body}\n)."
    
    return f"proc({proc_head},\n{formatted_body}\n)."
