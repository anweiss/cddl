import * as monaco from 'monaco-editor';

console.log('Script loaded! WASM validation starting...');

// Register CDDL language with Monaco Editor
monaco.languages.register({
  id: 'cddl',
  extensions: ['.cddl'],
  aliases: ['CDDL', 'cddl'],
  mimetypes: ['text/cddl']
});

// Define CDDL language tokens and syntax highlighting
monaco.languages.setMonarchTokensProvider('cddl', {
  tokenizer: {
    root: [
      // Comments
      [/;.*$/, 'comment'],
      
      // Built-in types
      [/\b(any|integer|uint|nint|int|bstr|bytes|tstr|text|tdate|time|number|biguint|bignint|bigint|unsigned|decfrac|bigfloat|eb64url|eb64legacy|eb16|encoded-cbor|uri|b64url|b64legacy|regexp|mime-message|cbor-any|float16|float32|float64|float16-32|float32-64|float|false|true|bool|nil|null|undefined)\b/, 'keyword'],
      
      // Numbers
      [/\b\d+(\.\d+)?([eE][+-]?\d+)?\b/, 'number'],
      [/#\w+/, 'number'],
      
      // Strings
      [/"([^"\\]|\\.)*$/, 'string.invalid'],
      [/"/, 'string', '@string'],
      [/'([^'\\]|\\.)*$/, 'string.invalid'],
      [/'/, 'string', '@string_single'],
      
      // Assignment operators
      [/(=|\/\/=|\/=)/, 'operator'],
      
      // Other operators
      [/(=>|:|\^|\?|\+|\*|\/\/|\/)/, 'operator'],
      
      // Control operators
      [/\.(size|bits|regexp|pcre|cbor|cborseq|within|and|lt|le|gt|ge|eq|ne|default)/, 'keyword.operator'],
      
      // Range operators
      [/\.\.\./, 'operator'],
      [/\.\./, 'operator'],
      
      // Socket/plug
      [/\$\$/, 'keyword.operator'],
      [/\$/, 'keyword.operator'],
      
      // Identifiers
      [/[a-zA-Z_][\w-]*/, 'identifier'],
      
      // Brackets
      [/[\[\]]/, 'bracket.square'],
      [/[{}]/, 'bracket.curly'],
      [/[()]/, 'bracket.parenthesis'],
      [/[<>]/, 'bracket.angle'],
      
      // Delimiters
      [/,/, 'delimiter'],
      
      // Whitespace
      [/[ \t\r\n]+/, 'white']
    ],
    
    string: [
      [/[^\\"]+/, 'string'],
      [/\\./, 'string.escape.invalid'],
      [/"/, 'string', '@pop']
    ],
    
    string_single: [
      [/[^\\']+/, 'string'],
      [/\\./, 'string.escape.invalid'],
      [/'/, 'string', '@pop']
    ]
  }
});

// Define CDDL theme colors
monaco.editor.defineTheme('cddl-theme', {
  base: 'vs',
  inherit: true,
  rules: [
    { token: 'comment', foreground: '6a994e', fontStyle: 'italic' },
    { token: 'keyword', foreground: '7c3aed', fontStyle: 'bold' },
    { token: 'keyword.operator', foreground: 'dc2626' },
    { token: 'operator', foreground: 'd73502' },
    { token: 'number', foreground: '1e40af' },
    { token: 'string', foreground: '166534' },
    { token: 'string.invalid', foreground: 'dc2626' },
    { token: 'string.escape.invalid', foreground: '92400e' },
    { token: 'identifier', foreground: '374151' },
    { token: 'bracket.square', foreground: '6b21a8' },
    { token: 'bracket.curly', foreground: 'a21caf' },
    { token: 'bracket.parenthesis', foreground: '0369a1' },
    { token: 'bracket.angle', foreground: '65a30d' },
    { token: 'delimiter', foreground: '6b7280' }
  ],
  colors: {}
});

// Add global error handlers to catch any unhandled errors
window.addEventListener('error', function(event) {
  console.error('Global error caught:', event.error);
});

window.addEventListener('unhandledrejection', function(event) {
  console.error('Unhandled promise rejection:', event.reason);
});

let wasmModule;

// Initialize the WASM module
async function initWasm() {
  try {
    console.log('Initializing WASM module...');
    
    // Import the WASM module from the pkg directory
    const wasmImport = await import('../pkg/cddl.js');
    
    console.log('WASM module imported, now initializing...');
    
    // Initialize the WASM module (this loads the actual .wasm binary)
    await wasmImport.default();
    
    console.log('WASM module initialized successfully');
    
    // Store the initialized module
    wasmModule = wasmImport;
    
    console.log('Available functions:', Object.keys(wasmModule));
    
    // Test that the function exists
    if (typeof wasmModule.cddl_from_str === 'function') {
      console.log('cddl_from_str function found');
      return true;
    } else {
      console.error('cddl_from_str function not found');
      return false;
    }
  } catch (error) {
    console.error('Failed to load WASM module:', error);
    return false;
  }
}

// Parse CDDL and return validation results
function validateCDDLText(cddlText) {
  try {
    if (!wasmModule || !wasmModule.cddl_from_str) {
      throw new Error('WASM module not initialized');
    }
    
    // Use the WASM function to validate CDDL
    console.log('Calling WASM cddl_from_str with input:', cddlText.substring(0, 100) + '...');
    const result = wasmModule.cddl_from_str(cddlText);
    console.log('CDDL validation result:', result);
    
    // If we get here without an exception, the CDDL is valid
    return {
      isValid: true,
      errors: []
    };
  } catch (error) {
    console.error('CDDL validation error:', error);
    console.log('Error type:', typeof error);
    console.log('Error string representation:', error.toString());
    console.log('Error object keys:', Object.keys(error));
    
    // Parse the error to extract useful information
    let errors = [];
    
    if (error && typeof error === 'object') {
      // Handle structured error from WASM
      console.log('Error type:', typeof error, 'Error object:', error);
      console.log('Error details:', {
        position: error.position,
        msg: error.msg,
        msgShort: error.msg?.short,
        msgExtended: error.msg?.extended
      });
      
      // Check if it's an array of errors
      if (Array.isArray(error)) {
        errors = error.map(err => formatErrorMessage(parseErrorObject(err)));
      } else if (error.position || error.msg) {
        // Single structured error
        errors = [formatErrorMessage(parseErrorObject(error))];
      } else {
        // Fallback to string parsing
        errors = [formatErrorMessage(parseErrorString(error.toString()))];
      }
    } else {
      // Handle string error
      errors = [formatErrorMessage(parseErrorString(error.toString()))];
    }
    
    return {
      isValid: false,
      errors: errors
    };
  }
}

// Helper function to parse structured error objects
function parseErrorObject(error) {
  console.log('parseErrorObject input:', error);
  console.log('parseErrorObject msg details:', {
    msgShort: error.msg?.short,
    msgExtended: error.msg?.extended,
    toString: error.toString()
  });
  
  const line = error.position?.line || 1;
  const column = error.position?.column || 1;
  const range = error.position?.range || null;
  
  // Try to extract the best message from various sources
  let message = error.msg?.short || error.msg?.extended || error.toString();
  console.log('Initial message selected:', message);
  
  // If we have rawMessage or the toString has diagnostic info, try to extract better message
  const rawMessage = error.toString();
  
  if (rawMessage && (rawMessage.includes('┌─') || rawMessage.includes('╰^') || rawMessage.includes('Lexer error'))) {
    // This looks like a diagnostic output, try to extract the actual error message
    const extractedMessage = extractMessageFromDiagnostic(rawMessage);
    console.log('Extracted message from diagnostic:', extractedMessage);
    
    if (extractedMessage && extractedMessage !== 'Lexer error.' && extractedMessage !== 'Parser error.') {
      message = extractedMessage;
      console.log('Using extracted message:', message);
    }
  }
  
  const result = {
    line: line,
    column: column,
    range: range,
    message: message,
    rawMessage: rawMessage
  };
  console.log('parseErrorObject result:', result);
  return result;
}

// Helper function to extract clean error messages from diagnostic output
function extractMessageFromDiagnostic(diagnostic) {
  // Look for the error message after "╰^" pattern
  const afterArrowMatch = diagnostic.match(/╰\^\s*(.+)$/m);
  if (afterArrowMatch) {
    let message = afterArrowMatch[1].trim();
    // Clean up any remaining diagnostic formatting characters
    message = message.replace(/[│├└┌┐┘╭╰╯╮─┬┴┼^|]/g, '').trim();
    return message;
  }
  
  // Fallback: extract the main error message from the end of the diagnostic
  const lines = diagnostic.split('\n');
  
  // Look for the actual error message (usually the last non-empty, non-formatting line)
  for (let i = lines.length - 1; i >= 0; i--) {
    const trimmed = lines[i]?.trim();
    
    // Skip empty lines, lines with box drawing characters, line numbers, and the error type prefix
    if (trimmed && 
        !trimmed.includes('─') && 
        !trimmed.includes('│') && 
        !trimmed.includes('╭') && 
        !trimmed.includes('╰') && 
        !trimmed.includes('┌') && 
        !trimmed.includes('┐') && 
        !trimmed.includes('└') && 
        !trimmed.includes('┘') && 
        !trimmed.includes('^') && 
        !trimmed.match(/^\d+\s*│/) &&
        !trimmed.match(/^(Lexer error|Parser error)/i)) {
      let message = trimmed;
      // Clean up any remaining diagnostic formatting characters
      message = message.replace(/[│├└┌┐┘╭╰╯╮─┬┴┼^|]/g, '').trim();
      return message;
    }
  }
  
  return null;
}

// Helper function to parse error strings
function parseErrorString(errorMessage) {
  let line = 1, column = 1;
  let message = errorMessage.replace(/^Error: /, '');
  
  // Parse WASM lexer/parser error format: "┌─ input:307:66"
  const wasmErrorMatch = errorMessage.match(/┌─ input:(\d+):(\d+)/);
  if (wasmErrorMatch) {
    line = parseInt(wasmErrorMatch[1]);
    column = parseInt(wasmErrorMatch[2]);
    
    // Use the shared diagnostic extraction function
    const extractedMessage = extractMessageFromDiagnostic(errorMessage);
    if (extractedMessage) {
      message = extractedMessage;
    } else {
      // If we couldn't find a good message, fall back to a cleaned version
      message = errorMessage.split('\n')[0].replace(/^(Lexer error|Parser error):\s*/i, '').trim();
    }
  } else {
    // Try to extract line number from error message (fallback)
    const lineMatch = errorMessage.match(/line (\d+)/i);
    if (lineMatch) {
      line = parseInt(lineMatch[1]);
    }
    
    // Try to extract column number from error message  
    const columnMatch = errorMessage.match(/column (\d+)/i);
    if (columnMatch) {
      column = parseInt(columnMatch[1]);
    }
  }
  
  return {
    line: line,
    column: column, 
    message: message,
    rawMessage: errorMessage
  };
}

// Enhanced error message formatting for better user experience
function formatErrorMessage(error) {
  let formattedMessage = error.message;
  let category = 'Syntax Error';
  let severity = 'error';
  
  // Clean up common WASM error messages, but preserve good messages we already extracted
  if (formattedMessage && !formattedMessage.match(/^(Lexer error|Parser error|Error)/i)) {
    // We already have a good message, just clean it up slightly
    formattedMessage = formattedMessage.trim();
    
    // Capitalize first letter if not already
    if (formattedMessage.length > 0 && formattedMessage[0] === formattedMessage[0].toLowerCase()) {
      formattedMessage = formattedMessage.charAt(0).toUpperCase() + formattedMessage.slice(1);
    }
    
    // Add period if missing and not already ending with punctuation
    if (formattedMessage.length > 0 && !/[.!?]$/.test(formattedMessage)) {
      formattedMessage += '.';
    }
  } else {
    // Remove technical prefixes for generic messages
    formattedMessage = formattedMessage
      .replace(/^(Lexer error|Parser error|Error):\s*/i, '')
      .replace(/^Error:\s*/i, '')
      .trim();
    
    // Capitalize first letter if not already
    if (formattedMessage.length > 0 && formattedMessage[0] === formattedMessage[0].toLowerCase()) {
      formattedMessage = formattedMessage.charAt(0).toUpperCase() + formattedMessage.slice(1);
    }
    
    // Add period if missing and not already ending with punctuation
    if (formattedMessage.length > 0 && !/[.!?]$/.test(formattedMessage)) {
      formattedMessage += '.';
    }
  }
  
  // Determine error category based on message content
  if (/lexer|token|character|symbol/i.test(error.rawMessage || error.message)) {
    category = 'Lexer Error';
  } else if (/parser|syntax|expected|unexpected/i.test(error.rawMessage || error.message)) {
    category = 'Parser Error';
  } else if (/undefined|not defined|unknown/i.test(error.rawMessage || error.message)) {
    category = 'Reference Error';
  } else if (/duplicate|already defined/i.test(error.rawMessage || error.message)) {
    category = 'Definition Error';
  }
  
  return {
    ...error,
    formattedMessage,
    category,
    severity,
    suggestion: getSuggestionForError(error, category)
  };
}

// Get helpful suggestions based on error type
function getSuggestionForError(error, category) {
  const suggestions = {
    'Syntax Error': 'Check for missing punctuation, unmatched brackets, or typos in keywords.',
    'Reference Error': 'Ensure all referenced types are defined before use, or check for spelling mistakes.',
    'Definition Error': 'Use unique names for each rule or type definition.',
    'Constraint Error': 'Verify that numeric ranges are valid and constraint syntax is correct.',
    'Operator Error': 'Check the CDDL specification for correct operator usage.'
  };
  
  return suggestions[category] || 'Review the CDDL syntax and check for any issues at the indicated position.';
}

let validationTimeout;
let isValidating = false;

// UI Elements (will be initialized when DOM loads)
let statusIndicator;
let validationStatus;
let errorList;

// Validation states
const ValidationState = {
  READY: 'ready',
  CHECKING: 'checking',
  VALID: 'valid',
  INVALID: 'invalid'
};

// Update UI based on validation state
function updateValidationUI(state, message, errors = []) {
  console.log('updateValidationUI called with state:', state, 'message:', message, 'errors:', errors);
  
  // Check if DOM elements exist
  console.log('DOM elements found:', {
    statusIndicator: !!statusIndicator,
    validationStatus: !!validationStatus,
    errorList: !!errorList
  });
  
  if (!statusIndicator || !validationStatus || !errorList) {
    console.error('Missing required DOM elements for validation UI');
    return;
  }
  
  console.log('Updating UI:', state, message, errors);
  
  // Update status indicator
  statusIndicator.className = 'status-indicator';
  
  // Update validation status panel
  validationStatus.className = 'validation-status';
  
  let icon = '';
  let statusMessage = message;
  
  switch (state) {
    case ValidationState.READY:
      statusIndicator.classList.add('ready');
      validationStatus.classList.add('ready');
      icon = '⚪';
      break;
    case ValidationState.CHECKING:
      statusIndicator.classList.add('checking');
      validationStatus.classList.add('checking');
      icon = '🔵';
      statusMessage = 'Validating...';
      break;
    case ValidationState.VALID:
      statusIndicator.classList.add('valid');
      validationStatus.classList.add('valid');
      icon = '✅';
      break;
    case ValidationState.INVALID:
      statusIndicator.classList.add('invalid');
      validationStatus.classList.add('invalid');
      icon = '❌';
      break;
  }
  
  validationStatus.innerHTML = `
    <div class="status-header">
      <span class="status-icon">${icon}</span>
      <span class="status-text">${statusMessage}</span>
    </div>
  `;
  
  // Update error list
  errorList.innerHTML = '';
  if (errors && errors.length > 0) {
    // Group errors by type/message
    const errorGroups = {};
    
    errors.forEach((error, index) => {
      // Extract just the core error message, not the whole formatted text
      let coreMessage = (error.formattedMessage || error.message || '');
      
      // For parser/lexer errors, try to get a cleaner message from rawMessage
      if ((coreMessage.includes('lexer error') || coreMessage.includes('parser error') || coreMessage === 'Lexer error.' || coreMessage === 'Parser error.') && error.rawMessage) {
        // Extract the actual error from the diagnostic output
        const lines = error.rawMessage.split('\n');
        for (let line of lines) {
          line = line.trim();
          // Look for lines that contain the actual error message (start with a caret or contain error descriptions)
          if (line.includes('unterminated') || line.includes('missing closing') || line.includes('unexpected') || line.includes('expected')) {
            coreMessage = line;
            break;
          }
          // Also check for lines that start with a caret (^) as they often contain the error
          if (line.startsWith('^') && line.length > 5) {
            const nextLineIndex = lines.indexOf(line) + 1;
            if (nextLineIndex < lines.length) {
              const nextLine = lines[nextLineIndex].trim();
              if (nextLine && !nextLine.includes('─') && !nextLine.includes('│')) {
                coreMessage = nextLine;
                break;
              }
            }
          }
        }
      }
      
      // If we still have a generic error, try to use the raw message
      if (coreMessage === 'Lexer Error' || coreMessage === 'Parser Error' || coreMessage.length < 10) {
        if (error.rawMessage && error.rawMessage.length > coreMessage.length) {
          coreMessage = error.rawMessage;
        }
      }
      
      // Clean up the message to remove trailing punctuation for grouping
      const groupKey = coreMessage.replace(/[.!?]+$/, '').trim();
      
      if (!errorGroups[groupKey]) {
        errorGroups[groupKey] = [];
      }
      
      errorGroups[groupKey].push({
        line: error.line,
        column: error.column,
        originalIndex: index
      });
    });
    
    // Create grouped error display
    Object.keys(errorGroups).forEach((errorType, groupIndex) => {
      const errorGroup = errorGroups[errorType];
      const errorCount = errorGroup.length;
      
      const errorItem = document.createElement('div');
      errorItem.className = 'error-item';
      
      // Create display text with count
      const displayText = errorCount > 1 ? 
        `${errorType} (${errorCount} instances)` : 
        errorType;
      
      errorItem.innerHTML = `
        <div class="error-header" onclick="toggleErrorDetails(${groupIndex})">
          <div class="error-summary">
            <div class="error-brief">${displayText}</div>
          </div>
          <div class="error-toggle" id="toggle-${groupIndex}">⋯</div>
        </div>
        <div class="error-details" id="details-${groupIndex}">
          <div class="error-content">
            ${errorGroup.map(error => `
              <div class="error-instance">
                <span class="error-location">Line ${error.line}:${error.column}</span>
                <button class="error-action-btn" onclick="jumpToError(${error.line}, ${error.column})">
                  Go to line
                </button>
              </div>
            `).join('')}
          </div>
        </div>
      `;
      errorList.appendChild(errorItem);
    });
    
    // Add error summary at the top
    const errorSummary = document.createElement('div');
    errorSummary.className = 'error-summary';
    errorSummary.innerHTML = `
      <div class="error-count">${errors.length} error${errors.length > 1 ? 's' : ''} found</div>
    `;
    errorList.insertBefore(errorSummary, errorList.firstChild);
  }
}

// Debounced validation function
async function validateCDDL(input, isInitialValidation = false) {
  console.log('validateCDDL called with input length:', input.length, 'isInitial:', isInitialValidation);
  
  if (isValidating && !isInitialValidation) {
    console.log('Already validating, skipping...');
    return;
  }
  
  isValidating = true;
  updateValidationUI(ValidationState.CHECKING, 'Validating CDDL...');
  
  try {
    if (!input.trim()) {
      updateValidationUI(ValidationState.READY, 'Enter CDDL to validate');
      return;
    }
    
    // Validate using the real WASM module
    const result = validateCDDLText(input);
    
    if (result.isValid) {
      updateValidationUI(ValidationState.VALID, 'Valid CDDL');
      
      // Clear any existing error markers
      if (window.editor) {
        monaco.editor.setModelMarkers(window.editor.getModel(), 'cddl', []);
      }
    } else {
      updateValidationUI(ValidationState.INVALID, 'Invalid CDDL', result.errors);
      
      // Show error markers in the editor
      if (window.editor && result.errors.length > 0) {
        const model = window.editor.getModel();
        const fullText = model.getValue();
        
        const markers = result.errors.map(error => {
          console.log('Processing error:', error);
          console.log('Error range:', error.range);
          console.log('Error position line/col:', error.line, error.column);
          console.log('Error message:', error.message);
          console.log('Error rawMessage:', error.rawMessage);
          console.log('Error formattedMessage:', error.formattedMessage);
          
          // Check if this is a delimiter-related error
          const isDelimiterError = (error.message && (
            error.message.includes('closing delimiter') || 
            error.message.includes('closing bracket') ||
            error.message.includes('closing brace') ||
            error.message.includes('closing paren') ||
            error.message.includes('unterminated') ||
            error.message.includes('missing closing')
          )) || (error.rawMessage && (
            error.rawMessage.includes('closing delimiter') || 
            error.rawMessage.includes('closing bracket') ||
            error.rawMessage.includes('closing brace') ||
            error.rawMessage.includes('closing paren') ||
            error.rawMessage.includes('unterminated') ||
            error.rawMessage.includes('missing closing')
          ));
          
          if (isDelimiterError && error.range && error.range.length === 2) {
            const startIndex = error.range[0];
            const endIndex = error.range[1];
            
            console.log(`Delimiter error - Range indices: start=${startIndex}, end=${endIndex}`);
            console.log(`Text at range: "${fullText.substring(startIndex, endIndex)}"`);
            console.log(`Range length: ${endIndex - startIndex}`);
            
            let delimiterIndex = startIndex;
            
            // For unterminated byte string errors, the range should point to the quote
            // but if it's still wrong, search for it
            if (error.message.includes('unterminated')) {
              const charAtStart = fullText.charAt(startIndex);
              console.log(`Character at start index: "${charAtStart}"`);
              
              if (charAtStart !== '\'') {
                // Search backwards to find the quote
                for (let i = startIndex; i >= Math.max(0, startIndex - 10); i--) {
                  const char = fullText.charAt(i);
                  if (char === '\'') {
                    delimiterIndex = i;
                    console.log(`Found opening quote at index ${i} (moved back ${startIndex - i} characters)`);
                    break;
                  }
                }
              } else {
                console.log('Quote already at start index, using as-is');
              }
            } else {
              // For other delimiter errors, look around the start position
              for (let i = Math.max(0, startIndex - 2); i <= Math.min(fullText.length - 1, startIndex + 2); i++) {
                const char = fullText.charAt(i);
                if (char === '[' || char === '{' || char === '(' || char === '\'' || char === '"') {
                  delimiterIndex = i;
                  console.log(`Found delimiter "${char}" at index ${i}`);
                  break;
                }
              }
            }
            
            // Highlight exactly one character - the delimiter
            const delimiterStartPos = model.getPositionAt(delimiterIndex);
            const delimiterEndPos = model.getPositionAt(delimiterIndex + 1);
            
            console.log(`Final highlighting: character "${fullText.charAt(delimiterIndex)}" at positions ${delimiterStartPos.lineNumber}:${delimiterStartPos.column} to ${delimiterEndPos.lineNumber}:${delimiterEndPos.column}`);
            
            return {
              startLineNumber: delimiterStartPos.lineNumber,
              startColumn: delimiterStartPos.column,
              endLineNumber: delimiterEndPos.lineNumber,
              endColumn: delimiterEndPos.column,
              message: error.formattedMessage || error.message,
              severity: monaco.MarkerSeverity.Error,
              code: error.category
            };
          } else if (error.range && error.range.length === 2) {
            // For non-delimiter errors, use the full range
            const startIndex = error.range[0];
            const endIndex = error.range[1];
            
            const startPos = model.getPositionAt(startIndex);
            const endPos = model.getPositionAt(endIndex);
            
            return {
              startLineNumber: startPos.lineNumber,
              startColumn: startPos.column,
              endLineNumber: endPos.lineNumber,
              endColumn: endPos.column,
              message: error.formattedMessage || error.message,
              severity: monaco.MarkerSeverity.Error,
              code: error.category
            };
          } else {
            // Fallback for errors without range info
            // Special handling for unterminated string literals
            if ((error.message && error.message.includes('unterminated') && 
                (error.message.includes('byte string') || error.message.includes('string literal'))) ||
                (error.rawMessage && error.rawMessage.includes('unterminated') && 
                (error.rawMessage.includes('byte string') || error.rawMessage.includes('string literal')))) {
              
              console.log(`String-based unterminated error at line ${error.line}, column ${error.column}`);
              
              // For unterminated strings, the error position usually points after the quote
              // We need to find and highlight just the opening quote
              const lineText = model.getLineContent(error.line);
              console.log(`Line text: "${lineText}"`);
              
              let quoteColumn = error.column;
              
              // Look backwards from the error position to find the quote
              for (let i = error.column - 1; i >= 1; i--) {
                const char = lineText.charAt(i - 1); // Subtract 1 because column is 1-based
                console.log(`Checking column ${i}, character: "${char}"`);
                if (char === '\'') {
                  quoteColumn = i;
                  console.log(`Found opening quote at column ${i}`);
                  break;
                }
              }
              
              return {
                startLineNumber: error.line,
                startColumn: quoteColumn,
                endLineNumber: error.line,
                endColumn: quoteColumn + 1,
                message: error.formattedMessage || error.message,
                severity: monaco.MarkerSeverity.Error,
                code: error.category
              };
            }
            
            // Default fallback for other errors
            const endColumn = Math.min(error.column + 3, 100);
            
            return {
              startLineNumber: error.line,
              startColumn: error.column,
              endLineNumber: error.line,
              endColumn: endColumn,
              message: error.formattedMessage || error.message,
              severity: monaco.MarkerSeverity.Error,
              code: error.category
            };
          }
        });
        
        console.log('Final markers:', markers);
        monaco.editor.setModelMarkers(model, 'cddl', markers);
      }
    }
  } catch (error) {
    console.error('Validation error:', error);
    updateValidationUI(ValidationState.INVALID, 'Validation failed: ' + error.message);
  } finally {
    isValidating = false;
  }
}

// Setup real-time validation with debouncing
function setupRealTimeValidation() {
  console.log('Setting up real-time validation...');
  
  if (!window.editor) {
    console.error('Editor not found for validation setup');
    return;
  }
  
  console.log('Editor found, adding content change listener');
  
  window.editor.onDidChangeModelContent(() => {
    console.log('Content changed, length:', window.editor.getValue().length);
    
    // Clear previous timeout
    if (validationTimeout) {
      clearTimeout(validationTimeout);
    }
    
    // Debounce validation by 300ms
    validationTimeout = setTimeout(() => {
      const content = window.editor.getValue();
      console.log('Running validation for content:', content.substring(0, 50) + '...');
      
      // Save content to cache
      saveCachedContent(content);
      
      // Validate the content
      validateCDDL(content);
    }, 300);
  });
}

// Local storage key for caching CDDL content
const CDDL_CACHE_KEY = 'cddl-validator-content';

// Load cached CDDL content from localStorage
function loadCachedContent() {
  try {
    const cached = localStorage.getItem(CDDL_CACHE_KEY);
    if (cached) {
      console.log('Found cached CDDL content');
      return cached;
    }
  } catch (error) {
    console.warn('Failed to load cached content:', error);
  }
  
  // Return default example if no cache
  return `; Example CDDL schema
person = {
  name: text,
  age: 0..120,
  ? address: text,
  ? email: text
}

people = [* person]`;
}

// Save CDDL content to localStorage
function saveCachedContent(content) {
  try {
    localStorage.setItem(CDDL_CACHE_KEY, content);
    console.log('Saved CDDL content to cache');
  } catch (error) {
    console.warn('Failed to save content to cache:', error);
  }
}

// Initialize everything when DOM is loaded
function initializeApp() {
  console.log('Initializing application...');
  
  // Initialize UI elements (use global variables, not const redeclaration)
  statusIndicator = document.getElementById('statusIndicator');
  validationStatus = document.getElementById('validationStatus');
  errorList = document.getElementById('errorList');
  
  console.log('UI elements found:', {
    statusIndicator: !!statusIndicator,
    validationStatus: !!validationStatus,
    errorList: !!errorList
  });
  
  // Check if the editor container exists
  const editorContainer = document.getElementById('cddlDataDefinition');
  console.log('Editor container found:', !!editorContainer);
  
  if (!editorContainer) {
    console.error('ERROR: Editor container not found!');
    return;
  }
  
  console.log('Creating Monaco editor...');
  
  try {
    // Load cached content or use default example
    const initialContent = loadCachedContent();
    
    // Create Monaco Editor
    window.editor = monaco.editor.create(editorContainer, {
      value: initialContent,
      language: 'cddl',
      theme: 'cddl-theme',
      fontSize: 14,
      wordWrap: 'on',
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      lineNumbers: 'on',
      hover: {
        enabled: true,
        delay: 300,
        sticky: true,
        above: false // Force hover below the line when possible
      },
      suggest: {
        showKeywords: false // Reduce suggestion popup interference
      }
    });
    
    // Add window resize handler to ensure proper layout
    window.addEventListener('resize', () => {
      if (window.editor) {
        window.editor.layout();
      }
    });
    
    console.log('Monaco editor created successfully');
    
    // Initialize WASM and setup validation
    initWasm().then(success => {
      if (success) {
        console.log('WASM loaded successfully, setting up validation');
        setupRealTimeValidation();
        
        // Immediately validate the loaded content (cached or example)
        const content = window.editor.getValue();
        if (content.trim()) {
          console.log('Validating initial content on load');
          validateCDDL(content, true); // Pass true for initial validation
        } else {
          updateValidationUI(ValidationState.READY, 'Ready to validate CDDL');
        }
      } else {
        console.error('WASM failed to load');
        updateValidationUI(ValidationState.INVALID, 'Failed to initialize CDDL parser');
      }
    });
    
  } catch (error) {
    console.error('Error creating Monaco editor:', error);
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeApp);
} else {
  // DOM is already loaded
  initializeApp();
}

// Global utility functions for error handling
window.jumpToError = function(line, column) {
  if (window.editor) {
    window.editor.setPosition({ lineNumber: line, column: column });
    window.editor.focus();
    window.editor.revealLineInCenter(line);
  }
};

// Test function for debugging
window.testWasm = function() {
  console.log('Testing WASM functionality...');
  if (wasmModule && wasmModule.cddl_from_str) {
    try {
      const testCddl = 'person = { name: text, age: uint }';
      const result = wasmModule.cddl_from_str(testCddl);
      console.log('WASM test successful:', result);
      alert('WASM is working correctly!');
    } catch (error) {
      console.error('WASM test failed:', error);
      alert('WASM test failed: ' + error.message);
    }
  } else {
    console.error('WASM module not loaded');
    alert('WASM module not loaded');
  }
};

// Load error examples for testing
window.loadErrorExample = function(type) {
  if (!window.editor) {
    alert('Editor not loaded yet');
    return;
  }
  
  const examples = {
    syntax: `; Example with syntax errors
person = {
  name: text,
  age: uint
  ; Missing comma above
  email text ; Missing colon
}

; Missing equals sign
invalid_rule {
  field: text
}`,
    
    reference: `; Example with reference errors
person = {
  name: text,
  age: uint,
  address: unknown_type ; This type is not defined
}

organization = {
  name: text,
  people: [* person_data] ; This should be 'person'
}`,
    
    duplicate: `; Example with duplicate definitions
person = {
  name: text,
  age: uint
}

; Duplicate definition
person = {
  id: uint,
  name: text
}

; This will also cause an error
person = [text, uint]`
  };
  
  if (examples[type]) {
    window.editor.setValue(examples[type]);
    saveCachedContent(examples[type]);
  }
};

// Function to toggle error details
function toggleErrorDetails(index) {
  const details = document.getElementById(`details-${index}`);
  const toggle = document.getElementById(`toggle-${index}`);
  
  if (details.classList.contains('expanded')) {
    details.classList.remove('expanded');
    toggle.textContent = '⋯';
  } else {
    details.classList.add('expanded');
    toggle.textContent = '×';
  }
}

// Make the function globally available
window.toggleErrorDetails = toggleErrorDetails;
