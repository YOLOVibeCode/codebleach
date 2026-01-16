/**
 * JavaScript rule for detecting database names.
 * 
 * This function returns a regex pattern string.
 * The pattern will be used by the sanitizer to find matches.
 * 
 * Advantages of JavaScript:
 * - Can build patterns dynamically
 * - Can include conditional logic
 * - Can combine multiple patterns programmatically
 */

function getPattern() {
    // Define patterns for different database naming conventions
    var patterns = [];
    
    // Pattern 1: Suffix with DB (e.g., PRODDB, SRCDB, DESTDB)
    patterns.push('[A-Z][A-Z0-9]*DB');
    
    // Pattern 2: Prefix with DB (e.g., DB2MEW, DB2BHI)
    patterns.push('DB[0-9]*[A-Z]+');
    
    // Pattern 3: Suffix with _DB (e.g., REPORTING_DB, ANALYTICS_DB)
    patterns.push('[A-Z][A-Z0-9_]*_DB');
    
    // Combine all patterns with word boundaries
    var combined = patterns.map(function(p) {
        return '\\b' + p + '\\b';
    }).join('|');
    
    return '(' + combined + ')';
}

/**
 * Optional: Advanced match function for complex scenarios.
 * This can be called directly by the sanitizer for custom matching logic.
 * 
 * @param {string} content - The content to search
 * @returns {string[]} - Array of matched values
 */
function match(content) {
    var results = [];
    var pattern = getPattern();
    var regex = new RegExp(pattern, 'g');
    var match;
    
    while ((match = regex.exec(content)) !== null) {
        // Skip if it looks like a column name (followed by period and lowercase)
        var nextChar = content[match.index + match[0].length];
        if (nextChar === '.') {
            // This is a schema qualifier, include it
            results.push(match[0]);
        } else {
            // Standalone database name
            results.push(match[0]);
        }
    }
    
    // Remove duplicates
    return results.filter(function(value, index, self) {
        return self.indexOf(value) === index;
    });
}

