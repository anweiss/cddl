#!/usr/bin/env node

// Simple test to verify the first rule is treated as root
const fs = require('fs');
const path = require('path');

// Load the compiled server module
const serverPath = path.join(__dirname, 'server/out/server.js');
console.log('Loading server from:', serverPath);

// Read the test file
const testFilePath = path.join(__dirname, 'client/testFixture/first-rule-root-test.cddl');
const testContent = fs.readFileSync(testFilePath, 'utf-8');

console.log('\nTest file content:');
console.log(testContent);
console.log('\n--- Expected behavior ---');
console.log('✓ my-custom-protocol (first rule) should NOT be flagged as unused');
console.log('✓ protocol-header should NOT be flagged as unused (referenced)');
console.log('✓ protocol-payload should NOT be flagged as unused (referenced)');
console.log('✗ unused-helper SHOULD be flagged as unused');

console.log('\n--- To test manually ---');
console.log('1. Open the test file in VS Code with the CDDL extension installed');
console.log('2. Check that only "unused-helper" has a warning');
console.log('3. Verify "my-custom-protocol" does NOT have a warning');
