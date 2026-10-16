// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import { parseRules, renderOutline, registerNavigation } from '../src/outline.js';
import { initToasts, toast } from '../src/toast.js';
import { initShortcutsModal } from '../src/shortcuts.js';
import {
  classifyFile,
  bytesToHex,
  pickFile,
  downloadText,
  attachDropZone,
} from '../src/file-io.js';
import { logFailure, logStorageUnavailable } from '../src/logger.js';

afterEach(() => {
  initToasts(null);
  vi.useRealTimers();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

describe('outline and navigation', () => {
  it('renders text safely, filters rules and selects by keyboard', () => {
    const list = document.createElement('div');
    document.body.append(list);
    const select = vi.fn();
    const rules = parseRules('; documentation\nfirst = uint\nsecond = { value: tstr }');
    renderOutline(list, rules, { onSelect: select, activeName: 'first' });
    const items = list.querySelectorAll('[role="treeitem"]');
    expect(items).toHaveLength(2);
    items[0].dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }));
    expect(document.activeElement).toBe(items[1]);
    items[1].dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
    expect(select).toHaveBeenCalledWith(rules[1]);
    items[0].click();
    expect(select).toHaveBeenCalledWith(rules[0]);
    renderOutline(list, rules, { filter: 'SECOND' });
    expect(list.textContent).toContain('second');
    expect(list.textContent).not.toContain('first');
    renderOutline(list, rules, { filter: 'missing' });
    expect(list.textContent).toContain('No matching rules');
    expect(list.querySelector('script')).toBeNull();
  });
  it('resolves dotted identifiers and disposes both Monaco providers', () => {
    const providers = {};
    const dispose = vi.fn();
    const monaco = {
      languages: {
        registerDefinitionProvider: (_, provider) => {
          providers.definition = provider;
          return { dispose };
        },
        registerHoverProvider: (_, provider) => {
          providers.hover = provider;
          return { dispose };
        },
      },
    };
    const stop = registerNavigation(monaco, () => '; note\nmy.rule = uint\nroot = my.rule');
    const model = {
      uri: 'test://schema',
      getLineContent: () => 'root = my.rule',
      getWordAtPosition: () => null,
    };
    const position = { lineNumber: 3, column: 11 };
    expect(providers.definition.provideDefinition(model, position).range.startLineNumber).toBe(2);
    expect(providers.hover.provideHover(model, position).contents[0].value).toContain('note');
    expect(
      providers.definition.provideDefinition(
        { ...model, getLineContent: () => 'missing' },
        { lineNumber: 1, column: 2 },
      ),
    ).toBeNull();
    stop();
    expect(dispose).toHaveBeenCalledTimes(2);
  });
});

describe('accessible transient UI', () => {
  it('deduplicates toasts, expires them and caps the visible queue', () => {
    vi.useFakeTimers();
    const container = document.createElement('div');
    document.body.append(container);
    initToasts(container);
    const dismiss = toast('<script>not markup</script>', 'error', 100);
    expect(toast('<script>not markup</script>', 'error', 100)).toBe(dismiss);
    expect(container.querySelectorAll('.toast')).toHaveLength(1);
    expect(container.querySelector('[role="alert"]')).not.toBeNull();
    expect(container.querySelector('script')).toBeNull();
    vi.advanceTimersByTime(400);
    expect(container.children).toHaveLength(0);
    for (let i = 0; i < 6; i++) toast(String(i), 'info', 0);
    vi.advanceTimersByTime(300);
    expect(container.children).toHaveLength(4);
    container.querySelector('button').click();
    vi.advanceTimersByTime(300);
    expect(container.children).toHaveLength(3);
  });
  it('opens shortcuts, traps focus, closes and removes listeners', () => {
    document.body.innerHTML =
      '<button id="open">Help</button><div id="modal"><button id="close">Close</button><div id="body"></div></div>';
    const btn = document.querySelector('#open');
    const modal = document.querySelector('#modal');
    const closeBtn = document.querySelector('#close');
    const bodyEl = document.querySelector('#body');
    const controller = initShortcutsModal({ btn, modal, closeBtn, bodyEl });
    btn.focus();
    btn.click();
    expect(modal.getAttribute('aria-hidden')).toBe('false');
    expect(bodyEl.textContent).toContain('Validate instance');
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab', cancelable: true }));
    expect(document.activeElement).toBe(closeBtn);
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    expect(document.activeElement).toBe(btn);
    expect(modal.getAttribute('aria-hidden')).toBe('true');
    document.dispatchEvent(new KeyboardEvent('keydown', { key: '?' }));
    expect(modal.getAttribute('aria-hidden')).toBe('false');
    controller.close();
    controller.toggle();
    closeBtn.click();
    controller.dispose();
    btn.click();
    expect(modal.getAttribute('aria-hidden')).toBe('true');
  });
});

describe('file operations and privacy', () => {
  it.each([
    ['a.CDDL', 'cddl'],
    ['a.json', 'json'],
    ['a.cbor', 'cbor'],
    ['a.txt', 'unknown'],
  ])('classifies %s', (name, kind) => {
    expect(classifyFile(name)).toBe(kind);
  });
  it('formats CBOR bytes and handles picker cancellation', async () => {
    expect(bytesToHex(new Uint8Array([0, 15, 255]))).toBe('000fff');
    const click = vi.spyOn(HTMLInputElement.prototype, 'click').mockImplementation(function () {
      this.dispatchEvent(new Event('cancel'));
    });
    expect(await pickFile()).toBeNull();
    expect(click).toHaveBeenCalledTimes(1);
  });
  it('reads a chosen file, cleans up download URLs and sanitizes filenames', async () => {
    vi.spyOn(HTMLInputElement.prototype, 'click').mockImplementation(function () {
      Object.defineProperty(this, 'files', { value: [new File(['root = uint'], 'schema.cddl')] });
      this.dispatchEvent(new Event('change'));
    });
    expect(await pickFile()).toMatchObject({ kind: 'cddl', text: 'root = uint', bytes: null });
    vi.useFakeTimers();
    const revokeObjectURL = vi.fn();
    vi.stubGlobal('URL', { createObjectURL: vi.fn(() => 'blob:test'), revokeObjectURL });
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(function () {
      expect(this.download).toBe('..schemacddl');
    });
    downloadText('../schema\\cddl', 'root = uint');
    expect(click).toHaveBeenCalledOnce();
    vi.runAllTimers();
    expect(revokeObjectURL).toHaveBeenCalledWith('blob:test');
  });
  it('registers drop handlers and removes them on disposal', () => {
    const target = document.createElement('div');
    const overlay = document.createElement('div');
    const remove = vi.spyOn(target, 'removeEventListener');
    const stop = attachDropZone(target, overlay, vi.fn());
    target.dispatchEvent(new Event('dragend'));
    stop();
    expect(remove.mock.calls.length).toBeGreaterThanOrEqual(4);
  });
  it('never logs document content, file names, stack traces or shared URLs', () => {
    const error = vi.spyOn(console, 'error').mockImplementation(() => {});
    const debug = vi.spyOn(console, 'debug').mockImplementation(() => {});
    const secret = 'synthetic-private-document https://example.test/#secret';
    logFailure('file.read_failed', new TypeError(secret));
    logFailure(secret, { message: secret });
    logStorageUnavailable('storage.instance_texts');
    logFailure('synthetic_private_identifier', new Error(secret));
    logStorageUnavailable('synthetic_private_identifier');
    const rows = [...error.mock.calls, ...debug.mock.calls].map(([row]) => row);
    expect(rows.join('')).not.toContain(secret);
    expect(JSON.parse(rows[0])).toMatchObject({
      level: 'error',
      errorType: 'TypeError',
      component: 'playground',
    });
    expect(JSON.parse(rows[1]).event).toBe('unknown_event');
    expect(rows.join('')).not.toContain('synthetic_private_identifier');
  });
});
