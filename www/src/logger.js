const ERROR_TYPES = new Set(['Error', 'TypeError', 'RangeError', 'SyntaxError', 'NetworkError']);
const FAILURE_EVENTS = new Set([
  'application.import_failed',
  'wasm.load_failed',
  'instance.initialization_failed',
  'instance.roots_failed',
  'outline.refresh_failed',
  'outline.initialization_failed',
  'file.initialization_failed',
  'file.read_failed',
  'file.picker_failed',
  'share.initialization_failed',
  'share.read_failed',
  'shortcuts.initialization_failed',
  'toast.initialization_failed',
]);
const STORAGE_EVENTS = new Set([
  'storage.instance_texts',
  'storage.instance_kind',
  'storage.instance_state',
  'storage.instance_pane',
  'storage.outline_pane',
  'storage.outline_preference',
  'storage.reference_check',
  'storage.format_preference',
  'storage.theme_preference',
  'storage.theme_save',
]);

/**
 * Logs contain only fixed event names and error categories. Never pass document
 * contents, URLs (including shared fragments), file names, or exception messages.
 * @param {string} event
 * @param {unknown} error
 */
export function logFailure(event, error) {
  const errorName = error instanceof Error ? error.name : '';
  console.error(
    JSON.stringify({
      timestamp: new Date().toISOString(),
      level: 'error',
      component: 'playground',
      event: FAILURE_EVENTS.has(event) ? event : 'unknown_event',
      errorType: ERROR_TYPES.has(errorName) ? errorName : 'UnknownError',
    }),
  );
}

/** @param {string} event */
export function logStorageUnavailable(event) {
  console.debug(
    JSON.stringify({
      timestamp: new Date().toISOString(),
      level: 'debug',
      component: 'playground',
      event: STORAGE_EVENTS.has(event) ? event : 'storage_unavailable',
    }),
  );
}
