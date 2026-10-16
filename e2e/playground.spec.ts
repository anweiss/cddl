import { test, expect, type Page } from '@playwright/test';

function shared(cddl: string, instance = '', kind = 'json') {
  return `/#s1=${Buffer.from(JSON.stringify({ c: cddl, i: instance, k: kind })).toString('base64url')}`;
}
async function ready(page: Page, cddl: string, instance = '') {
  await page.goto(shared(cddl, instance));
  await expect(page.locator('#statusText')).toHaveText('Valid');
}

test('loads the real WASM, validates JSON and rejects negative uint', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', (error) => errors.push(error.message));
  await ready(page, 'root = { count: uint }', '{"count":1}');
  await page.locator('#instanceValidateBtn').click();
  await expect(page.locator('#instanceResult')).toHaveClass(/ok/);
  await page.locator('#instanceEditor .monaco-editor').click();
  await page.keyboard.press('ControlOrMeta+A');
  // Monaco's native EditContext needs key events, not textarea input injection.
  await page.keyboard.type('{"count":-1}');
  await expect(page.locator('#instanceEditor .view-lines')).toContainText('-1');
  await page.locator('#instanceValidateBtn').click();
  await expect(page.locator('#instanceResult')).toHaveClass(/fail/);
  await page.locator('#instanceGenBtn').click();
  await page.locator('#instanceValidateBtn').click();
  await expect(page.locator('#instanceResult')).toHaveClass(/ok/);
  expect(errors).toEqual([]);
});

test('imports a local schema, displays diagnostics and downloads it', async ({ page }) => {
  await ready(page, 'root = uint');
  const chooser = page.waitForEvent('filechooser');
  await page.getByRole('button', { name: 'Open file', exact: true }).click();
  await (
    await chooser
  ).setFiles({ name: 'synthetic.cddl', mimeType: 'text/plain', buffer: Buffer.from('root = {') });
  await expect(page.locator('#problemsBadge')).not.toHaveText('0');
  const download = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Download schema', exact: true }).click();
  expect((await download).suggestedFilename()).toMatch(/\.cddl$/);
});

test('supports keyboard shortcuts and restores focus', async ({ page }) => {
  await ready(page, 'root = uint');
  const button = page.getByRole('button', { name: 'Keyboard shortcuts', exact: true });
  await button.focus();
  await page.keyboard.press('?');
  await expect(page.getByRole('dialog')).toBeVisible();
  await page.keyboard.press('Escape');
  await expect(page.getByRole('dialog')).not.toBeVisible();
  await expect(button).toBeFocused();
});

test('@security hostile schema text stays inert and no data leaves the origin', async ({
  page,
}) => {
  const unexpected: string[] = [];
  const errors: string[] = [];
  await page.route('**/*', async (route) => {
    if (new URL(route.request().url()).origin !== 'http://127.0.0.1:4173') {
      unexpected.push(route.request().url());
      await route.abort();
    } else await route.continue();
  });
  page.on('pageerror', (error) => errors.push(error.message));
  const attack = '<img src=x onerror=window.__injected=1>';
  await ready(page, `root = { ${JSON.stringify(attack)}: tstr }`);
  await expect(page.locator('#outlineList')).toContainText('root');
  expect(await page.locator('img[onerror], script:not([src])').count()).toBe(0);
  expect(await page.evaluate(() => '__injected' in window)).toBe(false);
  expect(unexpected).toEqual([]);
  expect(errors).toEqual([]);
});

test('@security malformed shared fragments cannot crash initialization', async ({ page }) => {
  await page.goto('/#s1=%%%');
  await expect(page.locator('#statusText')).toHaveText('Valid');
  await expect(page.locator('#cddlEditor .monaco-editor')).toBeVisible();
});
