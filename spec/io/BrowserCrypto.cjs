const {spawn} = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {once} = require('node:events');
const {createInterface} = require('node:readline');

const candidates = [process.env.SAPIENS_TEST_BROWSER,
  ...(process.platform === 'darwin' ? ['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge'] :
    process.platform === 'win32' ? [path.join(process.env.PROGRAMFILES || '', 'Google/Chrome/Application/chrome.exe'), path.join(process.env['PROGRAMFILES(X86)'] || '', 'Microsoft/Edge/Application/msedge.exe')] :
    ['/usr/bin/chromium', '/usr/bin/chromium-browser', '/usr/bin/google-chrome'])];
const executable = candidates.find(file => file && fs.existsSync(file));
if (!executable) throw new Error('Install Chrome/Chromium/Edge or set SAPIENS_TEST_BROWSER');
const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'sapiens-browser-'));
const browser = spawn(executable, ['--headless=new', '--remote-debugging-pipe', `--user-data-dir=${profile}`,
  '--no-first-run', '--no-default-browser-check', '--disable-background-networking', '--disable-extensions', 'about:blank'],
  {detached: process.platform !== 'win32', stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe']});
let serial = 0, incoming = Buffer.alloc(0), errors = '', stopping = false;
const pending = new Map();
browser.stderr.on('data', data => { errors = (errors + data).slice(-4000); });
browser.on('error', error => { for (const item of pending.values()) item.reject(error); });
browser.on('exit', code => {
  for (const item of pending.values()) item.reject(new Error(`Browser exited (${code}): ${errors}`));
});
browser.stdio[4].on('data', data => {
  incoming = Buffer.concat([incoming, data]);
  for (let end; (end = incoming.indexOf(0)) >= 0;) {
    const raw = incoming.subarray(0, end).toString(); incoming = incoming.subarray(end + 1);
    if (!raw) continue;
    const message = JSON.parse(raw), item = pending.get(message.id);
    if (!item) continue;
    pending.delete(message.id); clearTimeout(item.timer);
    message.error ? item.reject(new Error(message.error.message)) : item.resolve(message.result);
  }
});
function command(method, params = {}, sessionId) {
  return new Promise((resolve, reject) => {
    const id = ++serial;
    const timer = setTimeout(() => { pending.delete(id); reject(new Error(`Browser timed out: ${method}`)); }, 25000);
    pending.set(id, {resolve, reject, timer});
    browser.stdio[3].write(JSON.stringify({id, method, params, ...(sessionId ? {sessionId} : {})}) + '\0');
  });
}
async function close() {
  if (stopping) return;
  stopping = true;
  process.stdin.pause();
  process.stdin.destroy();
  if (browser.exitCode === null && browser.signalCode === null) {
    const exited = once(browser, 'exit');
    const deadline = setTimeout(() => browser.kill('SIGKILL'), 5000);
    command('Browser.close').catch(() => browser.kill());
    await exited;
    clearTimeout(deadline);
  }
  for (const item of pending.values()) clearTimeout(item.timer);
  pending.clear();
  for (const stream of browser.stdio) stream?.destroy();
  if (process.platform !== 'win32') {
    try { process.kill(-browser.pid, 'SIGKILL'); } catch (error) { if (error.code !== 'ESRCH') throw error; }
  }
  fs.rmSync(profile, {recursive: true, force: true, maxRetries: 10, retryDelay: 100});
}
process.on('SIGTERM', () => close().finally(() => process.exit(0)));
process.on('SIGINT', () => close().finally(() => process.exit(0)));

(async () => {
  const origin = new URL(process.argv[2]).origin;
  const {targetId} = await command('Target.createTarget', {url: origin + '/workspace/'});
  const {sessionId} = await command('Target.attachToTarget', {targetId, flatten: true});
  async function evaluate(expression) {
    const reply = await command('Runtime.evaluate', {expression, awaitPromise: true, returnByValue: true}, sessionId);
    if (reply.exceptionDetails) throw new Error(reply.exceptionDetails.exception?.description || reply.exceptionDetails.text);
    return reply.result.value;
  }
  const deadline = Date.now() + 20000;
  while (true) {
    if (await evaluate(`typeof SapiensCrypto !== 'undefined' && fetch('/api/host/crypto').then(r=>r.json()).then(r=>r.connected)`)) break;
    if (Date.now() > deadline) throw new Error('Browser crypto did not connect');
    await new Promise(resolve => setTimeout(resolve, 50));
  }

  process.stdout.write('ready\n');
  for await (const line of createInterface({input: process.stdin})) {
    if (!line.trim()) break;
    const input = JSON.parse(line);
    if (input.operation === 'application') {
      const result = await require('./BrowserApp.cjs').observe(evaluate, command, sessionId, origin);
      process.stdout.write(JSON.stringify(result) + '\n');
      continue;
    }
    const result = await evaluate(`(input => {
      const C = SapiensCrypto;
      if (input.operation === 'crypto') {
        const [data, index] = input.args;
        const bytes = text => Uint8Array.from(atob(text), c => c.charCodeAt(0));
        const base64 = value => btoa(String.fromCharCode(...value));
        try {
          let value;
          switch (input.method) {
            case 'hex': value = base64(Uint8Array.from(data.match(/../g), byte => parseInt(byte,16))); break;
            case 'encode': value = base64(new TextEncoder().encode(data)); break;
            case 'byteLength': value = bytes(data).length; break;
            case 'flipByte': { const changed = bytes(data); changed[index] ^= 1; value = base64(changed); break; }
            case 'truncate': value = base64(bytes(data).subarray(0,index)); break;
            case 'zeros': value = base64(new Uint8Array(data)); break;
            default: value = C[input.method](...input.args);
          }
          return {ok: true, value};
        } catch (error) { return {ok: false, error: String(error)}; }
      }
      if (input.operation === 'request') {
        const config = input.config, nonce = C.randomBytes(24);
        const body = {...input.body, v: 'sapiens-remote-box-v1', room: config.room,
          direction: 'phone-to-desktop', id: crypto.randomUUID(), expires: Date.now() / 1000 + 300};
        return {sender: config.public, nonce,
          ciphertext: C.box(JSON.stringify(body), nonce, config.desktop, config.private)};
      }
      if (input.operation === 'response') {
        const {config, frame} = input;
        return JSON.parse(C.unbox(frame.ciphertext, frame.nonce, config.desktop, config.private));
      }
      throw new Error('Unknown fixture operation');
    })(${JSON.stringify(input)})`);
    process.stdout.write(JSON.stringify(result) + '\n');
  }
})().catch(error => { process.stderr.write(String(error.stack || error) + '\n'); process.exitCode = 1; })
  .finally(close);
