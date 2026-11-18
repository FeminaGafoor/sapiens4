(() => {
  const nacl = globalThis.RemoteVendor.nacl;
  const maxPlaintext = 16000000;
  const encode = bytes => {
    let text = '';
    for (let i = 0; i < bytes.length; i += 32768) text += String.fromCharCode(...bytes.subarray(i, i + 32768));
    return btoa(text);
  };
  const decode = (value, size) => {
    if (typeof value !== 'string' || value.length > 22000000) throw new Error('Invalid encoded value');
    const bytes = Uint8Array.from(atob(value), c => c.charCodeAt(0));
    if ((size !== undefined && bytes.length !== size) || encode(bytes) !== value) throw new Error('Invalid encoded value');
    return bytes;
  };
  const keyPair = () => {
    const pair = nacl.box.keyPair();
    return {publicKey: encode(pair.publicKey), secretKey: encode(pair.secretKey)};
  };
  const publicKey = secret => encode(nacl.box.keyPair.fromSecretKey(decode(secret, 32)).publicKey);
  const randomBytes = size => {
    if (!Number.isInteger(size) || size < 0 || size > 65536) throw new Error('Invalid random length');
    return encode(nacl.randomBytes(size));
  };
  const keys = (peer, secret) => {
    const pk = decode(peer, 32), sk = decode(secret, 32);
    // TweetNaCl accepts low-order points; reject their all-zero shared secret.
    const shared = nacl.scalarMult(sk, pk);
    const invalid = shared.every(byte => byte === 0);
    shared.fill(0);
    if (invalid) { sk.fill(0); throw new Error('Invalid peer key'); }
    return {pk, sk};
  };
  const encrypt = (message, nonce, peer, secret) => {
    if (message.length > maxPlaintext) throw new Error('Remote message is too large');
    const n = decode(nonce, 24), {pk, sk} = keys(peer, secret);
    try { return encode(nacl.box(message, n, pk, sk)); }
    finally { sk.fill(0); }
  };
  const decrypt = (ciphertext, nonce, peer, secret) => {
    const cipher = decode(ciphertext), n = decode(nonce, 24);
    if (cipher.length < 16 || cipher.length > maxPlaintext + 16) throw new Error('Invalid encrypted message');
    const {pk, sk} = keys(peer, secret);
    try {
      const plain = nacl.box.open(cipher, n, pk, sk);
      if (plain === null) throw new Error('Invalid encrypted message');
      return plain;
    } finally { sk.fill(0); }
  };
  const box = (text, nonce, peer, secret) => encrypt(new TextEncoder().encode(text), nonce, peer, secret);
  const unbox = (ciphertext, nonce, peer, secret) => {
    try { return new TextDecoder('utf-8', {fatal: true}).decode(decrypt(ciphertext, nonce, peer, secret)); }
    catch (_) { return ''; }
  };
  const perform = (operation, input) => {
    switch (operation) {
      case 'keypair': return keyPair();
      case 'random': return randomBytes(input.size);
      case 'seal': {
        const nonce = randomBytes(24);
        return {sender: publicKey(input.secret), nonce,
          ciphertext: encrypt(decode(input.plaintext), nonce, input.peer, input.secret)};
      }
      case 'open': {
        if (input.frame.sender !== input.peer) throw new Error('Unexpected sender');
        return encode(decrypt(input.frame.ciphertext, input.frame.nonce, input.peer, input.secret));
      }
      default: throw new Error('Unsupported crypto operation');
    }
  };

  let socket, reconnect, started = false, paused = false;
  const connect = () => {
    if (paused || (socket && socket.readyState < WebSocket.CLOSING)) return;
    const url = new URL('/api/host/crypto', location.origin);
    url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const current = socket = new WebSocket(url);
    let ready;
    const connected = new Promise(resolve => { ready = resolve; });
    current.onopen = () => ready();
    current.onmessage = event => {
      let job;
      try {
        if (typeof event.data !== 'string' || event.data.length > 24000000) throw new Error('Invalid crypto request');
        job = JSON.parse(event.data);
        current.send(JSON.stringify({id: job.id, ok: true, result: perform(job.operation, job.input)}));
      } catch (_) {
        if (job && current.readyState === WebSocket.OPEN) current.send(JSON.stringify({id: job.id, ok: false}));
        else current.close();
      }
    };
    current.onclose = () => {
      ready();
      if (socket === current) socket = undefined;
      if (!paused) reconnect = setTimeout(connect, 1000);
    };
    current.onerror = () => current.close();
    return connected;
  };
  const startHost = async () => {
    if (started) return;
    if (location.protocol !== 'http:' || !['127.0.0.1', 'localhost'].includes(location.hostname)) return;
    // Relay pages serve the same app but have no local execution endpoint.
    try {
      const response = await fetch('/api/host/crypto', {cache: 'no-store'});
      if (!response.ok || (await response.json()).browser_crypto !== true) return;
    } catch (_) { return; }
    started = true;
    addEventListener('pagehide', () => { paused = true; clearTimeout(reconnect); socket?.close(); });
    addEventListener('pageshow', () => { paused = false; connect(); });
    await connect();
  };
  globalThis.SapiensCrypto = Object.freeze({keyPair, publicKey, randomBytes, box, unbox, perform, startHost});
})();
