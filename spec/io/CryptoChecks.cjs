module.exports = () => {
  const C = globalThis.SapiensCrypto;
  const assert = (ok, message) => { if (!ok) throw new Error(message); };
  const base64 = bytes => btoa(String.fromCharCode(...bytes));
  const hex = text => base64(Uint8Array.from(text.match(/../g), byte => parseInt(byte, 16)));
  const fails = action => { try { action(); } catch (_) { return true; } return false; };
  const alice = hex('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a');
  const bob = hex('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb');
  assert(C.publicKey(alice) === hex('8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a'), 'Alice key vector');
  assert(C.publicKey(bob) === hex('de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f'), 'Bob key vector');
  const a = {secretKey: alice, publicKey: C.publicKey(alice)}, b = {secretKey: bob, publicKey: C.publicKey(bob)};
  // NaCl crypto_box known-answer vectors, including a binary plaintext.
  const nonce = hex('69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37');
  const plaintext = hex('be075fc53c81f2d5cf141316ebeb0c7b5228c52a4c62cbd44b66849b64244ffce5ecbaaf33bd751a1ac728d45e6c61296cdc3c01233561f41db66cce314adb310e3be8250c46f06dceea3a7fa1348057e2f6556ad6b1318a024a838f21af1fde048977eb48f59ffd4924ca1c60902e52f0a089bc76897040e082f937763848645e0705');
  const ciphertext = hex('f3ffc7703f9400e52a7dfb4b3d3305d98e993b9f48681273c29650ba32fc76ce48332ea7164d96a4476fb8c531a1186ac0dfc17c98dce87b4da7f011ec48c97271d2c20f9b928fe2270d6fb863d51738b48eeee314a7cc8ab932164548e526ae90224368517acfeabd6bb3732bc0e9da99832b61ca01b6de56244a9e88d5f9b37973f622a43d14a6599b1f654cb45a74e355a5');
  assert(C.perform('open', {secret: bob, peer: a.publicKey, frame: {sender: a.publicKey, nonce, ciphertext}}) === plaintext, 'NaCl ciphertext vector');
  assert(C.box('', nonce, b.publicKey, alice) === hex('2539121d8e234e652d651fa4c8cff880'), 'NaCl empty-message vector');
  for (const message of ['', 'Remote control ✓ 👩🏽‍💻', 'x'.repeat(16384)]) {
    const plain = base64(new TextEncoder().encode(message));
    const frame = C.perform('seal', {secret: a.secretKey, peer: b.publicKey, plaintext: plain});
    assert(C.perform('open', {secret: b.secretKey, peer: a.publicKey, frame}) === plain, 'Host round trip');
    assert(C.unbox(frame.ciphertext, frame.nonce, a.publicKey, b.secretKey) === message, 'Remote client opens host message');
    const cipher = C.box(message, frame.nonce, b.publicKey, a.secretKey);
    assert(cipher === frame.ciphertext, 'Host and client use identical encryption');
    const bytes = Uint8Array.from(atob(cipher), c => c.charCodeAt(0));
    for (const position of new Set([0, 15, bytes.length - 1])) {
      bytes[position] ^= 1;
      const altered = {...frame, ciphertext: base64(bytes)};
      assert(fails(() => C.perform('open', {secret: b.secretKey, peer: a.publicKey, frame: altered})), 'Tampered frame accepted');
      bytes[position] ^= 1;
    }
    for (const altered of [{...frame, sender: b.publicKey}, {...frame, nonce: C.randomBytes(24)}, {...frame, ciphertext: base64(bytes.subarray(0, 15))}]) {
      assert(fails(() => C.perform('open', {secret: b.secretKey, peer: a.publicKey, frame: altered})), 'Invalid frame accepted');
    }
    assert(fails(() => C.perform('open', {secret: C.randomBytes(32), peer: a.publicKey, frame})), 'Wrong secret key accepted');
  }
  assert(fails(() => C.box('work', C.randomBytes(24), base64(new Uint8Array(32)), alice)), 'Low-order peer accepted');
  assert(fails(() => C.box('work', C.randomBytes(23), b.publicKey, alice)), 'Wrong nonce length accepted');
  assert(fails(() => C.perform('seal', {secret: alice, peer: b.publicKey, plaintext: '%%%'})), 'Malformed encoding accepted');
  assert(fails(() => C.perform('unknown', {})), 'Unknown operation accepted');
  const keys = C.keyPair();
  assert(C.publicKey(keys.secretKey) === keys.publicKey, 'Generated public key mismatch');
  assert(C.randomBytes(32) !== C.randomBytes(32), 'Random tokens repeated');
  return true;
};
