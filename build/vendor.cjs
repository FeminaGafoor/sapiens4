const path = require('node:path');
const root = path.resolve(__dirname, '..');
const libs = path.join(root, 'submodules/libs4');
require(path.join(libs, 'node_modules/esbuild')).buildSync({
  stdin: {contents: "export {default as nacl} from 'tweetnacl'; export {default as jsQR} from 'jsqr'; export {default as QRCode} from 'qrcode';",
    resolveDir: libs},
  bundle:true,platform:'browser',format:'iife',globalName:'RemoteVendor',minify:true,
  legalComments:'eof',outfile:path.join(root,'.build/web/remote-vendor.js'),
});
