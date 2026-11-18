const fs = require('node:fs');
const sharp = require('../../build/dependencies/node_modules/sharp');

(async () => {
  const [source, destination] = process.argv.slice(2);
  if (!source || !destination) throw new Error('Usage: node svg-to-ico.cjs source.svg destination.ico');
  const sizes = [16, 20, 24, 32, 40, 48, 64, 128, 256];
  const frames = await Promise.all(sizes.map(size => sharp(source, {density: 768}).resize(size, size).png().toBuffer()));
  const header = Buffer.alloc(6 + 16 * sizes.length);
  header.writeUInt16LE(1, 2);
  header.writeUInt16LE(sizes.length, 4);
  let offset = header.length;
  frames.forEach((frame, index) => {
    const entry = 6 + index * 16;
    header[entry] = header[entry + 1] = sizes[index] % 256;
    header.writeUInt16LE(1, entry + 4);
    header.writeUInt16LE(32, entry + 6);
    header.writeUInt32LE(frame.length, entry + 8);
    header.writeUInt32LE(offset, entry + 12);
    offset += frame.length;
  });
  fs.writeFileSync(destination, Buffer.concat([header, ...frames]));
})().catch(error => { console.error(error); process.exitCode = 1; });
