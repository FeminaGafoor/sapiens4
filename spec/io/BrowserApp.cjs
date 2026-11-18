const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');

exports.observe = async (evaluate, command, sessionId, origin) => {
  const root = process.cwd();
  const esbuild = require(path.join(root, 'build/dependencies/node_modules/esbuild'));
  const bundle = esbuild.buildSync({stdin: {contents:
    `export * as Audio from './.build/web/modules/Sapiens.BrowserAudio/index.js';
     export * as Native from './.build/web/modules/Sapiens.BrowserNative/index.js';`, resolveDir: root},
    bundle: true, format: 'iife', globalName: 'AppObservation', write: false}).outputFiles[0].text;
  await evaluate(bundle);
  await evaluate(fs.readFileSync(path.join(root, '.build/web/preferences.js'), 'utf8'));
  const observations = await evaluate(`(async () => {
    const N=AppObservation.Native,A=AppObservation.Audio;
    const errorOf=action=>{try{action();return '';}catch(error){return error.message;}};
    const malformedCrypto={unboxNull:SapiensCrypto.unbox(null,'','',''),randomFraction:errorOf(()=>SapiensCrypto.randomBytes(1.5)),missingPlaintext:errorOf(()=>SapiensCrypto.perform('seal',{peer:'',secret:''}))};
    const markup=N.parseMarkup('<p id="one">hello <strong>café</strong><!--ignore--></p>');
    let prompted=0;
    const stopInstall=N.subscribe('window')('beforeinstallprompt')(()=>()=>{})();
    const promptEvent=new Event('beforeinstallprompt',{cancelable:true});promptEvent.prompt=async()=>{prompted++;};
    dispatchEvent(promptEvent);await N.installPrompt();await N.installPrompt();stopInstall();
    const install={prevented:promptEvent.defaultPrevented,prompted};
    const originalRegistration=navigator.serviceWorker.getRegistration;
    let added=0,removedListeners=0,workerFailure='';
    const update=async state=>{
      const worker=new EventTarget();worker.state='installing';
      const add=worker.addEventListener.bind(worker),remove=worker.removeEventListener.bind(worker);
      worker.addEventListener=(...args)=>{added++;add(...args);};worker.removeEventListener=(...args)=>{removedListeners++;remove(...args);};
      navigator.serviceWorker.getRegistration=async()=>({installing:worker,update:async()=>{setTimeout(()=>{worker.state=state;worker.dispatchEvent(new Event('statechange'));},0);}});
      await N.updateWorker();
    };
    try{await update('activated');try{await update('redundant');}catch(error){workerFailure=error.message;}navigator.serviceWorker.getRegistration=async()=>null;await N.updateWorker();}
    finally{navigator.serviceWorker.getRegistration=originalRegistration;}
    const updating={failure:workerFailure,added,removed:removedListeners};
    const qr=await N.qrImage('sapiens-purescript')();
    const decodedQR=await N.decodeQR(qr)(1600)();
    const cache='sapiens-migration-'+crypto.randomUUID();
    await N.cacheAll(cache)(['/favicon.svg'])();
    const cached=await N.cacheResponse(cache)(location.origin+'/favicon.svg?migration=1')();
    const fallback=await N.cacheResponse(cache)(location.origin+'/manifest.webmanifest')();
    const caching={cachedStatus:cached.status,cachedSVG:(await cached.text()).includes('<svg'),fallbackStatus:fallback.status};
    await N.cacheDelete(cache)();
    const database='sapiens-migration-'+crypto.randomUUID();
    const access=(operation,key,value)=>N.database(database)(1)(['records'])(operation)('records')(key)(value)();
    await access('put','entry',{text:'café 🧬'});
    const saved=await access('get','entry',null), all=await access('getAll','',null);
    await access('delete','entry',null);
    const removed=await access('get','entry',null);
    let invalidStorage=false;try{await access('invalid','',null);}catch{invalidStorage=true;}
    indexedDB.deleteDatabase(database);
    const notifications=[];
    window.webkit={messageHandlers:{browser:{postMessage:value=>notifications.push(value)}}};
    localStorage.clear();
    localStorage.setItem('s4.native.storage','old-root');
    localStorage.setItem('s4.workspace.old','private');
    localStorage.setItem('s4.snapshot.old','private');
    localStorage.setItem('unrelated','keep');
    window.sapiensPreferences([location.origin,'new-root',{'sapi-color-theme':'dark','s4.active':'owner','forbidden':'drop'},'restore-once']);
    const restored={theme:localStorage.getItem('sapi-color-theme'),active:localStorage.getItem('s4.active'),oldWorkspace:localStorage.getItem('s4.workspace.old'),oldSnapshot:localStorage.getItem('s4.snapshot.old'),unrelated:localStorage.getItem('unrelated'),forbidden:localStorage.getItem('forbidden')};
    localStorage.setItem('sapi-color-theme','light');
    window.sapiensPreferences([location.origin,'new-root',{'sapi-color-theme':'dark'},'restore-once']);
    const restoredOnce=localStorage.getItem('sapi-color-theme')==='light';
    const beforeForeign=notifications.length;
    window.sapiensPreferences(['https://untrusted.invalid','foreign',{'sapi-color-theme':'dark'},'foreign']);
    const rejectedOrigin=notifications.length===beforeForeign&&localStorage.getItem('s4.native.storage')==='new-root';
    localStorage.setItem('forbidden','still-local');
    const preferences={...restored,restoredOnce,rejectedOrigin,onlyAllowed:notifications.every(value=>['sapi-color-theme','s4.active'].includes(value.clientPreferences.key)||value.clientPreferences.key.startsWith('s4.workspace.')),publishedLight:notifications.some(value=>value.clientPreferences.key==='sapi-color-theme'&&value.clientPreferences.value==='light')};
    localStorage.setItem('s4.workspace.stale','old');
    window.sapiensPreferences([location.origin,'new-root',null,'restore-snapshot']);
    const keptWithoutSnapshot=localStorage.getItem('s4.workspace.stale')==='old';
    window.sapiensPreferences([location.origin,'new-root',{'sapi-color-theme':'dark','s4.workspace.saved':'draft'},'restore-snapshot']);
    const removedStale=localStorage.getItem('s4.workspace.stale')===null&&localStorage.getItem('s4.active')===null;
    dispatchEvent(new CustomEvent('sapiens-preferences-flush',{detail:'flush-nonce'}));
    const inert=document.documentElement.inert;
    const snapshot=notifications.findLast(value=>value.preferencesFlush)?.preferencesFlush;
    dispatchEvent(new Event('sapiens-preferences-resume'));
    const flush={keptWithoutSnapshot,removedStale,inert,resumed:!document.documentElement.inert,snapshot};
    const originals={context:window.AudioContext,node:window.AudioWorkletNode,microphone:navigator.mediaDevices.getUserMedia};
    let node,stopped=0,closed=0;
    navigator.mediaDevices.getUserMedia=async()=>({getTracks:()=>[{stop:()=>stopped++}]});
    window.AudioContext=class {constructor(){this.sampleRate=48000;this.audioWorklet={addModule:async()=>{}};}createMediaStreamSource(){return {connect(){},disconnect(){}};}async close(){closed++;}};
    window.AudioWorkletNode=class {constructor(){node=this;this.port={onmessage:null};}connect(){}disconnect(){}};
    let audio;
    try {
      const pieces=[];
      const voice=await A.voiceStart('/voice-worklet.js')(1)(piece=>()=>pieces.push(piece))();
      const input=new Float32Array(48000).fill(0.5);input[0]=0;input[1]=1;input[2]=2;
      for(const [start,end] of [[0,1],[1,2],[2,127],[127,8191],[8191,48000]])node.port.onmessage({data:input.slice(start,end)});
      const first=voice.stop(),second=voice.stop(),result=await first;
      const decode=piece=>new DataView(Uint8Array.from(atob(piece.data),c=>c.charCodeAt(0)).buffer);
      const wave=decode(pieces[0]),tail=decode(pieces[1]);
      const clipping=A.allocate(5)();[-2,-1,0,1,2].forEach((value,index)=>A.writeSample(clipping)(index)(value)());
      const clip=new DataView(Uint8Array.from(atob(A.wav(clipping)()),c=>c.charCodeAt(0)).buffer);
      audio={pieces:pieces.length,firstFinal:pieces[0].final,lastFinal:pieces[1].final,firstIndex:pieces[0].index,lastIndex:pieces[1].index,firstStart:pieces[0].startMs,lastStart:pieces[1].startMs,duration:result.durationMs,segments:result.segments,rate:wave.getUint32(24,true),dataLength:wave.getUint32(40,true),firstSample:wave.getInt16(44,true),secondSample:wave.getInt16(46,true),tailLength:tail.getUint32(40,true),tailSample:tail.getInt16(44,true),sameStop:first===second,stopped,closed,clipping:Array.from({length:5},(_,index)=>clip.getInt16(44+index*2,true))};
    } finally {
      window.AudioContext=originals.context;window.AudioWorkletNode=originals.node;navigator.mediaDevices.getUserMedia=originals.microphone;
    }
    return {storage:{saved,all,removed,invalidStorage},preferences,flush,audio,decodedQR,caching,markup,install,updating,malformedCrypto};
  })()`);
  await command('Page.navigate', {url: pathToFileURL(path.join(root, 'app/shell/bootstrap.html')).href}, sessionId);
  const deadline = Date.now()+15000;
  while (!await evaluate("typeof window.setTermsState==='function'")) {
    if (Date.now()>deadline) throw new Error('Compiled setup script did not load');
    await new Promise(resolve=>setTimeout(resolve,50));
  }
  observations.setup = await evaluate(`(() => {
    window.setupMessages=[];window.webkit={messageHandlers:{browser:{postMessage:value=>window.setupMessages.push(value)}}};
    window.showStorageSetup();window.setTermsState(false,'terms-v1');
    const button=document.getElementById('get-started'),terms=document.getElementById('terms-consent');
    const initialBlocked=button.disabled;
    terms.checked=true;terms.dispatchEvent(new Event('change'));
    const consentEnables=!button.disabled;
    document.getElementById('storage-pick').click();
    const pickingBlocks=button.disabled;
    window.storagePicked(['C:','Users','Ada','Sapiens'].join(String.fromCharCode(92)));
    const directoryLabel=document.getElementById('storage-directory').textContent;
    button.click();button.click();
    const submitted=setupMessages.filter(value=>value.storage==='existing');
    window.storageError('Try another folder');
    const recovered=!button.disabled&&document.getElementById('error').textContent==='Try another folder';
    terms.checked=false;terms.dispatchEvent(new Event('change'));button.click();
    const consentBlocks=button.disabled&&setupMessages.filter(value=>value.storage==='existing').length===submitted.length;
    document.getElementById('privacy-link').click();
    return {initialBlocked,consentEnables,pickingBlocks,directoryLabel,submitted:submitted.length,selected:submitted[0],recovered,consentBlocks,legal:setupMessages.at(-1),visible:!document.querySelector('main').hidden};
  })()`);
  return observations;
};
