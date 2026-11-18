export const environment = () => ({origin:location.origin,href:location.href,path:location.pathname,hash:location.hash,search:location.search,contentType:document.contentType,body:{...document.body?.dataset},userAgent:navigator.userAgent,platform:navigator.platform,touchPoints:navigator.maxTouchPoints,installed:navigator.standalone===true||matchMedia('(display-mode: standalone)').matches,dark:matchMedia('(prefers-color-scheme: dark)').matches,touch:matchMedia('(pointer: coarse)').matches,width:innerWidth,height:innerHeight,orientation:screen.orientation?.type||'',viewportHeight:visualViewport?.height||innerHeight,viewportScale:visualViewport?.scale||1,hidden:document.hidden,secure:isSecureContext,native:!!(window.webkit?.messageHandlers?.browser||window.chrome?.webview),online:navigator.onLine});
export const now = () => Date.now();
export const uuid = () => crypto.randomUUID();
export const request = url => method => body => headers => async () => {const r=await fetch(url,{method,headers,body:['GET','HEAD'].includes(method)?undefined:body,signal:AbortSignal.timeout(20000),redirect:'error',cache:'no-store'});return {status:r.status,mime:r.headers.get('Content-Type')||'',text:await r.text()};};
export const storageRead = key => () => {try{return localStorage.getItem(key)||'';}catch{return '';}};
export const storageWrite = key => value => () => {try{localStorage.setItem(key,value);return true;}catch{return false;}};
export const storageRemove = key => () => localStorage.removeItem(key);
export const storageKeys = () => Object.keys(localStorage);
export const database = name => version => stores => operation => store => key => value => () => new Promise((resolve,reject)=>{let db,finished=false;const finish=(error,result)=>{if(finished)return;finished=true;clearTimeout(timer);db?.close();error?reject(error):resolve(result??null);};const timer=setTimeout(()=>finish(new Error('Device storage is unavailable.')),3000);const opening=indexedDB.open(name,version);opening.onupgradeneeded=()=>{for(const s of stores)if(!opening.result.objectStoreNames.contains(s))opening.result.createObjectStore(s);};opening.onerror=()=>finish(opening.error);opening.onblocked=()=>finish(new Error('Close other tabs and try again.'));opening.onsuccess=()=>{db=opening.result;if(finished){db.close();return;}const tx=db.transaction(store,['get','getAll'].includes(operation)?'readonly':'readwrite'),s=tx.objectStore(store);let result;const r=operation==='getAll'?s.getAll():operation==='get'?s.get(key):operation==='put'?s.put(value,key):s.delete(key);r.onsuccess=()=>{result=r.result;};tx.oncomplete=()=>finish(null,result);tx.onabort=()=>finish(tx.error||new Error('Device storage failed.'));};});
export const openSocket = url => opened => message => closed => () => {const s=new WebSocket(url);s.onopen=()=>opened();s.onmessage=e=>message(typeof e.data==='string'?e.data:'')();s.onclose=e=>closed(e.code)();s.onerror=()=>s.close();return s;};
export const socketSend = socket => value => () => socket.send(value);
export const socketClose = socket => () => socket.close();
const eventValue=e=>({type:e.type,message:e.message||e.reason?.message||'',key:e.key||'',code:e.code||'',shift:!!e.shiftKey,meta:!!e.metaKey,ctrl:!!e.ctrlKey,composing:!!e.isComposing,id:e.target?.id||'',tag:e.target?.tagName||'',value:e.target?.value||'',checked:!!e.target?.checked,start:e.target?.selectionStart||0,end:e.target?.selectionEnd||0,open:!!e.target?.open,dataset:{...e.target?.dataset},ancestors:e.composedPath?.().map(n=>n.id).filter(Boolean)||[],storageKey:e.key||'',x:e.clientX||0,y:e.clientY||0});
let deferredInstall;
export const subscribe = target => name => emit => () => {const node=target==='document'?document:target==='viewport'?visualViewport:window;const fn=e=>{if(name==='beforeinstallprompt'){e.preventDefault();deferredInstall=e;}emit(eventValue(e))();};node?.addEventListener(name,fn);return ()=>node?.removeEventListener(name,fn);};
export const callback = name => emit => () => {const fn=value=>emit(value)();window[name]=fn;return ()=>{if(window[name]===fn)delete window[name];};};
export const eventData = e => () => eventValue(e);
export const focus = selector => () => document.querySelector(selector)?.focus();
export const selectRange = selector => start => end => () => document.querySelector(selector)?.setSelectionRange(start,end);
const geometry=n=>{if(!n)return {};const r=n.getBoundingClientRect();return {x:r.x,y:r.y,width:r.width,height:r.height,top:r.top,bottom:r.bottom,scrollTop:n.scrollTop,scrollHeight:n.scrollHeight,clientHeight:n.clientHeight,clientWidth:n.clientWidth,scrollWidth:n.scrollWidth,dataset:{...n.dataset},value:n.value||'',start:n.selectionStart||0,end:n.selectionEnd||0,text:n.textContent||'',focused:document.activeElement===n,open:!!n.open};};
export const measure = selector => () => geometry(document.querySelector(selector));
export const measureChildren = selector => () => [...(document.querySelector(selector)?.children||[])].map(geometry);
export const scroll = selector => value => () => {const n=document.querySelector(selector);if(n&&Math.abs(n.scrollTop-value)>1)n.scrollTop=value;};
export const scrollIntoView = selector => () => document.querySelector(selector)?.scrollIntoView({block:'start'});
export const cssProperty = selector => name => () => {const n=document.querySelector(selector);return n?getComputedStyle(n).getPropertyValue(name).trim():'';};
export const setStyle = selector => name => value => () => document.querySelector(selector)?.style.setProperty(name,value);
export const setAttribute = selector => name => value => () => document.querySelector(selector)?.setAttribute(name,value);
export const title = value => () => {document.title=value;};
export const nativeMessage = value => () => {if(window.webkit?.messageHandlers?.browser)window.webkit.messageHandlers.browser.postMessage(value);else window.chrome?.webview?.postMessage(value);};
export const showDialog = selector => open => () => {const n=document.querySelector(selector);if(!n)return;if(open&&!n.open)n.showModal();else if(!open&&n.open)n.close();};
export const historyPush = value => () => history.pushState(value,'');
export const historyReplace = path => () => history.replaceState(null,'',path);
export const historyBack = () => history.back();
export const reload = () => location.reload();
export const confirm = text => () => window.confirm(text);
export const clipboard = text => () => navigator.clipboard.writeText(text);
export const chooseFileImpl = accept => limit => () => new Promise((resolve, reject) => {
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = accept;
  input.style.cssText = 'position:fixed;left:-10000px;top:0;width:1px;height:1px;opacity:0';
  document.body.appendChild(input);
  let settled = false;
  const finish = (fn, value) => {
    if (settled) return;
    settled = true;
    input.remove();
    fn(value);
  };
  input.addEventListener('cancel', () => finish(resolve, null));
  input.addEventListener('change', () => {
    const file = input.files[0];
    if (!file) { finish(resolve, null); return; }
    if (file.size > limit) { finish(reject, new Error('File is too large.')); return; }
    const reader = new FileReader();
    reader.onerror = () => finish(reject, new Error('Could not read this file.'));
    reader.onabort = () => finish(reject, new Error('File reading was cancelled.'));
    reader.onload = () => {
      if (typeof reader.result !== 'string' || !reader.result.includes(',')) {
        finish(reject, new Error('Could not read this file.'));
        return;
      }
      finish(resolve, {
        name: file.name,
        mime: file.type === '' ? null : file.type,
        size: file.size,
        data: reader.result.slice(reader.result.indexOf(',') + 1)
      });
    };
    try { reader.readAsDataURL(file); }
    catch (error) { finish(reject, error); }
  });
  try { input.click(); }
  catch (error) { finish(reject, error); }
});
const readQR=(image,width,height,limit)=>{const c=document.createElement('canvas'),scale=Math.min(1,limit/Math.max(width,height));c.width=Math.round(width*scale);c.height=Math.round(height*scale);const x=c.getContext('2d',{willReadFrequently:true});x.drawImage(image,0,0,c.width,c.height);const q=RemoteVendor.jsQR(x.getImageData(0,0,c.width,c.height).data,c.width,c.height);return {data:q?.data||'',width,height};};
export const decodeQR = source => limit => async () => {const i=new Image();i.src=source;await i.decode();if(i.naturalWidth*i.naturalHeight>20000000)throw new Error('Image dimensions are too large.');return readQR(i,i.naturalWidth,i.naturalHeight,limit);};
export const cameraStart = selector => async () => {const c=await navigator.mediaDevices.getUserMedia({video:{facingMode:{ideal:'environment'}},audio:false}),v=document.querySelector(selector);if(!v){c.getTracks().forEach(t=>t.stop());throw new Error('Camera view closed.');}v.srcObject=c;await v.play();return c;};
export const cameraStop = stream => () => stream.getTracks().forEach(t=>t.stop());
export const cameraQR = selector => limit => () => {const v=document.querySelector(selector);return v?.readyState>=2&&v.videoWidth?readQR(v,v.videoWidth,v.videoHeight,limit):{data:''};};
const voiceRate=16000;
const voiceWav=(samples,rate)=>{const v=new DataView(new ArrayBuffer(44+samples.length*2)),tag=(o,s)=>{for(let i=0;i<4;i++)v.setUint8(o+i,s.charCodeAt(i));};
  tag(0,'RIFF');v.setUint32(4,36+samples.length*2,true);tag(8,'WAVE');tag(12,'fmt ');v.setUint32(16,16,true);v.setUint16(20,1,true);v.setUint16(22,1,true);
  v.setUint32(24,rate,true);v.setUint32(28,rate*2,true);v.setUint16(32,2,true);v.setUint16(34,16,true);tag(36,'data');v.setUint32(40,samples.length*2,true);
  for(let i=0;i<samples.length;i++)v.setInt16(44+i*2,Math.max(-1,Math.min(1,samples[i]))*0x7fff,true);return v.buffer;};
const voiceBase64=buffer=>{const u=new Uint8Array(buffer);let s='';for(let i=0;i<u.length;i+=0x8000)s+=String.fromCharCode(...u.subarray(i,i+0x8000));return btoa(s);};
// Browser errors (DOMException) become plain Errors so the app can show their real message.
const voiceError=e=>e instanceof DOMException||!(e instanceof Error)?new Error((e&&e.name?e.name+': ':'')+(e&&e.message?e.message:String(e))):e;
// Records the microphone as a series of voice pieces: every segmentSeconds, a finished
// 16 kHz mono WAV piece is handed to onSegment while recording continues; the last one is marked final.
// The AudioWorklet is loaded from a real same-origin file (workletUrl), which a strict CSP (script-src 'self') allows.
export const voiceStart = workletUrl => segmentSeconds => onSegment => () => startVoice(workletUrl,segmentSeconds,onSegment).catch(e=>{throw voiceError(e);});
const startVoice = async (workletUrl,segmentSeconds,onSegment) => {
  if(!navigator.mediaDevices?.getUserMedia)throw new Error('Microphone is not available here.');
  if(!(segmentSeconds>0))throw new Error('Segment length must be positive.');
  const stream=await navigator.mediaDevices.getUserMedia({audio:{channelCount:1,echoCancellation:true,noiseSuppression:true},video:false});
  try{
    const context=new AudioContext();
    await context.audioWorklet.addModule(workletUrl);
    const source=context.createMediaStreamSource(stream),node=new AudioWorkletNode(context,'s4-voice'),ratio=context.sampleRate/voiceRate;
    const recording=crypto.randomUUID(),size=segmentSeconds*voiceRate,buffer=new Float32Array(size);
    let filled=0,index=0,carry=new Float32Array(0),position=0,stopping=null;
    const emit=last=>{onSegment({recording,index,final:last,startMs:Math.round(index*size*1000/voiceRate),durationMs:Math.round(filled*1000/voiceRate),mime:'audio/wav',data:voiceBase64(voiceWav(buffer.subarray(0,filled),voiceRate))})();index++;filled=0;};
    const write=sample=>{buffer[filled++]=sample;if(filled===size)emit(false);};
    // Downsample by averaging each group of input samples; leftovers carry into the next block.
    node.port.onmessage=event=>{const input=new Float32Array(carry.length+event.data.length);input.set(carry);input.set(event.data,carry.length);
      let p=position;while(p+ratio<=input.length){const a=Math.floor(p),b=Math.floor(p+ratio);let sum=0;for(let k=a;k<b;k++)sum+=input[k];write(b>a?sum/(b-a):input[a]);p+=ratio;}
      const keep=Math.floor(p);carry=input.slice(keep);position=p-keep;};
    source.connect(node);node.connect(context.destination);
    return {stop:()=>stopping||(stopping=(async()=>{
      node.port.onmessage=null;source.disconnect();node.disconnect();stream.getTracks().forEach(t=>t.stop());await context.close();
      const durationMs=Math.round((index*size+filled)*1000/voiceRate);
      if(filled===0)buffer[filled++]=0; // the last piece always carries a sample, so the host learns the recording ended
      emit(true);
      return {recording,segments:index,durationMs};})().catch(e=>{throw voiceError(e);}))};
  }catch(error){stream.getTracks().forEach(t=>t.stop());throw error;}
};
export const voiceStop = voice => () => voice.stop();
export const qrImage = text => () => RemoteVendor.QRCode.toDataURL(text,{width:320,margin:3});
export const parseMarkup = source => {const t=document.createElement('template');t.innerHTML=source;const read=n=>n.nodeType===3?{text:n.textContent}:n.nodeType===1?{tag:n.localName,attrs:Object.fromEntries([...n.attributes].map(a=>[a.name,a.value])),children:[...n.childNodes].map(read),text:n.textContent,source:n.outerHTML}:{};return [...t.content.childNodes].map(read);};
export const parseURL = base => value => {try{const u=new URL(value,base);return {href:u.href,origin:u.origin,protocol:u.protocol,host:u.hostname,path:u.pathname,search:u.search,hash:u.hash,username:u.username,password:u.password};}catch{return null;}};
export const encodeURI = encodeURIComponent;
export const decodeURI = value => {try{return decodeURIComponent(value);}catch{return value;}};
const bytes=s=>typeof s==='string'?Uint8Array.from(atob(s),c=>c.charCodeAt(0)):Uint8Array.from(Object.values(s));
const b64=b=>btoa(Array.from(b,x=>String.fromCharCode(x)).join(''));
export const base64Encode = value => b64(new TextEncoder().encode(value));
export const base64Decode = value => {try{return new TextDecoder().decode(bytes(value));}catch{return null;}};
export const byteKey = value => typeof value==='string'?value:b64(bytes(value));
export const keyPair = () => {const p=RemoteVendor.nacl.box.keyPair();return {publicKey:b64(p.publicKey),secretKey:b64(p.secretKey)};};
export const publicKey = secret => b64(RemoteVendor.nacl.box.keyPair.fromSecretKey(bytes(secret)).publicKey);
export const byteLength = value => {try{return bytes(value).length;}catch{return -1;}};
export const randomBytes = count => () => b64(RemoteVendor.nacl.randomBytes(count));
export const box = text => nonce => peer => secret => b64(RemoteVendor.nacl.box(new TextEncoder().encode(text),bytes(nonce),bytes(peer),bytes(secret)));
export const unbox = cipher => nonce => peer => secret => {try{const p=RemoteVendor.nacl.box.open(bytes(cipher),bytes(nonce),bytes(peer),bytes(secret));return p?new TextDecoder().decode(p):'';}catch{return '';}};
export const digest = prefix => keys => async () => {const head=new TextEncoder().encode(prefix),rest=keys.map(bytes),data=new Uint8Array(head.length+rest.reduce((n,b)=>n+b.length,0));data.set(head);let cursor=head.length;for(const b of rest){data.set(b,cursor);cursor+=b.length;}return [...new Uint8Array(await crypto.subtle.digest('SHA-256',data))];};
export const parseTime = value => {const n=Date.parse(value);return Number.isFinite(n)?n:0;};
export const formatDate = locale => options => value => {const d=new Date(value);return Number.isFinite(d.getTime())?d.toLocaleString(locale||undefined,options):'';};
export const registerWorker = path => async () => {if('serviceWorker' in navigator)await navigator.serviceWorker.register(path);};
export const updateWorker = async () => {const r=await navigator.serviceWorker?.getRegistration();if(!r)return;await r.update();const w=r.installing||r.waiting||r.active;if(!w)return;await new Promise((resolve,reject)=>{const change=()=>{if(w.state==='activated'){w.removeEventListener('statechange',change);resolve();}else if(w.state==='redundant'){w.removeEventListener('statechange',change);reject(new Error('Update unavailable'));}};w.addEventListener('statechange',change);change();});};
export const installPrompt = async () => {const p=deferredInstall;deferredInstall=null;await p?.prompt();};
export const workerInstall = name => action => () => self.addEventListener(name,e=>e.waitUntil(action({})()));
export const workerFetch = eligible => action => () => self.addEventListener('fetch',e=>{const u=new URL(e.request.url),r={method:e.request.method,origin:u.origin,localOrigin:location.origin,url:e.request.url,path:u.pathname,mode:e.request.mode};if(eligible(r))e.respondWith(action(r)());});
export const cacheAll = key => urls => async () => {const c=await caches.open(key);await c.addAll(urls);};
export const cacheKeys = () => caches.keys();
export const cacheDelete = key => async () => {await caches.delete(key);};
export const cacheResponse = key => url => async () => {const c=await caches.open(key);return (await c.match(url,{ignoreSearch:true}))||fetch(url);};
export const fetchResponse = url => () => fetch(url);
export const activateWorker = () => self.skipWaiting();

export const requestBytes = url => headers => async () => {const r=await fetch(url,{headers,signal:AbortSignal.timeout(20000),cache:'no-store',redirect:'error'});return {status:r.status,mime:r.headers.get('Content-Type')||'',body:b64(new Uint8Array(await r.arrayBuffer()))};};
export const removeAttribute = selector => name => () => document.querySelector(selector)?.removeAttribute(name);
export const timestamp = value => new Date(value).toISOString();

export const styleSheet = id => css => () => {let s=document.getElementById(id);if(!s){s=document.createElement("style");s.id=id;document.documentElement.append(s);}s.textContent=css;};
export const callbackResult = name => fn => () => {window[name]=value=>fn(value)();};

export const subscribeEvent = target => name => emit => () => {const node=target==="document"?document:window;const listener=e=>emit(e)();node.addEventListener(name,listener);return ()=>node.removeEventListener(name,listener);};

export const subscribeMedia = query => emit => () => {const media=matchMedia(query),listener=()=>emit();media.addEventListener("change",listener);return ()=>media.removeEventListener("change",listener);};