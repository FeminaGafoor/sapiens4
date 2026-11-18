export const openDatabase = name => version => stores => failed => ready => () => {
  const request=indexedDB.open(name,version);let cancelled=false,database;
  request.onupgradeneeded=()=>{for(const store of stores)if(!request.result.objectStoreNames.contains(store))request.result.createObjectStore(store);};
  request.onerror=()=>failed(request.error?.message||'open')();
  request.onblocked=()=>{cancelled=true;failed('blocked')();};
  request.onsuccess=()=>{database=request.result;if(cancelled)database.close();else ready(database)();};
  return ()=>{cancelled=true;database?.close();};
};
export const closeDatabase = database => () => database.close();
export const transact = database => mode => store => operation => args => failed => ready => () => {
  const transaction=database.transaction(store,mode),object=transaction.objectStore(store),request=object[operation](...args);let result;
  request.onsuccess=()=>{result=request.result;};
  transaction.oncomplete=()=>ready(result??null)();
  transaction.onabort=()=>failed(transaction.error?.message||'abort')();
  return ()=>{try{transaction.abort();}catch{}};
};
