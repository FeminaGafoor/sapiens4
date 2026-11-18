#define _WIN32_WINNT 0x0602
#include <windows.h>
#include <aclapi.h>
#include <sddl.h>
#include <wintrust.h>
#include <softpub.h>
#include <wincrypt.h>
#include <bcrypt.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void *d2_lease(const wchar_t *path, int exclusive) {
    HANDLE h = CreateFileW(path, GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) return NULL;
    OVERLAPPED range = {0};
    if (!LockFileEx(h, exclusive ? LOCKFILE_EXCLUSIVE_LOCK : 0, 0, 1, 0, &range)) {
        CloseHandle(h); return NULL;
    }
    return h;
}
void d2_close(void *h) { CloseHandle(h); }
DWORD d2_downgrade(void *h) {
    OVERLAPPED range = {0};
    if (!LockFileEx(h, 0, 0, 1, 0, &range)) return GetLastError();
    if (!UnlockFileEx(h, 0, 1, 0, &range)) return GetLastError();
    return 0;
}
DWORD d2_publish(const wchar_t *path, const wchar_t *temporary, const BYTE *bytes, DWORD size) {
    HANDLE h = CreateFileW(temporary, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    DWORD written = 0, error = 0;
    if (!WriteFile(h, bytes, size, &written, NULL) || written != size || !FlushFileBuffers(h)) error = GetLastError();
    if (!error && written != size) error = ERROR_WRITE_FAULT;
    if (!CloseHandle(h) && !error) error = GetLastError();
    if (!error && !MoveFileExW(temporary, path, MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH)) error = GetLastError();
    if (error) DeleteFileW(temporary);
    return error;
}
static DWORD inspect(const wchar_t *path, BY_HANDLE_FILE_INFORMATION *info) {
    HANDLE h = CreateFileW(path, FILE_READ_ATTRIBUTES, FILE_SHARE_READ, NULL, OPEN_EXISTING,
        FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    DWORD error = GetFileInformationByHandle(h, info) ? 0 : GetLastError();
    CloseHandle(h);
    if (!error && (info->dwFileAttributes & (FILE_ATTRIBUTE_REPARSE_POINT | FILE_ATTRIBUTE_ENCRYPTED |
        FILE_ATTRIBUTE_SPARSE_FILE | FILE_ATTRIBUTE_COMPRESSED | FILE_ATTRIBUTE_OFFLINE))) error = ERROR_NOT_SUPPORTED;
    if (!error && !(info->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && info->nNumberOfLinks != 1) error = ERROR_NOT_SUPPORTED;
    return error;
}
DWORD d2_validate(const wchar_t *path) { BY_HANDLE_FILE_INFORMATION info; return inspect(path, &info); }
static DWORD security(const wchar_t *source, const wchar_t *destination) {
    PSID owner = NULL, group = NULL; PACL dacl = NULL; PSECURITY_DESCRIPTOR sd = NULL, dest = NULL;
    DWORD flags = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
    DWORD error = GetNamedSecurityInfoW((wchar_t *)source, SE_FILE_OBJECT, flags, &owner, &group, &dacl, NULL, &sd);
    if (error) return error;
    SECURITY_DESCRIPTOR_CONTROL control; DWORD revision;
    if (!GetSecurityDescriptorControl(sd, &control, &revision)) { error = GetLastError(); goto done; }
    PSID destOwner = NULL, destGroup = NULL;
    error = GetNamedSecurityInfoW((wchar_t *)destination, SE_FILE_OBJECT, flags, &destOwner, &destGroup, NULL, NULL, &dest);
    if (error) goto done;
    flags = DACL_SECURITY_INFORMATION | ((control & SE_DACL_PROTECTED) ? PROTECTED_DACL_SECURITY_INFORMATION : UNPROTECTED_DACL_SECURITY_INFORMATION);
    if (!EqualSid(owner, destOwner)) flags |= OWNER_SECURITY_INFORMATION;
    if (!EqualSid(group, destGroup)) flags |= GROUP_SECURITY_INFORMATION;
    if (control & SE_DACL_AUTO_INHERITED)
        error = SetNamedSecurityInfoW((wchar_t *)destination, SE_FILE_OBJECT, flags, owner, group, dacl, NULL);
    else if (!SetFileSecurityW(destination, flags, sd)) error = GetLastError();
 done:
    if (dest) LocalFree(dest);
    LocalFree(sd);
    return error;
}
static DWORD attributes(const wchar_t *source, const wchar_t *destination, BOOL flush) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source, &info);
    if (error) return error;
    DWORD attrs = GetFileAttributesW(destination);
    if (attrs == INVALID_FILE_ATTRIBUTES) return GetLastError();
    if (!SetFileAttributesW(destination, attrs & ~FILE_ATTRIBUTE_READONLY)) return GetLastError();
    HANDLE h = CreateFileW(destination, FILE_WRITE_ATTRIBUTES | (flush ? GENERIC_WRITE : 0), FILE_SHARE_READ,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    if (!SetFileTime(h, &info.ftCreationTime, NULL, &info.ftLastWriteTime)) error = GetLastError();
    if (!error && flush && !FlushFileBuffers(h)) error = GetLastError();
    CloseHandle(h);
    if (!error) error = security(source, destination);
    if (!SetFileAttributesW(destination, info.dwFileAttributes) && !error) error = GetLastError();
    return error;
}
DWORD d2_copy(const wchar_t *source, const wchar_t *destination) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source, &info);
    if (error) return error;
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) return ERROR_DIRECTORY;
    if (!CopyFileW(source, destination, TRUE)) return GetLastError();
    return 0;
}
static wchar_t *streamPath(const wchar_t *path, const wchar_t *stream) {
    wchar_t *result = malloc((wcslen(path)+wcslen(stream)+1)*sizeof(wchar_t));
    if (result) { wcscpy(result,path); wcscat(result,stream); }
    return result;
}
DWORD d2_directory_metadata(const wchar_t *source, const wchar_t *destination) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source,&info);
    if (!error) error = inspect(destination,&info);
    if (error) return error;
    WIN32_FIND_STREAM_DATA entry;
    HANDLE find = FindFirstStreamW(destination,FindStreamInfoStandard,&entry,0);
    if (find != INVALID_HANDLE_VALUE) {
        do {
            wchar_t *path = streamPath(destination,entry.cStreamName);
            if (!path) { error=ERROR_OUTOFMEMORY; break; }
            if (!DeleteFileW(path)) error=GetLastError();
            free(path);
            if (error) break;
        } while (FindNextStreamW(find,&entry));
        if (!error && GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
        FindClose(find);
    } else if (GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
    if (error) return error;
    find = FindFirstStreamW(source,FindStreamInfoStandard,&entry,0);
    if (find != INVALID_HANDLE_VALUE) {
        do {
            wchar_t *from=streamPath(source,entry.cStreamName), *to=streamPath(destination,entry.cStreamName);
            if (!from || !to) error=ERROR_OUTOFMEMORY;
            HANDLE input=INVALID_HANDLE_VALUE, output=INVALID_HANDLE_VALUE;
            if (!error) input=CreateFileW(from,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,0,NULL);
            if (!error && input==INVALID_HANDLE_VALUE) error=GetLastError();
            if (!error) output=CreateFileW(to,GENERIC_WRITE,0,NULL,CREATE_NEW,0,NULL);
            if (!error && output==INVALID_HANDLE_VALUE) error=GetLastError();
            BYTE buffer[65536]; DWORD count,written;
            while (!error) {
                if (!ReadFile(input,buffer,sizeof(buffer),&count,NULL)) { error=GetLastError(); break; }
                if (!count) break;
                if (!WriteFile(output,buffer,count,&written,NULL) || written!=count) { error=ERROR_WRITE_FAULT; break; }
            }
            if (!error && !FlushFileBuffers(output)) error=GetLastError();
            if (input!=INVALID_HANDLE_VALUE) CloseHandle(input);
            if (output!=INVALID_HANDLE_VALUE) CloseHandle(output);
            free(from); free(to);
            if (error) break;
        } while (FindNextStreamW(find,&entry));
        if (!error && GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
        FindClose(find);
    } else if (GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
    return error ? error : attributes(source,destination,FALSE);
}
DWORD d2_copy_metadata(const wchar_t *source, const wchar_t *destination) {
    DWORD kind = GetFileAttributesW(source);
    if (kind == INVALID_FILE_ATTRIBUTES) return GetLastError();
    return (kind & FILE_ATTRIBUTE_DIRECTORY) ? d2_directory_metadata(source,destination) : attributes(source,destination,TRUE);
}
static BOOL digest(const wchar_t *path, BYTE result[32]) {
    HANDLE file = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE) return FALSE;
    BCRYPT_ALG_HANDLE alg = NULL; BCRYPT_HASH_HANDLE hash = NULL;
    BYTE bytes[65536]; DWORD count; BOOL ok = FALSE;
    if (BCryptOpenAlgorithmProvider(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0) < 0) goto done;
    if (BCryptCreateHash(alg, &hash, NULL, 0, NULL, 0, 0) < 0) goto done;
    for (;;) {
        if (!ReadFile(file, bytes, sizeof(bytes), &count, NULL)) goto done;
        if (!count) break;
        if (BCryptHashData(hash, bytes, count, 0) < 0) goto done;
    }
    ok = BCryptFinishHash(hash, result, 32, 0) >= 0;
 done:
    if (hash) BCryptDestroyHash(hash);
    if (alg) BCryptCloseAlgorithmProvider(alg, 0);
    CloseHandle(file); return ok;
}
DWORD d2_metadata(const wchar_t *path, BYTE *out, DWORD capacity) {
    BY_HANDLE_FILE_INFORMATION info; DWORD used = 0;
    if (inspect(path, &info)) return 0;
#define APPEND(p,n) do { DWORD length_ = (DWORD)(n); if (used + length_ > capacity) goto failed; memcpy(out+used,(p),length_); used += length_; } while(0)
    APPEND(&info.dwFileAttributes, sizeof(DWORD));
    APPEND(&info.ftCreationTime, sizeof(FILETIME)); APPEND(&info.ftLastWriteTime, sizeof(FILETIME));
    DWORD flags = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
    PSECURITY_DESCRIPTOR sd = NULL; wchar_t *sddl = NULL;
    if (GetNamedSecurityInfoW((wchar_t *)path, SE_FILE_OBJECT, flags, NULL, NULL, NULL, NULL, &sd)) return 0;
    if (!ConvertSecurityDescriptorToStringSecurityDescriptorW(sd, SDDL_REVISION_1, flags, &sddl, NULL)) { LocalFree(sd); return 0; }
    DWORD length = (DWORD)((wcslen(sddl)+1)*sizeof(wchar_t));
    if (used + length > capacity) { LocalFree(sddl); LocalFree(sd); return 0; }
    memcpy(out+used,sddl,length); used += length; LocalFree(sddl); LocalFree(sd);
    {
        WIN32_FIND_STREAM_DATA data;
        HANDLE find = FindFirstStreamW(path, FindStreamInfoStandard, &data, 0);
        if (find == INVALID_HANDLE_VALUE) return GetLastError()==ERROR_HANDLE_EOF && (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? used : 0;
        do {
            wchar_t *stream = malloc((wcslen(path)+wcslen(data.cStreamName)+1)*sizeof(wchar_t));
            if (!stream) { FindClose(find); return 0; }
            wcscpy(stream,path); wcscat(stream,data.cStreamName);
            BYTE sha[32]; BOOL ok = digest(stream,sha); free(stream);
            length = (DWORD)((wcslen(data.cStreamName)+1)*sizeof(wchar_t));
            if (!ok || used+length+32>capacity) { FindClose(find); return 0; }
            memcpy(out+used,data.cStreamName,length); used+=length; memcpy(out+used,sha,32); used+=32;
        } while (FindNextStreamW(find,&data));
        DWORD error = GetLastError(); FindClose(find);
        if (error != ERROR_HANDLE_EOF) return 0;
    }
    return used;
 failed: return 0;
#undef APPEND
}
static DWORD trusted(const wchar_t *path, BYTE *fingerprint, DWORD *size) {
    WINTRUST_FILE_INFO file = {0}; file.cbStruct = sizeof(file); file.pcwszFilePath = path;
    WINTRUST_DATA data = {0}; data.cbStruct = sizeof(data); data.dwUIChoice = WTD_UI_NONE;
    data.fdwRevocationChecks = WTD_REVOKE_WHOLECHAIN; data.dwUnionChoice = WTD_CHOICE_FILE; data.pFile = &file;
    data.dwStateAction = WTD_STATEACTION_VERIFY; data.dwProvFlags = WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT;
    GUID policy = WINTRUST_ACTION_GENERIC_VERIFY_V2;
    LONG status = WinVerifyTrust(NULL, &policy, &data);
    if (!status) {
        CRYPT_PROVIDER_DATA *provider = WTHelperProvDataFromStateData(data.hWVTStateData);
        CRYPT_PROVIDER_SGNR *signer = provider ? WTHelperGetProvSignerFromChain(provider,0,FALSE,0) : NULL;
        CRYPT_PROVIDER_CERT *cert = signer ? WTHelperGetProvCertFromChain(signer,0) : NULL;
        if (!cert || !CertGetCertificateContextProperty(cert->pCert,CERT_SHA256_HASH_PROP_ID,fingerprint,size)) status = TRUST_E_SUBJECT_NOT_TRUSTED;
    }
    data.dwStateAction = WTD_STATEACTION_CLOSE; WinVerifyTrust(NULL,&policy,&data);
    return (DWORD)status;
}
DWORD d2_trust(const wchar_t *current, const wchar_t *payload) {
    BYTE a[32],b[32]; DWORD na=32,nb=32;
    DWORD error = trusted(current,a,&na);
    if (!error) error = trusted(payload,b,&nb);
    if (!error && (na!=32 || nb!=32 || memcmp(a,b,32))) error = TRUST_E_SUBJECT_NOT_TRUSTED;
    return error;
}
DWORD d2_job(void *job, int commit) {
    if (commit) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = {0};
        if (!QueryInformationJobObject(job,JobObjectExtendedLimitInformation,&limits,sizeof(limits),NULL)) return GetLastError();
        limits.BasicLimitInformation.LimitFlags &= ~JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        return SetInformationJobObject(job,JobObjectExtendedLimitInformation,&limits,sizeof(limits)) ? 0 : GetLastError();
    }
    if (!TerminateJobObject(job,1)) return GetLastError();
    JOBOBJECT_BASIC_ACCOUNTING_INFORMATION accounting;
    do {
        if (!QueryInformationJobObject(job,JobObjectBasicAccountingInformation,&accounting,sizeof(accounting),NULL)) return GetLastError();
        if (accounting.ActiveProcesses) Sleep(10);
    } while (accounting.ActiveProcesses);
    return 0;
}
