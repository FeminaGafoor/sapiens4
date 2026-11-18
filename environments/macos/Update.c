#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/xattr.h>
#include <sys/acl.h>
#include <sys/attr.h>
#include <copyfile.h>
#include <CommonCrypto/CommonDigest.h>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int d2_validate(const char *path) {
    struct stat st;
    if (lstat(path, &st)) return errno;
    if ((!S_ISREG(st.st_mode) && !S_ISDIR(st.st_mode)) ||
        (S_ISREG(st.st_mode) && st.st_nlink != 1) ||
        (st.st_flags & (UF_IMMUTABLE | UF_APPEND | SF_IMMUTABLE | SF_APPEND))) return ENOTSUP;
    struct statfs fs;
    if (statfs(path, &fs)) return errno;
    return strcmp(fs.f_fstypename, "apfs") ? ENOTSUP : 0;
}
int d2_lease(const char *path, int mode) {
    int fd = open(path, O_RDWR | O_CREAT | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return -1;
    struct flock lock = {.l_whence = SEEK_SET, .l_type = mode ? F_WRLCK : F_RDLCK};
    int status;
    do { status = fcntl(fd, mode == 2 ? F_SETLK : F_SETLKW, &lock); } while (status && errno == EINTR);
    if (status) {
        close(fd); return -1;
    }
    return fd;
}
int d2_downgrade(int fd) {
    struct flock lock = {.l_whence = SEEK_SET, .l_type = F_RDLCK};
    return fcntl(fd, F_SETLK, &lock);
}
int d2_hangup(void) {
    struct pollfd fd = {STDIN_FILENO, 0, 0};
    return poll(&fd, 1, 0) > 0 && (fd.revents & POLLHUP);
}
int d2_publish(const char *path, const char *temporary, const void *bytes, size_t count) {
    int fd = open(temporary, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return errno;
    int error = 0;
    const char *cursor = bytes;
    while (count) {
        ssize_t n = write(fd, cursor, count);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { error = errno ? errno : EIO; break; }
        cursor += n; count -= n;
    }
    if (!error && fcntl(fd, F_FULLFSYNC)) error = errno;
    if (close(fd) && !error) error = errno;
    if (!error && rename(temporary, path)) error = errno;
    if (!error) {
        char *parent = strdup(path);
        char *slash = strrchr(parent, '/');
        if (!slash) error = EINVAL;
        else {
            *slash = 0;
            int directory = open(*parent ? parent : "/", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
            if (directory < 0) error = errno;
            else { if (fsync(directory) || fcntl(directory, F_FULLFSYNC)) error = errno; close(directory); }
        }
        free(parent);
    }
    if (error) unlink(temporary);
    return error;
}
int d2_copy(const char *source, const char *destination, int metadata) {
    int error = d2_validate(source);
    if (error) return error;
    if (copyfile(source, destination, NULL, (metadata ? COPYFILE_METADATA : COPYFILE_ALL) | COPYFILE_NOFOLLOW)) return errno;
    struct stat original;
    if (lstat(source, &original)) return errno;
    struct attrlist attributes = {.bitmapcount = ATTR_BIT_MAP_COUNT, .commonattr = ATTR_CMN_CRTIME};
    if (setattrlist(destination, &attributes, &original.st_birthtimespec, sizeof(original.st_birthtimespec), FSOPT_NOFOLLOW)) return errno;
    int fd = open(destination, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
    if (fd < 0) return errno;
    struct stat st;
    if (fstat(fd, &st)) error = errno;
    else if (S_ISREG(st.st_mode) ? fcntl(fd, F_FULLFSYNC) : fsync(fd)) error = errno;
    close(fd);
    return error;
}
static int compare(const void *a, const void *b) { return strcmp(*(const char **)a, *(const char **)b); }
int d2_metadata(const char *path, unsigned char *digest) {
    struct stat st;
    int error = d2_validate(path);
    if (error) return error;
    if (lstat(path, &st)) return errno;
    CC_SHA256_CTX hash;
    CC_SHA256_Init(&hash);
#define HASH(value) CC_SHA256_Update(&hash, &(value), sizeof(value))
    HASH(st.st_mode); HASH(st.st_uid); HASH(st.st_gid); HASH(st.st_flags);
    HASH(st.st_mtimespec); HASH(st.st_birthtimespec);
    acl_t acl = acl_get_file(path, ACL_TYPE_EXTENDED);
    if (!acl && errno != ENOENT) return errno;
    ssize_t length = 0;
    char *text = acl ? acl_to_text(acl, &length) : NULL;
    if (acl && !text) { acl_free(acl); return errno; }
    HASH(length);
    if (text) { CC_SHA256_Update(&hash, text, (CC_LONG)length); acl_free(text); }
    if (acl) acl_free(acl);
    ssize_t size = listxattr(path, NULL, 0, XATTR_NOFOLLOW);
    if (size < 0) return errno;
    char *names = malloc(size + 1);
    if (!names) return ENOMEM;
    if (listxattr(path, names, size, XATTR_NOFOLLOW) != size) { free(names); return EIO; }
    char **sorted = calloc(size + 1, sizeof(char *));
    if (!sorted) { free(names); return ENOMEM; }
    size_t total = 0;
    for (char *p = names; p < names + size; p += strlen(p) + 1) sorted[total++] = p;
    qsort(sorted, total, sizeof(char *), compare);
    for (size_t i = 0; i < total && !error; i++) {
        ssize_t n = getxattr(path, sorted[i], NULL, 0, 0, XATTR_NOFOLLOW);
        if (n < 0) { error = errno; break; }
        char *value = malloc(n + 1);
        if (!value) { error = ENOMEM; break; }
        if (getxattr(path, sorted[i], value, n, 0, XATTR_NOFOLLOW) != n) error = EIO;
        else { CC_SHA256_Update(&hash, sorted[i], (CC_LONG)strlen(sorted[i]) + 1); HASH(n); CC_SHA256_Update(&hash, value, (CC_LONG)n); }
        free(value);
    }
    free(sorted); free(names);
    if (error) return error;
    if (S_ISREG(st.st_mode)) {
        int fd = open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
        if (fd < 0) return errno;
        char buffer[65536]; ssize_t n;
        while ((n = read(fd, buffer, sizeof(buffer))) > 0) CC_SHA256_Update(&hash, buffer, (CC_LONG)n);
        if (n < 0) error = errno;
        close(fd);
    }
    CC_SHA256_Final(digest, &hash);
    return error;
}

int d2_flush(const char *path, int full) {
    int fd = open(path, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) return errno;
    int error = 0;
    if (fsync(fd) || (full && fcntl(fd, F_FULLFSYNC))) error = errno;
    close(fd);
    return error;
}
