/* The size of a task file, with one statx call that need not reach the file's data servers.
 *
 * On Lustre, stat() fetches the size from the object storage servers that hold the file (a "glimpse"), which costs 10-70 ms
 * a file on Expanse's scratch when the client holds no lock on it; a scheduler that builds a task map stats thousands of
 * files in a row, and spent minutes there before its first task (bootstrap-compute #1344). AT_STATX_DONT_SYNC lets the
 * filesystem answer from what the metadata server knows (Lustre's lazy size, kept on close): the existence and type are
 * exact, and the size is the one of the file's last close, which is what a finished task's output has. Other filesystems
 * ignore the flag or answer from their cache.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/stat.h>

/* 1: a regular file, its size in *size; 0: something else (a directory, ...); -1: an error, in errno (ENOENT: nothing).
 * Follows symbolic links, as stat() does. */
int hyperion_scheduler_task_file_size(const char *path, int64_t *size)
{
  struct statx s;
  if (statx(AT_FDCWD, path, AT_STATX_DONT_SYNC, STATX_TYPE | STATX_SIZE, &s) != 0)
    return -1;
  if (!(s.stx_mask & STATX_TYPE)) {
    errno = EIO;
    return -1;
  }
  if (!S_ISREG(s.stx_mode))
    return 0;
  *size = (int64_t) s.stx_size;
  return 1;
}
