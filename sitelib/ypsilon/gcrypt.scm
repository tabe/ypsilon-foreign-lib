(library (ypsilon gcrypt)
  (export GCRYCTL_SET_KEY
          GCRYCTL_SET_IV
          GCRYCTL_CFB_SYNC
          GCRYCTL_RESET
          GCRYCTL_FINALIZE
          GCRYCTL_GET_KEYLEN
          GCRYCTL_GET_BLKLEN
          GCRYCTL_TEST_ALGO
          GCRYCTL_IS_SECURE
          GCRYCTL_GET_ASNOID
          GCRYCTL_ENABLE_ALGO
          GCRYCTL_DISABLE_ALGO
          GCRYCTL_DUMP_RANDOM_STATS
          GCRYCTL_DUMP_SECMEM_STATS
          GCRYCTL_GET_ALGO_NPKEY
          GCRYCTL_GET_ALGO_NSKEY
          GCRYCTL_GET_ALGO_NSIGN
          GCRYCTL_GET_ALGO_NENCR
          GCRYCTL_SET_VERBOSITY
          GCRYCTL_SET_DEBUG_FLAGS
          GCRYCTL_CLEAR_DEBUG_FLAGS
          GCRYCTL_USE_SECURE_RNDPOOL
          GCRYCTL_DUMP_MEMORY_STATS
          GCRYCTL_INIT_SECMEM
          GCRYCTL_TERM_SECMEM
          GCRYCTL_DISABLE_SECMEM_WARN
          GCRYCTL_SUSPEND_SECMEM_WARN
          GCRYCTL_RESUME_SECMEM_WARN
          GCRYCTL_DROP_PRIVS
          GCRYCTL_ENABLE_M_GUARD
          GCRYCTL_START_DUMP
          GCRYCTL_STOP_DUMP
          GCRYCTL_GET_ALGO_USAGE
          GCRYCTL_IS_ALGO_ENABLED
          GCRYCTL_DISABLE_INTERNAL_LOCKING
          GCRYCTL_DISABLE_SECMEM
          GCRYCTL_INITIALIZATION_FINISHED
          GCRYCTL_INITIALIZATION_FINISHED_P
          GCRYCTL_ANY_INITIALIZATION_P
          GCRYCTL_SET_CBC_CTS
          GCRYCTL_SET_CBC_MAC
          GCRYCTL_SET_CTR
          GCRYCTL_ENABLE_QUICK_RANDOM
          GCRYCTL_SET_RANDOM_SEED_FILE
          GCRYCTL_UPDATE_RANDOM_SEED_FILE
          GCRYCTL_SET_THREAD_CBS
          GCRYCTL_FAST_POLL
          GCRYCTL_SET_RANDOM_DAEMON_SOCKET
          GCRYCTL_USE_RANDOM_DAEMON
          GCRYCTL_FAKED_RANDOM_P
          GCRYCTL_SET_RNDEGD_SOCKET
          GCRYCTL_PRINT_CONFIG
          GCRYCTL_OPERATIONAL_P
          GCRYCTL_FIPS_MODE_P
          GCRYCTL_FORCE_FIPS_MODE
          GCRYCTL_SELFTEST
          GCRY_MD_NONE
          GCRY_MD_MD5
          GCRY_MD_SHA1
          GCRY_MD_RMD160
          GCRY_MD_MD2
          GCRY_MD_TIGER
          GCRY_MD_HAVAL
          GCRY_MD_SHA256
          GCRY_MD_SHA384
          GCRY_MD_SHA512
          GCRY_MD_SHA224
          GCRY_MD_MD4
          GCRY_MD_CRC32
          GCRY_MD_CRC32_RFC1510
          GCRY_MD_CRC24_RFC2440
          GCRY_MD_WHIRLPOOL
          GCRY_MD_FLAG_SECURE
          GCRY_MD_FLAG_HMAC
          gcry_check_version
          gcry_control
          gcry_md_open
          gcry_md_enable
          gcry_md_setkey
          gcry_md_close
          gcry_md_reset
          gcry_md_copy
          gcry_md_write
          gcry_md_putc
          gcry_md_read
          gcry_md_hash_buffer
          )
  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgcrypt.so")
          (on-sunos   "libgcrypt.so")
          (on-freebsd "libgcrypt.so")
          (on-openbsd "libgcrypt.so")
          (else
           (assertion-violation #f "can not locate libgcrypt, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-c-enum
    (GCRYCTL_SET_KEY . 1)
    GCRYCTL_SET_IV
    GCRYCTL_CFB_SYNC
    GCRYCTL_RESET
    GCRYCTL_FINALIZE
    GCRYCTL_GET_KEYLEN
    GCRYCTL_GET_BLKLEN
    GCRYCTL_TEST_ALGO
    GCRYCTL_IS_SECURE
    GCRYCTL_GET_ASNOID
    GCRYCTL_ENABLE_ALGO
    GCRYCTL_DISABLE_ALGO
    GCRYCTL_DUMP_RANDOM_STATS
    GCRYCTL_DUMP_SECMEM_STATS
    GCRYCTL_GET_ALGO_NPKEY
    GCRYCTL_GET_ALGO_NSKEY
    GCRYCTL_GET_ALGO_NSIGN
    GCRYCTL_GET_ALGO_NENCR
    GCRYCTL_SET_VERBOSITY
    GCRYCTL_SET_DEBUG_FLAGS
    GCRYCTL_CLEAR_DEBUG_FLAGS
    GCRYCTL_USE_SECURE_RNDPOOL
    GCRYCTL_DUMP_MEMORY_STATS
    GCRYCTL_INIT_SECMEM
    GCRYCTL_TERM_SECMEM
    GCRYCTL_DISABLE_SECMEM_WARN
    GCRYCTL_SUSPEND_SECMEM_WARN
    GCRYCTL_RESUME_SECMEM_WARN
    GCRYCTL_DROP_PRIVS
    GCRYCTL_ENABLE_M_GUARD
    GCRYCTL_START_DUMP
    GCRYCTL_STOP_DUMP
    GCRYCTL_GET_ALGO_USAGE
    GCRYCTL_IS_ALGO_ENABLED
    GCRYCTL_DISABLE_INTERNAL_LOCKING
    GCRYCTL_DISABLE_SECMEM
    GCRYCTL_INITIALIZATION_FINISHED
    GCRYCTL_INITIALIZATION_FINISHED_P
    GCRYCTL_ANY_INITIALIZATION_P
    GCRYCTL_SET_CBC_CTS
    GCRYCTL_SET_CBC_MAC
    GCRYCTL_SET_CTR
    GCRYCTL_ENABLE_QUICK_RANDOM
    GCRYCTL_SET_RANDOM_SEED_FILE
    GCRYCTL_UPDATE_RANDOM_SEED_FILE
    GCRYCTL_SET_THREAD_CBS
    GCRYCTL_FAST_POLL
    GCRYCTL_SET_RANDOM_DAEMON_SOCKET
    GCRYCTL_USE_RANDOM_DAEMON
    GCRYCTL_FAKED_RANDOM_P
    GCRYCTL_SET_RNDEGD_SOCKET
    GCRYCTL_PRINT_CONFIG
    GCRYCTL_OPERATIONAL_P
    GCRYCTL_FIPS_MODE_P
    GCRYCTL_FORCE_FIPS_MODE
    GCRYCTL_SELFTEST
    )

  (define-c-enum
    GCRY_MD_NONE
    GCRY_MD_MD5
    GCRY_MD_SHA1
    GCRY_MD_RMD160
    (GCRY_MD_MD2 . 5)
    GCRY_MD_TIGER
    GCRY_MD_HAVAL
    GCRY_MD_SHA256
    GCRY_MD_SHA384
    GCRY_MD_SHA512
    GCRY_MD_SHA224
    (GCRY_MD_MD4  . 301)
    GCRY_MD_CRC32
    GCRY_MD_CRC32_RFC1510
    GCRY_MD_CRC24_RFC2440
    GCRY_MD_WHIRLPOOL
    )

  (define-c-enum
    (GCRY_MD_FLAG_SECURE . 1)
    GCRY_MD_FLAG_HMAC
    )

  ;; const char * gcry_check_version (const char *req_version)
  (define-function char* gcry_check_version (char*))

  ;; gcry_error_t gcry_control (enum gcry_ctl_cmds cmd, ...)
  (define-function unsigned-int gcry_control (int ...))

  ;; gcry_error_t gcry_md_open (gcry_md_hd_t *hd, int algo, unsigned int flags)
  (define-function unsigned-int gcry_md_open (void* int unsigned-int))

  ;; gcry_error_t gcry_md_enable (gcry_md_hd_t h, int algo)
  (define-function unsigned-int gcry_md_enable (void* int))

  ;; gcry_error_t gcry_md_setkey (gcry_md_hd_t h, const void *key, size_t keylen)
  (define-function unsigned-int gcry_md_setkey (void* void* size_t))

  ;; void gcry_md_close (gcry_md_hd_t h)
  (define-function void gcry_md_close (void*))

  ;; void gcry_md_reset (gcry_md_hd_t h)
  (define-function void gcry_md_reset (void*))

  ;; gcry_error_t gcry_md_copy (gcry_md_hd_t *handle_dst, gcry_md_hd_t handle_src)
  (define-function unsigned-int gcry_md_copy (void* void*))

  ;; void gcry_md_write (gcry_md_hd_t h, const void *buffer, size_t length)
  (define-function void gcry_md_write (void* void* size_t))

  ;; void gcry_md_putc (gcry_md_hd_t h, int c)
  (define-function void gcry_md_putc (void* int))

  ;; unsigned char * gcry_md_read (gcry_md_hd_t h, int algo)
  (define-function void* gcry_md_read (void* int))

  ;; void gcry_md_hash_buffer (int algo, void *digest, const void *buffer, size_t length)
  (define-function void gcry_md_hash_buffer (int void* void* size_t))

)
