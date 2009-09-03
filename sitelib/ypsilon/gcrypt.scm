(library (ypsilon gcrypt)
  (export GCRY_MD_NONE
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
       (define name (c-function lib lib-name ret __stdcall name args)))))

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
