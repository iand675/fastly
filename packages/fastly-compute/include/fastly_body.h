/*
 * Fastly Compute Body Hostcalls
 *
 * Header declarations for Fastly HTTP body-related hostcalls.
 * These functions are provided by the Fastly Compute runtime.
 */

#ifndef FASTLY_BODY_H
#define FASTLY_BODY_H

#include <stdint.h>
#include <stddef.h>

/* Status type returned by all hostcalls */
typedef uint32_t fastly_status_t;

/* Handle type */
typedef uint32_t fastly_body_handle_t;

/*
 * Create a new HTTP body
 */
fastly_status_t fastly_http_body_new(
    fastly_body_handle_t *body_handle_out
);

/*
 * Write data to an HTTP body
 * write_mode: 0 = append to back, 1 = prepend to front
 */
fastly_status_t fastly_http_body_write(
    fastly_body_handle_t body_handle,
    const uint8_t *data,
    size_t data_len,
    uint32_t write_mode,
    size_t *nwritten_out
);

/*
 * Close an HTTP body (finish writing)
 */
fastly_status_t fastly_http_body_close(
    fastly_body_handle_t body_handle
);

#endif /* FASTLY_BODY_H */
