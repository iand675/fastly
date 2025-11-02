/*
 * Fastly Compute Response Hostcalls
 *
 * Header declarations for Fastly response-related hostcalls.
 * These functions are provided by the Fastly Compute runtime.
 */

#ifndef FASTLY_RESP_H
#define FASTLY_RESP_H

#include <stdint.h>
#include <stddef.h>

/* Status type returned by all hostcalls */
typedef uint32_t fastly_status_t;

/* Handle types */
typedef uint32_t fastly_response_handle_t;
typedef uint32_t fastly_body_handle_t;

/*
 * Create a new HTTP response
 */
fastly_status_t fastly_resp_new(
    fastly_response_handle_t *resp_handle_out
);

/*
 * Set the HTTP status code of a response
 */
fastly_status_t fastly_resp_status_set(
    fastly_response_handle_t resp_handle,
    uint16_t status
);

/*
 * Send a response downstream to the client
 */
fastly_status_t fastly_resp_send_downstream(
    fastly_response_handle_t resp_handle,
    fastly_body_handle_t body_handle,
    uint32_t streaming
);

/*
 * Insert a header into an HTTP response
 */
fastly_status_t fastly_http_resp_header_insert(
    fastly_response_handle_t resp_handle,
    const uint8_t *name,
    size_t name_len,
    const uint8_t *value,
    size_t value_len
);

#endif /* FASTLY_RESP_H */
