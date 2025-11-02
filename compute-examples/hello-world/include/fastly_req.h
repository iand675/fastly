/*
 * Fastly Compute Request Hostcalls
 *
 * Header declarations for Fastly request-related hostcalls.
 * These functions are provided by the Fastly Compute runtime.
 */

#ifndef FASTLY_REQ_H
#define FASTLY_REQ_H

#include <stdint.h>
#include <stddef.h>

/* Status type returned by all hostcalls */
typedef uint32_t fastly_status_t;

/* Handle types */
typedef uint32_t fastly_request_handle_t;
typedef uint32_t fastly_body_handle_t;

/*
 * Get the downstream client request
 * Returns the incoming HTTP request from the client.
 */
fastly_status_t fastly_req_downstream_client_request(
    fastly_request_handle_t *req_handle_out,
    fastly_body_handle_t *body_handle_out
);

/*
 * Get the body of a request
 */
fastly_status_t fastly_req_body_downstream_get(
    fastly_request_handle_t req_handle,
    fastly_body_handle_t *body_handle_out
);

#endif /* FASTLY_REQ_H */
