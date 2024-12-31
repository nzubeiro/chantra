
;; title: personalized
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Personalized Access Management Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-expired (err u103))

;; Data Maps
(define-map content-licenses
  { content-id: uint, user: principal }
  {
    access-token: (optional principal),
    expiration: uint,
    usage-rights: (string-ascii 100)
  }
)

(define-map content-details
  { content-id: uint }
  {
    creator: principal,
    required-token: (optional principal),
    price: uint
  }
)

;; Variables
(define-data-var next-content-id uint u1)

;; Private Functions
(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (current-time)
  (unwrap-panic (get-block-info? time u0))
)

;; Public Functions

;; Create new content
(define-public (create-content (required-token (optional principal)) (price uint))
  (let
    (
      (content-id (var-get next-content-id))
    )
    (asserts! (is-owner) err-owner-only)
    (map-set content-details
      { content-id: content-id }
      {
        creator: tx-sender,
        required-token: required-token,
        price: price
      }
    )
    (var-set next-content-id (+ content-id u1))
    (ok content-id)
  )
)

;; Grant access to content
(define-public (grant-access (content-id uint) (user principal) (duration uint) (usage-rights (string-ascii 100)))
  (let
    (
      (content (unwrap! (map-get? content-details { content-id: content-id }) err-not-found))
      (access-token (get required-token content))
      (expiration (+ (current-time) duration))
    )
    (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
    (map-set content-licenses
      { content-id: content-id, user: user }
      {
        access-token: access-token,
        expiration: expiration,
        usage-rights: usage-rights
      }
    )
    (ok true)
  )
)

;; Check access to content
(define-public (check-access (content-id uint) (user principal))
  (let
    (
      (license (unwrap! (map-get? content-licenses { content-id: content-id, user: user }) err-not-found))
      (current-time-value (current-time))
    )
    (asserts! (>= (get expiration license) current-time-value) err-expired)
    (ok {
      access-token: (get access-token license),
      expiration: (get expiration license),
      usage-rights: (get usage-rights license)
    })
  )
)

;; Revoke access to content
(define-public (revoke-access (content-id uint) (user principal))
  (let
    (
      (content (unwrap! (map-get? content-details { content-id: content-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator content)) err-unauthorized)
    (map-delete content-licenses { content-id: content-id, user: user })
    (ok true)
  )
)

;; Read-only function to get content details
(define-read-only (get-content-details (content-id uint))
  (map-get? content-details { content-id: content-id })
)