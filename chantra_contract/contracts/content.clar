
;; title: content
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

;; Content Marketplace Smart Contract

(define-map content-listings 
    {content-id: uint} 
    {
        creator: principal,
        title: (string-ascii 100),
        price: uint,
        is-sold: bool,
        content-hash: (buff 32)
    }
)

(define-map content-ownership
    {content-id: uint}
    {owner: principal}
)

(define-data-var next-content-id uint u0)

;; Create a new content listing
(define-public (list-content 
    (title (string-ascii 100))
    (price uint)
    (content-hash (buff 32))
)
    (let 
        (
            (content-id (+ (var-get next-content-id) u1))
        )
        (asserts! (> price u0) (err u1))
        
        (map-set content-listings 
            {content-id: content-id}
            {
                creator: tx-sender,
                title: title,
                price: price,
                is-sold: false,
                content-hash: content-hash
            }
        )
        
        (map-set content-ownership
            {content-id: content-id}
            {owner: tx-sender}
        )
        
        (var-set next-content-id content-id)
        
        (ok content-id)
    )
)

;; Purchase content
(define-public (purchase-content (content-id uint))
    (let 
        (
            (content (unwrap! 
                (map-get? content-listings {content-id: content-id}) 
                (err u2)
            ))
            (current-owner (unwrap! 
                (map-get? content-ownership {content-id: content-id}) 
                (err u3)
            ))
        )
        
        ;; Ensure content is not already sold
        (asserts! (not (get is-sold content)) (err u4))
        
        ;; Transfer payment to current owner
        (try! (stx-transfer? 
            (get price content) 
            tx-sender 
            (get owner current-owner)
        ))
        
        ;; Update ownership
        (map-set content-ownership
            {content-id: content-id}
            {owner: tx-sender}
        )
        
        ;; Mark content as sold
        (map-set content-listings 
            {content-id: content-id}
            (merge content {is-sold: true})
        )
        
        (ok content-id)
    )
)

;; Get content details
(define-read-only (get-content-details (content-id uint))
    (map-get? content-listings {content-id: content-id})
)

;; Get content ownership
(define-read-only (get-content-owner (content-id uint))
    (map-get? content-ownership {content-id: content-id})
)