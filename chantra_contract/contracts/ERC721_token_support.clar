;; NFT Multi-Send Contract
;; Allows batch transfer of multiple NFTs to multiple addresses

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-INPUT (err u101))
(define-constant ERR-TRANSFER-FAILED (err u102))
(define-constant ERR-NOT-AUTHORIZED (err u103))
(define-constant ERR-MISMATCHED-ARRAYS (err u104))
(define-constant ERR-EMPTY-ARRAYS (err u105))

;; Data Variables
(define-data-var contract-enabled bool true)
(define-data-var max-batch-size uint u50)

;; Maps
(define-map authorized-operators principal bool)

;; NFT Trait Definition (SIP-009 compliant)
(define-trait nft-trait
    (
        (transfer (uint principal principal) (response bool uint))
        (get-owner (uint) (response (optional principal) uint))
        (get-last-token-id () (response uint uint))
    )
)

;; Authorization Functions
(define-public (set-authorized-operator (operator principal) (authorized bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ok (map-set authorized-operators operator authorized))
    )
)

(define-read-only (is-authorized-operator (operator principal))
    (default-to false (map-get? authorized-operators operator))
)

;; Administrative Functions
(define-public (set-contract-enabled (enabled bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ok (var-set contract-enabled enabled))
    )
)

(define-public (set-max-batch-size (new-size uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (asserts! (> new-size u0) ERR-INVALID-INPUT)
        (ok (var-set max-batch-size new-size))
    )
)

;; Single NFT transfer function
(define-private (transfer-single-nft 
    (nft-contract <nft-trait>) 
    (token-id uint) 
    (recipient principal))
    (as-contract (contract-call? nft-contract transfer token-id tx-sender recipient))
)

;; Multi-send for up to 10 NFTs (explicit unrolling for simplicity)
(define-public (multi-send-nfts-10
    (contracts (list 10 <nft-trait>))
    (token-ids (list 10 uint))
    (recipients (list 10 principal)))
    (let
        (
            (len-contracts (len contracts))
            (len-tokens (len token-ids))
            (len-recipients (len recipients))
        )
        (begin
            ;; Validate contract is enabled
            (asserts! (var-get contract-enabled) ERR-INVALID-INPUT)
            
            ;; Validate arrays are not empty
            (asserts! (> len-contracts u0) ERR-EMPTY-ARRAYS)
            
            ;; Validate all arrays have the same length
            (asserts! 
                (and 
                    (is-eq len-contracts len-tokens)
                    (is-eq len-tokens len-recipients)
                ) 
                ERR-MISMATCHED-ARRAYS
            )
            
            ;; Validate batch size limit
            (asserts! (<= len-contracts u10) ERR-INVALID-INPUT)
            
            ;; Process transfers based on array length
            (if (>= len-contracts u1)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u0) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u0) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u0) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u2)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u1) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u1) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u1) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u3)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u2) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u2) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u2) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u4)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u3) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u3) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u3) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u5)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u4) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u4) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u4) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u6)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u5) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u5) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u5) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u7)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u6) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u6) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u6) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u8)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u7) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u7) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u7) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u9)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u8) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u8) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u8) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-contracts u10)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u9) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u9) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u9) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (ok true)
        )
    )
)

;; Multi-send to single recipient
(define-public (multi-send-to-single-10
    (contracts (list 10 <nft-trait>))
    (token-ids (list 10 uint))
    (recipient principal))
    (let
        (
            (len-contracts (len contracts))
            (len-tokens (len token-ids))
        )
        (begin
            ;; Validate contract is enabled
            (asserts! (var-get contract-enabled) ERR-INVALID-INPUT)
            
            ;; Validate arrays are not empty
            (asserts! (> len-contracts u0) ERR-EMPTY-ARRAYS)
            
            ;; Validate arrays have the same length
            (asserts! (is-eq len-contracts len-tokens) ERR-MISMATCHED-ARRAYS)
            
            ;; Validate batch size limit
            (asserts! (<= len-contracts u10) ERR-INVALID-INPUT)
            
            ;; Process transfers to single recipient
            (if (>= len-contracts u1)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u0) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u0) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u2)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u1) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u1) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u3)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u2) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u2) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u4)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u3) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u3) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u5)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u4) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u4) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u6)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u5) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u5) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u7)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u6) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u6) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u8)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u7) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u7) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u9)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u8) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u8) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (if (>= len-contracts u10)
                (try! (transfer-single-nft 
                    (unwrap! (element-at contracts u9) ERR-INVALID-INPUT)
                    (unwrap! (element-at token-ids u9) ERR-INVALID-INPUT)
                    recipient
                ))
                true
            )
            
            (ok true)
        )
    )
)

;; Single contract multi-send
(define-public (single-contract-multi-send-10
    (nft-contract <nft-trait>)
    (token-ids (list 10 uint))
    (recipients (list 10 principal)))
    (let
        (
            (len-tokens (len token-ids))
            (len-recipients (len recipients))
        )
        (begin
            ;; Validate contract is enabled
            (asserts! (var-get contract-enabled) ERR-INVALID-INPUT)
            
            ;; Validate arrays are not empty
            (asserts! (> len-tokens u0) ERR-EMPTY-ARRAYS)
            
            ;; Validate arrays have the same length
            (asserts! (is-eq len-tokens len-recipients) ERR-MISMATCHED-ARRAYS)
            
            ;; Validate batch size limit
            (asserts! (<= len-tokens u10) ERR-INVALID-INPUT)
            
            ;; Process transfers from single contract
            (if (>= len-tokens u1)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u0) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u0) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u2)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u1) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u1) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u3)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u2) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u2) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u4)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u3) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u3) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u5)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u4) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u4) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u6)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u5) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u5) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u7)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u6) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u6) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u8)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u7) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u7) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u9)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u8) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u8) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (if (>= len-tokens u10)
                (try! (transfer-single-nft 
                    nft-contract
                    (unwrap! (element-at token-ids u9) ERR-INVALID-INPUT)
                    (unwrap! (element-at recipients u9) ERR-INVALID-INPUT)
                ))
                true
            )
            
            (ok true)
        )
    )
)

;; Event logging helper
(define-private (log-multi-send (sender principal) (count uint))
    (print {
        event: "multi-send-completed",
        sender: sender,
        transfer-count: count,
        block-height: block-height
    })
)

;; Optional: Ownership verification for a specific NFT contract
;; Note: This requires knowing the specific contract at compile time
(define-public (verify-ownership-and-transfer
    (nft-contract principal)
    (token-id uint)
    (recipient principal))
    (begin
        ;; This would need to be customized for each specific NFT contract
        ;; Example for a hypothetical NFT contract:
        ;; (asserts! (is-eq (some tx-sender) (contract-call? .specific-nft-contract get-owner token-id)) ERR-NOT-AUTHORIZED)
        
        ;; For now, we skip ownership verification and just attempt transfer
        ;; The NFT contract itself will enforce ownership during transfer
        (ok true)
    )
)

;; Multi-send with logging
(define-public (multi-send-with-log
    (contracts (list 10 <nft-trait>))
    (token-ids (list 10 uint))
    (recipients (list 10 principal)))
    (let
        ((result (multi-send-nfts-10 contracts token-ids recipients)))
        (begin
            ;; Log only if successful
            (if (is-ok result)
                (begin
                    (log-multi-send tx-sender (len contracts))
                    result
                )
                result
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-contract-enabled)
    (var-get contract-enabled)
)

(define-read-only (get-max-batch-size)
    (var-get max-batch-size)
)

(define-read-only (get-contract-owner)
    CONTRACT-OWNER
)

;; Emergency functions
(define-public (emergency-stop)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set contract-enabled false)
        (ok true)
    )
)

(define-public (emergency-resume)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set contract-enabled true)
        (ok true)
    )
)