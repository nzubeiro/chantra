;; Enhanced Storage with Min/Max Constraints Smart Contract
;; This contract stores a value with comprehensive bounds management and additional features

;; Constants for error codes
(define-constant ERR-VALUE-TOO-LOW (err u100))
(define-constant ERR-VALUE-TOO-HIGH (err u101))
(define-constant ERR-INVALID-BOUNDS (err u102))
(define-constant ERR-NOT-AUTHORIZED (err u103))
(define-constant ERR-CONTRACT-FROZEN (err u104))
(define-constant ERR-INVALID-PERCENTAGE (err u105))
(define-constant ERR-HISTORY-EMPTY (err u106))
(define-constant ERR-INVALID-INDEX (err u107))
(define-constant ERR-COOLDOWN-ACTIVE (err u108))

;; Principal who deployed the contract (admin)
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var stored-value uint u0)
(define-data-var min-bound uint u0)
(define-data-var max-bound uint u100)
(define-data-var is-frozen bool false)
(define-data-var total-updates uint u0)
(define-data-var last-update-block uint u0)
(define-data-var cooldown-period uint u0)

;; History tracking
(define-data-var history-count uint u0)
(define-map value-history uint {value: uint, block: uint, sender: principal})

;; Access control
(define-map authorized-users principal bool)
(define-data-var public-access bool true)

;; Statistics
(define-data-var min-ever-stored uint u0)
(define-data-var max-ever-stored uint u0)
(define-data-var sum-of-all-values uint u0)

;; Events (using print for logging)
(define-private (emit-value-changed (old-value uint) (new-value uint) (sender principal))
  (print {event: "value-changed", old: old-value, new: new-value, sender: sender, block: block-height})
)

(define-private (emit-bounds-changed (old-min uint) (old-max uint) (new-min uint) (new-max uint))
  (print {event: "bounds-changed", old-min: old-min, old-max: old-max, new-min: new-min, new-max: new-max})
)

;; Initialize the contract with default values
(define-private (initialize)
  (begin
    (var-set min-bound u0)
    (var-set max-bound u100)
    (var-set stored-value u50)
    (var-set min-ever-stored u50)
    (var-set max-ever-stored u50)
    (var-set sum-of-all-values u50)
    (var-set total-updates u1)
    (var-set last-update-block block-height)
    (map-set authorized-users CONTRACT-OWNER true)
    (record-history u50)
    (ok true)
  )
)

;; Utility functions for min/max since Clarity doesn't have built-in min/max
(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
  (if (> a b) a b)
)

;; Access control helpers
(define-private (is-authorized (user principal))
  (or (is-eq user CONTRACT-OWNER)
    (and (var-get public-access) true)
    (default-to false (map-get? authorized-users user))
  )
)

(define-private (is-owner (user principal))
  (is-eq user CONTRACT-OWNER)
)

(define-private (check-cooldown)
  (let ((cooldown (var-get cooldown-period))
      (last-block (var-get last-update-block)))
    (if (> cooldown u0)
        (if (>= (- block-height last-block) cooldown)
          (ok true)
          ERR-COOLDOWN-ACTIVE)
        (ok true)
    )
  )
)

;; History management
(define-private (record-history (value uint))
  (let ((count (var-get history-count)))
    (begin
      (map-set value-history count {value: value, block: block-height, sender: tx-sender})
      (var-set history-count (+ count u1))
      true
    )
  )
)

;; Statistics updates
(define-private (update-stats (new-value uint))
  (begin
    (var-set min-ever-stored (min (var-get min-ever-stored) new-value))
    (var-set max-ever-stored (max (var-get max-ever-stored) new-value))
    (var-set sum-of-all-values (+ (var-get sum-of-all-values) new-value))
    (var-set total-updates (+ (var-get total-updates) u1))
    (var-set last-update-block block-height)
    (record-history new-value)
  )
)

;; Admin functions

;; Freeze/unfreeze the contract
(define-public (set-frozen (frozen bool))
  (begin
    (asserts! (is-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set is-frozen frozen)
    (print {event: "contract-frozen", frozen: frozen})
    (ok frozen)
  )
)

;; Set access control mode
(define-public (set-public-access (public bool))
  (begin
    (asserts! (is-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set public-access public)
    (print {event: "access-mode-changed", public: public})
    (ok public)
  )
)

;; Authorize/deauthorize users
(define-public (set-user-authorization (user principal) (authorized bool))
  (begin
    (asserts! (is-owner tx-sender) ERR-NOT-AUTHORIZED)
    (map-set authorized-users user authorized)
    (print {event: "user-authorization-changed", user: user, authorized: authorized})
    (ok authorized)
  )
)

;; Set cooldown period between updates
(define-public (set-cooldown-period (blocks uint))
  (begin
    (asserts! (is-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set cooldown-period blocks)
    (print {event: "cooldown-changed", blocks: blocks})
    (ok blocks)
  )
)

;; Emergency reset to middle of range
(define-public (emergency-reset)
  (begin
    (asserts! (is-owner tx-sender) ERR-NOT-AUTHORIZED)
    (let ((middle (/ (+ (var-get min-bound) (var-get max-bound)) u2))
        (old-value (var-get stored-value)))
      (var-set stored-value middle)
      (update-stats middle)
      (emit-value-changed old-value middle tx-sender)
      (ok middle)
    )
  )
)

;; Enhanced bounds management

;; Set new bounds with automatic value adjustment
(define-public (set-bounds (new-min uint) (new-max uint))
  (begin
    (asserts! (not (var-get is-frozen)) ERR-CONTRACT-FROZEN)
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (< new-min new-max) ERR-INVALID-BOUNDS)
    (try! (check-cooldown))
    
    (let ((old-min (var-get min-bound))
        (old-max (var-get max-bound))
        (current-value (var-get stored-value)))
      
      ;; Update bounds
      (var-set min-bound new-min)
      (var-set max-bound new-max)
      
      ;; Adjust current value if needed
      (let ((adjusted-value 
            (if (< current-value new-min)
              new-min
              (if (> current-value new-max)
                new-max
                current-value))))
        
        ;; Update value and stats if adjustment needed
        (if (not (is-eq adjusted-value current-value))
            (begin
              (var-set stored-value adjusted-value)
              (update-stats adjusted-value)
              (emit-value-changed current-value adjusted-value tx-sender)
              true)
            true)
        
        (emit-bounds-changed old-min old-max new-min new-max)
        (ok {min: new-min, max: new-max, adjusted-value: adjusted-value})
      )
    )
  )
)

;; Expand bounds by percentage
(define-public (expand-bounds-by-percent (percent uint))
  (begin
    (asserts! (<= percent u100) ERR-INVALID-PERCENTAGE)
    (let ((current-min (var-get min-bound))
        (current-max (var-get max-bound))
        (range (- current-max current-min))
        (expansion (/ (* range percent) u100)))
      (set-bounds 
        (if (>= current-min expansion) (- current-min expansion) u0)
        (+ current-max expansion)
      )
    )
  )
)

;; Enhanced value management

;; Set value with validation and logging
(define-public (set-value (new-value uint))
  (begin
    (asserts! (not (var-get is-frozen)) ERR-CONTRACT-FROZEN)
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (try! (check-cooldown))
    
    (let ((current-min (var-get min-bound))
        (current-max (var-get max-bound))
        (old-value (var-get stored-value)))
      
      (asserts! (>= new-value current-min) ERR-VALUE-TOO-LOW)
      (asserts! (<= new-value current-max) ERR-VALUE-TOO-HIGH)
      
      (var-set stored-value new-value)
      (update-stats new-value)
      (emit-value-changed old-value new-value tx-sender)
      (ok new-value)
    )
  )
)

;; Set value as percentage of range
(define-public (set-value-by-percentage (percentage uint))
  (begin
    (asserts! (<= percentage u100) ERR-INVALID-PERCENTAGE)
    (let ((min-val (var-get min-bound))
        (max-val (var-get max-bound))
        (range (- max-val min-val))
        (target-value (+ min-val (/ (* range percentage) u100))))
      (set-value target-value)
    )
  )
)

;; Increment with overflow protection
(define-public (safe-increment (amount uint))
  (let ((current-value (var-get stored-value))
      (max-val (var-get max-bound)))
    (if (<= (+ current-value amount) max-val)
      (set-value (+ current-value amount))
      (set-value max-val)
    )
  )
)

;; Decrement with underflow protection
(define-public (safe-decrement (amount uint))
  (let ((current-value (var-get stored-value))
      (min-val (var-get min-bound)))
    (if (>= current-value amount)
      (if (>= (- current-value amount) min-val)
        (set-value (- current-value amount))
        (set-value min-val))
      (set-value min-val)
    )
  )
)

;; Set to random value within bounds (using block height as entropy)
(define-public (set-random-value)
  (let ((min-val (var-get min-bound))
    (max-val (var-get max-bound))
    (range (- max-val min-val)))
    (if (is-eq range u0)
      (set-value min-val)
      (let ((random-seed (mod block-height u256))
          (random-value (+ min-val (mod random-seed (+ range u1)))))
        (set-value random-value)
      )
    )
  )
)

;; Batch operations

;; Set multiple values in sequence (for testing/demo)
(define-public (batch-set-values (values (list 10 uint)))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (try! (fold check-and-set-value values (ok (list))))
    (ok (len values))
  )
)

(define-private (check-and-set-value (value uint) (prev-result (response (list 10 uint) uint)))
  (match prev-result
    success (match (set-value value)
              ok (ok (unwrap-panic (as-max-len? (append success value) u10)))
              error (err error))
    error (err error)
  )
)

;; Enhanced read-only functions

;; Get comprehensive status
(define-read-only (get-full-status)
  {
    value: (var-get stored-value),
    bounds: {min: (var-get min-bound), max: (var-get max-bound)},
    stats: {
      total-updates: (var-get total-updates),
      min-ever: (var-get min-ever-stored),
      max-ever: (var-get max-ever-stored),
      average: (/ (var-get sum-of-all-values) (var-get total-updates))
    },
    system: {
      frozen: (var-get is-frozen),
      public-access: (var-get public-access),
      last-update-block: (var-get last-update-block),
      cooldown-period: (var-get cooldown-period),
      history-count: (var-get history-count)
    }
  }
)

;; Get value position as percentage of range
(define-read-only (get-value-percentage)
  (let ((current-value (var-get stored-value))
      (min-val (var-get min-bound))
      (max-val (var-get max-bound))
      (range (- max-val min-val)))
    (if (is-eq range u0)
      u0
      (/ (* (- current-value min-val) u100) range)
    )
  )
)

;; Check if user is authorized
(define-read-only (is-user-authorized (user principal))
  (is-authorized user)
)

;; Get historical value by index
(define-read-only (get-history-entry (index uint))
  (if (< index (var-get history-count))
    (ok (map-get? value-history index))
    ERR-INVALID-INDEX
  )
)

;; Get recent history (last N entries)
(define-read-only (get-recent-history (count uint))
  (let ((total-count (var-get history-count))
      (start-index (if (> total-count count) (- total-count count) u0)))
    (map get-history-by-index (list u0 u1 u2 u3 u4))
  )
)

(define-private (get-history-by-index (offset uint))
  (let ((total (var-get history-count)))
    (if (> total offset)
      (map-get? value-history (- total offset u1))
      none
    )
  )
)

;; Calculate distance from bounds
(define-read-only (get-distance-from-bounds)
  (let ((current (var-get stored-value))
      (min-val (var-get min-bound))
      (max-val (var-get max-bound)))
    {
      from-min: (- current min-val),
      from-max: (- max-val current),
      closest-bound: (if (< (- current min-val) (- max-val current)) "min" "max")
    }
  )
)

;; Validate a potential new bound setting
(define-read-only (validate-bounds (new-min uint) (new-max uint))
  {
    valid: (< new-min new-max),
    current-value-in-range: (and (>= (var-get stored-value) new-min)
                                (<= (var-get stored-value) new-max)),
    range-size: (if (< new-min new-max) (- new-max new-min) u0)
  }
)

;; Get statistics summary
(define-read-only (get-statistics)
  (let ((total (var-get total-updates)))
    {
      total-updates: total,
      average-value: (if (> total u0) (/ (var-get sum-of-all-values) total) u0),
      min-ever: (var-get min-ever-stored),
      max-ever: (var-get max-ever-stored),
      current-range: (- (var-get max-bound) (var-get min-bound)),
      utilization-percentage: (get-value-percentage)
    }
  )
)

;; Initialize the contract
(initialize)