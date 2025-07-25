;; Enhanced Multi-Proposal Voting Smart Contract
;; Advanced voting system with multiple proposals, delegation, weighted voting, and governance features

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_CANDIDATE_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_CLOSED (err u104))
(define-constant ERR_PROPOSAL_EXISTS (err u105))
(define-constant ERR_VOTING_NOT_STARTED (err u106))
(define-constant ERR_INSUFFICIENT_STAKE (err u107))
(define-constant ERR_DELEGATION_EXISTS (err u108))
(define-constant ERR_SELF_DELEGATION (err u109))
(define-constant ERR_INVALID_TIMEFRAME (err u110))
(define-constant ERR_QUORUM_NOT_MET (err u111))
(define-constant ERR_ALREADY_FINALIZED (err u112))
(define-constant ERR_NOT_FINALIZED (err u113))
(define-constant ERR_ADMIN_ONLY (err u114))
(define-constant ERR_INVALID_VOTING_TYPE (err u115))

;; Contract owner and admins
(define-constant CONTRACT_OWNER tx-sender)
(define-map admins principal bool)

;; Initialize contract owner as admin
(map-set admins CONTRACT_OWNER true)

;; Voting types
(define-constant VOTING_TYPE_SIMPLE u1)
(define-constant VOTING_TYPE_WEIGHTED u2)
(define-constant VOTING_TYPE_QUADRATIC u3)

;; Data structures

;; Enhanced proposal information
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    is-active: bool,
    is-finalized: bool,
    total-votes: uint,
    total-weight: uint,
    created-by: principal,
    created-at: uint,
    start-time: uint,
    end-time: uint,
    min-stake-required: uint,
    quorum-required: uint,
    voting-type: uint,
    category: (string-ascii 50),
    winner-candidate-id: (optional uint)
  }
)

;; Enhanced candidates
(define-map candidates
  { proposal-id: uint, candidate-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint,
    vote-weight: uint,
    platform-url: (optional (string-ascii 200)),
    endorsements: uint
  }
)

;; Voter information and staking
(define-map voters
  { voter: principal }
  {
    stake-amount: uint,
    reputation-score: uint,
    total-votes-cast: uint,
    delegate: (optional principal)
  }
)

;; Enhanced voting records
(define-map voter-proposal-status
  { voter: principal, proposal-id: uint }
  { 
    has-voted: bool, 
    voted-for: uint,
    vote-weight: uint,
    voted-at: uint,
    is-delegated: bool
  }
)

;; Delegation system
(define-map delegations
  { delegator: principal, proposal-id: uint }
  { delegate: principal, weight: uint }
)

;; Vote delegation power
(define-map delegate-power
  { delegate: principal, proposal-id: uint }
  { total-delegated-weight: uint, delegator-count: uint }
)

;; Proposal categories
(define-map proposal-categories
  { category: (string-ascii 50) }
  { description: (string-ascii 200), proposal-count: uint }
)

;; Voting history and analytics
(define-map daily-voting-stats
  { date: uint }
  { total-votes: uint, unique-voters: uint, total-weight: uint }
)

;; Comments and discussions
(define-map proposal-comments
  { proposal-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-ascii 500),
    timestamp: uint,
    upvotes: uint,
    downvotes: uint
  }
)

;; Endorsements
(define-map candidate-endorsements
  { proposal-id: uint, candidate-id: uint, endorser: principal }
  { timestamp: uint, weight: uint }
)

;; Counters
(define-data-var next-proposal-id uint u1)
(define-map proposal-candidate-counter { proposal-id: uint } { next-candidate-id: uint })
(define-map proposal-comment-counter { proposal-id: uint } { next-comment-id: uint })

;; Contract settings
(define-data-var min-proposal-stake uint u1000)
(define-data-var default-voting-period uint u1440) ;; ~1 day in blocks
(define-data-var platform-fee-rate uint u25) ;; 0.25%

;; Helper functions

(define-private (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-private (get-current-time)
  block-height
)

(define-private (calculate-quadratic-weight (stake uint))
  (let ((sqrt-stake (pow stake u2)))
    (if (> sqrt-stake u0) sqrt-stake u1)
  )
)

(define-private (calculate-vote-weight (voter principal) (voting-type uint))
  (let 
    (
      (voter-info (default-to 
        { stake-amount: u1, reputation-score: u1, total-votes-cast: u0, delegate: none }
        (map-get? voters { voter: voter })
      ))
      (base-stake (get stake-amount voter-info))
      (reputation (get reputation-score voter-info))
    )
    (if (is-eq voting-type VOTING_TYPE_SIMPLE)
      u1
      (if (is-eq voting-type VOTING_TYPE_WEIGHTED)
        (+ base-stake reputation)
        (calculate-quadratic-weight base-stake) ;; Quadratic voting
      )
    )
  )
)

;; Administrative functions

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_ADMIN_ONLY)
    (map-set admins new-admin true)
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_ADMIN_ONLY)
    (asserts! (not (is-eq admin CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (map-delete admins admin)
    (ok true)
  )
)

(define-public (update-contract-settings (min-stake uint) (voting-period uint) (fee-rate uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_ADMIN_ONLY)
    (var-set min-proposal-stake min-stake)
    (var-set default-voting-period voting-period)
    (var-set platform-fee-rate fee-rate)
    (ok true)
  )
)

;; Staking and voter registration

(define-public (register-voter (stake-amount uint))
  (let 
    (
      (current-voter (map-get? voters { voter: tx-sender }))
    )
    (begin
      (asserts! (>= stake-amount (var-get min-proposal-stake)) ERR_INSUFFICIENT_STAKE)
      ;; In a real implementation, this would transfer tokens to the contract
      (map-set voters
        { voter: tx-sender }
        {
          stake-amount: stake-amount,
          reputation-score: (match current-voter existing-voter (get reputation-score existing-voter) u1),
          total-votes-cast: (match current-voter existing-voter (get total-votes-cast existing-voter) u0),
          delegate: none
        }
      )
      (ok true)
    )
  )
)

(define-public (increase-stake (additional-stake uint))
  (let 
    (
      (voter-info (unwrap! (map-get? voters { voter: tx-sender }) ERR_UNAUTHORIZED))
    )
    (begin
      (map-set voters
        { voter: tx-sender }
        (merge voter-info { stake-amount: (+ (get stake-amount voter-info) additional-stake) })
      )
      (ok true)
    )
  )
)

;; Enhanced proposal creation

(define-public (create-proposal 
  (title (string-ascii 100)) 
  (description (string-ascii 500))
  (voting-duration uint)
  (min-stake uint)
  (quorum uint)
  (voting-type uint)
  (category (string-ascii 50))
)
  (let 
    (
      (proposal-id (var-get next-proposal-id))
      (current-time (get-current-time))
      (voter-info (unwrap! (map-get? voters { voter: tx-sender }) ERR_UNAUTHORIZED))
    )
    (begin
      ;; Validate inputs
      (asserts! (>= (get stake-amount voter-info) (var-get min-proposal-stake)) ERR_INSUFFICIENT_STAKE)
      (asserts! (> voting-duration u0) ERR_INVALID_TIMEFRAME)
      (asserts! (<= voting-type VOTING_TYPE_QUADRATIC) ERR_INVALID_VOTING_TYPE)
      
      ;; Create the proposal
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description: description,
          is-active: false,
          is-finalized: false,
          total-votes: u0,
          total-weight: u0,
          created-by: tx-sender,
          created-at: current-time,
          start-time: (+ current-time u10), ;; Start in 10 blocks
          end-time: (+ current-time voting-duration),
          min-stake-required: min-stake,
          quorum-required: quorum,
          voting-type: voting-type,
          category: category,
          winner-candidate-id: none
        }
      )
      
      ;; Initialize counters
      (map-set proposal-candidate-counter { proposal-id: proposal-id } { next-candidate-id: u1 })
      (map-set proposal-comment-counter { proposal-id: proposal-id } { next-comment-id: u1 })
      
      ;; Update category stats
      (let ((cat-info (map-get? proposal-categories { category: category })))
        (map-set proposal-categories
          { category: category }
          {
            description: "User-defined category",
            proposal-count: (+ (match cat-info existing (get proposal-count existing) u0) u1)
          }
        )
      )
      
      ;; Increment proposal counter
      (var-set next-proposal-id (+ proposal-id u1))
      (ok proposal-id)
    )
  )
)

;; Enhanced candidate management

(define-public (add-candidate 
  (proposal-id uint) 
  (name (string-ascii 50)) 
  (description (string-ascii 200))
  (platform-url (optional (string-ascii 200)))
)
  (let
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (candidate-id (default-to u1 (get next-candidate-id (map-get? proposal-candidate-counter { proposal-id: proposal-id }))))
    )
    (begin
      ;; Only proposal creator or admins can add candidates
      (asserts! (or (is-eq tx-sender (get created-by proposal-info)) (is-admin tx-sender)) ERR_UNAUTHORIZED)
      ;; Cannot add candidates to active proposals
      (asserts! (not (get is-active proposal-info)) ERR_VOTING_CLOSED)
      
      ;; Add the candidate
      (map-set candidates
        { proposal-id: proposal-id, candidate-id: candidate-id }
        {
          name: name,
          description: description,
          vote-count: u0,
          vote-weight: u0,
          platform-url: platform-url,
          endorsements: u0
        }
      )
      
      ;; Update candidate counter
      (map-set proposal-candidate-counter
        { proposal-id: proposal-id }
        { next-candidate-id: (+ candidate-id u1) }
      )
      (ok candidate-id)
    )
  )
)

;; Proposal lifecycle management

(define-public (activate-proposal (proposal-id uint))
  (let 
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (current-time (get-current-time))
    )
    (begin
      ;; Only proposal creator or admins can activate
      (asserts! (or (is-eq tx-sender (get created-by proposal-info)) (is-admin tx-sender)) ERR_UNAUTHORIZED)
      (asserts! (>= current-time (get start-time proposal-info)) ERR_VOTING_NOT_STARTED)
      (asserts! (< current-time (get end-time proposal-info)) ERR_VOTING_CLOSED)
      
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal-info { is-active: true })
      )
      (ok true)
    )
  )
)

;; Delegation system

(define-public (delegate-vote (proposal-id uint) (delegate principal))
  (let 
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (voter-info (unwrap! (map-get? voters { voter: tx-sender }) ERR_UNAUTHORIZED))
      (voter-weight (calculate-vote-weight tx-sender (get voting-type proposal-info)))
    )
    (begin
      ;; Validation checks
      (asserts! (not (is-eq tx-sender delegate)) ERR_SELF_DELEGATION)
      (asserts! (get is-active proposal-info) ERR_VOTING_CLOSED)
      (asserts! (is-none (map-get? voter-proposal-status { voter: tx-sender, proposal-id: proposal-id })) ERR_ALREADY_VOTED)
      
      ;; Record delegation
      (map-set delegations
        { delegator: tx-sender, proposal-id: proposal-id }
        { delegate: delegate, weight: voter-weight }
      )
      
      ;; Update delegate power
      (let ((current-power (map-get? delegate-power { delegate: delegate, proposal-id: proposal-id })))
        (map-set delegate-power
          { delegate: delegate, proposal-id: proposal-id }
          {
            total-delegated-weight: (+ voter-weight (match current-power existing (get total-delegated-weight existing) u0)),
            delegator-count: (+ u1 (match current-power existing (get delegator-count existing) u0))
          }
        )
      )
      
      (ok true)
    )
  )
)

;; Enhanced voting function

(define-public (vote (proposal-id uint) (candidate-id uint))
  (let
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (candidate-info (unwrap! (map-get? candidates { proposal-id: proposal-id, candidate-id: candidate-id }) ERR_CANDIDATE_NOT_FOUND))
      (voter-info (unwrap! (map-get? voters { voter: tx-sender }) ERR_UNAUTHORIZED))
      (current-time (get-current-time))
      (vote-weight (+ 
        (calculate-vote-weight tx-sender (get voting-type proposal-info))
        (match (map-get? delegate-power { delegate: tx-sender, proposal-id: proposal-id })
          power-info (get total-delegated-weight power-info)
          u0
        )
      ))
    )
    (begin
      ;; Validation checks
      (asserts! (get is-active proposal-info) ERR_VOTING_CLOSED)
      (asserts! (< current-time (get end-time proposal-info)) ERR_VOTING_CLOSED)
      (asserts! (>= (get stake-amount voter-info) (get min-stake-required proposal-info)) ERR_INSUFFICIENT_STAKE)
      (asserts! (is-none (map-get? voter-proposal-status { voter: tx-sender, proposal-id: proposal-id })) ERR_ALREADY_VOTED)
      
      ;; Record the vote
      (map-set voter-proposal-status
        { voter: tx-sender, proposal-id: proposal-id }
        { 
          has-voted: true, 
          voted-for: candidate-id,
          vote-weight: vote-weight,
          voted-at: current-time,
          is-delegated: false
        }
      )
      
      ;; Update candidate metrics
      (map-set candidates
        { proposal-id: proposal-id, candidate-id: candidate-id }
        (merge candidate-info { 
          vote-count: (+ (get vote-count candidate-info) u1),
          vote-weight: (+ (get vote-weight candidate-info) vote-weight)
        })
      )
      
      ;; Update proposal metrics
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal-info { 
          total-votes: (+ (get total-votes proposal-info) u1),
          total-weight: (+ (get total-weight proposal-info) vote-weight)
        })
      )
      
      ;; Update voter stats
      (map-set voters
        { voter: tx-sender }
        (merge voter-info { 
          total-votes-cast: (+ (get total-votes-cast voter-info) u1),
          reputation-score: (+ (get reputation-score voter-info) u1)
        })
      )
      
      (ok true)
    )
  )
)

;; Comments and discussion

(define-public (add-comment (proposal-id uint) (content (string-ascii 500)))
  (let 
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (comment-id (default-to u1 (get next-comment-id (map-get? proposal-comment-counter { proposal-id: proposal-id }))))
    )
    (begin
      (map-set proposal-comments
        { proposal-id: proposal-id, comment-id: comment-id }
        {
          author: tx-sender,
          content: content,
          timestamp: (get-current-time),
          upvotes: u0,
          downvotes: u0
        }
      )
      
      (map-set proposal-comment-counter
        { proposal-id: proposal-id }
        { next-comment-id: (+ comment-id u1) }
      )
      
      (ok comment-id)
    )
  )
)

;; Endorsement system

(define-public (endorse-candidate (proposal-id uint) (candidate-id uint))
  (let 
    (
      (voter-info (unwrap! (map-get? voters { voter: tx-sender }) ERR_UNAUTHORIZED))
      (candidate-info (unwrap! (map-get? candidates { proposal-id: proposal-id, candidate-id: candidate-id }) ERR_CANDIDATE_NOT_FOUND))
      (endorsement-weight (get reputation-score voter-info))
    )
    (begin
      (map-set candidate-endorsements
        { proposal-id: proposal-id, candidate-id: candidate-id, endorser: tx-sender }
        { timestamp: (get-current-time), weight: endorsement-weight }
      )
      
      (map-set candidates
        { proposal-id: proposal-id, candidate-id: candidate-id }
        (merge candidate-info { endorsements: (+ (get endorsements candidate-info) endorsement-weight) })
      )
      
      (ok true)
    )
  )
)

;; Proposal finalization and results

(define-public (finalize-proposal (proposal-id uint))
  (let 
    (
      (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (current-time (get-current-time))
    )
    (begin
      ;; Check if proposal can be finalized
      (asserts! (>= current-time (get end-time proposal-info)) ERR_VOTING_CLOSED)
      (asserts! (not (get is-finalized proposal-info)) ERR_ALREADY_FINALIZED)
      (asserts! (>= (get total-weight proposal-info) (get quorum-required proposal-info)) ERR_QUORUM_NOT_MET)
      
      ;; Find winner (simplified - gets candidate with most weight)
      (let ((winner-id (get-winning-candidate proposal-id)))
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal-info { 
            is-active: false,
            is-finalized: true,
            winner-candidate-id: winner-id
          })
        )
      )
      
      (ok true)
    )
  )
)

;; Helper function to find winning candidate (simplified implementation)
(define-private (get-winning-candidate (proposal-id uint))
  ;; This is a simplified version - in practice, you'd iterate through all candidates
  ;; For now, returns the first candidate (candidate-id u1)
  (some u1)
)

;; Enhanced read-only functions

(define-read-only (get-proposal-full (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-candidate-full (proposal-id uint) (candidate-id uint))
  (map-get? candidates { proposal-id: proposal-id, candidate-id: candidate-id })
)

(define-read-only (get-voter-info (voter principal))
  (map-get? voters { voter: voter })
)

(define-read-only (get-voting-power (voter principal) (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal-info (calculate-vote-weight voter (get voting-type proposal-info))
    u0
  )
)

(define-read-only (get-delegation-info (delegator principal) (proposal-id uint))
  (map-get? delegations { delegator: delegator, proposal-id: proposal-id })
)

(define-read-only (get-delegate-power-info (delegate principal) (proposal-id uint))
  (map-get? delegate-power { delegate: delegate, proposal-id: proposal-id })
)

(define-read-only (get-proposal-results (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal-info (some {
      total-votes: (get total-votes proposal-info),
      total-weight: (get total-weight proposal-info),
      is-finalized: (get is-finalized proposal-info),
      winner: (get winner-candidate-id proposal-info),
      quorum-met: (>= (get total-weight proposal-info) (get quorum-required proposal-info))
    })
    none
  )
)

(define-read-only (get-comment (proposal-id uint) (comment-id uint))
  (map-get? proposal-comments { proposal-id: proposal-id, comment-id: comment-id })
)

(define-read-only (get-endorsement (proposal-id uint) (candidate-id uint) (endorser principal))
  (map-get? candidate-endorsements { proposal-id: proposal-id, candidate-id: candidate-id, endorser: endorser })
)

(define-read-only (get-contract-stats)
  {
    total-proposals: (- (var-get next-proposal-id) u1),
    min-stake: (var-get min-proposal-stake),
    default-period: (var-get default-voting-period),
    platform-fee: (var-get platform-fee-rate)
  }
)

;; Category management
(define-read-only (get-category-info (category (string-ascii 50)))
  (map-get? proposal-categories { category: category })
)