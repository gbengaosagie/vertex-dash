;; VertexDash - Quadratic Voting DAO Governance
;; Simplified implementation for Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-proposal-closed (err u104))
(define-constant err-insufficient-reputation (err u105))
(define-constant err-invalid-amount (err u106))

;; Data Variables
(define-data-var proposal-count uint u0)
(define-data-var min-reputation-to-propose uint u100)
(define-data-var voting-period uint u1440) ;; ~10 days in blocks

;; Data Maps
(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    yes-votes: uint,
    no-votes: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    active: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { 
    vote-weight: uint,
    support: bool,
    block-height: uint
  }
)

(define-map member-reputation
  principal
  uint
)

(define-map vote-delegation
  principal
  principal
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-reputation (member principal))
  (default-to u0 (map-get? member-reputation member))
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (get-vote proposal-id voter))
)

(define-read-only (get-delegation (delegator principal))
  (map-get? vote-delegation delegator)
)

(define-read-only (calculate-quadratic-cost (vote-count uint))
  ;; Quadratic voting: cost = votes^2
  (* vote-count vote-count)
)

(define-read-only (get-proposal-status (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) (err err-not-found))))
    (ok {
      active: (get active proposal),
      executed: (get executed proposal),
      is-open: (and 
        (get active proposal)
        (< block-height (get end-block proposal))
        (not (get executed proposal))
      ),
      yes-votes: (get yes-votes proposal),
      no-votes: (get no-votes proposal),
      total-votes: (+ (get yes-votes proposal) (get no-votes proposal))
    })
  )
)

;; Governance weight with time decay
(define-read-only (calculate-voting-weight (voter principal) (vote-amount uint))
  (let (
    (reputation (get-reputation voter))
    (quadratic-cost (calculate-quadratic-cost vote-amount))
  )
    (if (>= reputation quadratic-cost)
      (ok vote-amount)
      (err err-insufficient-reputation)
    )
  )
)

;; Public functions
(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)))
  (let (
    (proposer-reputation (get-reputation tx-sender))
    (new-proposal-id (+ (var-get proposal-count) u1))
    (start-height block-height)
    (end-height (+ block-height (var-get voting-period)))
  )
    (asserts! (>= proposer-reputation (var-get min-reputation-to-propose)) err-insufficient-reputation)
    
    (map-set proposals new-proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      yes-votes: u0,
      no-votes: u0,
      start-block: start-height,
      end-block: end-height,
      executed: false,
      active: true
    })
    
    (var-set proposal-count new-proposal-id)
    (ok new-proposal-id)
  )
)

(define-public (cast-vote (proposal-id uint) (support bool) (vote-amount uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-not-found))
    (voter (default-to tx-sender (get-delegation tx-sender)))
    (voting-weight (unwrap! (calculate-voting-weight voter vote-amount) err-insufficient-reputation))
  )
    (asserts! (get active proposal) err-proposal-closed)
    (asserts! (< block-height (get end-block proposal)) err-proposal-closed)
    (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
    (asserts! (> vote-amount u0) err-invalid-amount)
    
    ;; Record the vote
    (map-set votes 
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote-weight: voting-weight,
        support: support,
        block-height: block-height
      }
    )
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id
      (merge proposal {
        yes-votes: (if support 
          (+ (get yes-votes proposal) voting-weight)
          (get yes-votes proposal)
        ),
        no-votes: (if support
          (get no-votes proposal)
          (+ (get no-votes proposal) voting-weight)
        )
      })
    )
    
    ;; Deduct reputation (quadratic cost)
    (map-set member-reputation voter
      (- (get-reputation voter) (calculate-quadratic-cost vote-amount))
    )
    
    (ok true)
  )
)

(define-public (delegate-vote (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) err-unauthorized)
    (map-set vote-delegation tx-sender delegate)
    (ok true)
  )
)

(define-public (revoke-delegation)
  (begin
    (map-delete vote-delegation tx-sender)
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get executed proposal)) err-proposal-closed)
    (asserts! (>= block-height (get end-block proposal)) err-proposal-closed)
    (asserts! (> (get yes-votes proposal) (get no-votes proposal)) err-unauthorized)
    
    (map-set proposals proposal-id
      (merge proposal { executed: true, active: false })
    )
    
    (ok true)
  )
)

(define-public (award-reputation (member principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set member-reputation member
      (+ (get-reputation member) amount)
    )
    (ok true)
  )
)

(define-public (close-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) err-not-found)))
    (asserts! (is-eq tx-sender (get proposer proposal)) err-unauthorized)
    (asserts! (not (get executed proposal)) err-proposal-closed)
    
    (map-set proposals proposal-id
      (merge proposal { active: false })
    )
    (ok true)
  )
)

;; Admin functions
(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (set-min-reputation (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-reputation-to-propose new-min)
    (ok true)
  )
)

;; Initialize contract
(begin
  (map-set member-reputation contract-owner u1000)
)