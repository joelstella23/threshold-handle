;; eco-rewards
;; A contract that issues and manages tokenized rewards for verified sustainable actions
;; Part of the EcoForge platform - A blockchain-powered carbon footprint tracking application

;; =========================================
;; Constants and Error Codes
;; =========================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-INVALID-ACTION (err u102))
(define-constant ERR-INSUFFICIENT-TOKENS (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-ACTION-ALREADY-VERIFIED (err u105))
(define-constant ERR-BADGE-ALREADY-AWARDED (err u106))
(define-constant ERR-CHALLENGE-NOT-ACTIVE (err u107))
(define-constant ERR-CHALLENGE-REQUIREMENTS-NOT-MET (err u108))
(define-constant ERR-INVALID-REDEMPTION (err u109))

;; Action types and their base reward values
(define-constant ACTION-PUBLIC-TRANSPORT u10)
(define-constant ACTION-RENEWABLE-ENERGY u15)
(define-constant ACTION-REDUCE-WASTE u8)
(define-constant ACTION-PLANT-TREE u20)
(define-constant ACTION-SUSTAINABLE-DIET u12)

;; Verification levels and their multipliers (in basis points, 100 = 1x)
(define-constant VERIFICATION-SELF u80)       ;; 0.8x for self-reported
(define-constant VERIFICATION-COMMUNITY u100) ;; 1x for community verified
(define-constant VERIFICATION-EXPERT u150)    ;; 1.5x for expert verified

;; Badge tier thresholds
(define-constant BADGE-TIER-BRONZE u500)
(define-constant BADGE-TIER-SILVER u2000)
(define-constant BADGE-TIER-GOLD u5000)
(define-constant BADGE-TIER-PLATINUM u10000)

;; =========================================
;; Data Maps and Variables
;; =========================================

;; Track user token balances
(define-map user-balances principal uint)

;; Track total actions performed by a user
(define-map user-action-counts {user: principal, action-type: uint} uint)

;; Record of verified actions
(define-map verified-actions {user: principal, action-id: (string-ascii 36)} 
  {
    action-type: uint,
    verification-level: uint,
    timestamp: uint,
    rewarded: bool,
    reward-amount: uint
  }
)

;; Track badges earned by users
(define-map user-badges {user: principal, badge-id: uint} bool)

;; Active challenges
(define-map active-challenges uint 
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    action-type: uint,
    required-count: uint,
    reward-amount: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool
  }
)

;; User progress in challenges
(define-map challenge-progress {user: principal, challenge-id: uint} uint)

;; Track completed challenges
(define-map completed-challenges {user: principal, challenge-id: uint} bool)

;; Redemption options
(define-map redemption-options uint 
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    token-cost: uint,
    available: bool
  }
)

;; Track total tokens issued
(define-data-var total-tokens-issued uint u0)

;; Track total tokens redeemed
(define-data-var total-tokens-redeemed uint u0)

;; Track next challenge ID
(define-data-var next-challenge-id uint u1)

;; Track next redemption option ID
(define-data-var next-redemption-id uint u1)

;; =========================================
;; Private Functions
;; =========================================

;; Get user balance or default to zero if not found
(define-private (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Get user action count or default to zero if not found
(define-private (get-action-count (user principal) (action-type uint))
  (default-to u0 (map-get? user-action-counts {user: user, action-type: action-type}))
)

;; Get challenge progress or default to zero if not found
(define-private (get-challenge-progress (user principal) (challenge-id uint))
  (default-to u0 (map-get? challenge-progress {user: user, challenge-id: challenge-id}))
)

;; Add tokens to a user's balance
(define-private (add-tokens (user principal) (amount uint))
  (let (
    (current-balance (get-balance user))
    (new-balance (+ current-balance amount))
  )
    (map-set user-balances user new-balance)
    (var-set total-tokens-issued (+ (var-get total-tokens-issued) amount))
    (ok new-balance)
  )
)

;; Check if a user has earned a new badge and award if eligible
(define-private (check-and-award-badges (user principal))
  (let (
    (user-balance (get-balance user))
  )
    (begin
      ;; Check for each badge tier and award if eligible
      (if (and (>= user-balance BADGE-TIER-BRONZE) (is-none (map-get? user-badges {user: user, badge-id: u1})))
        (map-set user-badges {user: user, badge-id: u1} true)
        true
      )
      (if (and (>= user-balance BADGE-TIER-SILVER) (is-none (map-get? user-badges {user: user, badge-id: u2})))
        (map-set user-badges {user: user, badge-id: u2} true)
        true
      )
      (if (and (>= user-balance BADGE-TIER-GOLD) (is-none (map-get? user-badges {user: user, badge-id: u3})))
        (map-set user-badges {user: user, badge-id: u3} true)
        true
      )
      (if (and (>= user-balance BADGE-TIER-PLATINUM) (is-none (map-get? user-badges {user: user, badge-id: u4})))
        (map-set user-badges {user: user, badge-id: u4} true)
        true
      )
      (ok true)
    )
  )
)

;; Update challenge progress for a user's action
(define-private (update-challenge-progress (user principal) (action-type uint))
  (let (
    (current-block-height block-height)
  )
    ;; Loop through challenges using fold - find active challenges matching the action type
    (ok true) ;; Placeholder - actual implementation would check each challenge
  )
)

;; =========================================
;; Read-Only Functions
;; =========================================

;; Get user token balance
(define-read-only (get-user-balance (user principal))
  (ok (get-balance user))
)

;; Get action details
(define-read-only (get-action-details (user principal) (action-id (string-ascii 36)))
  (map-get? verified-actions {user: user, action-id: action-id})
)

;; Check if user has a specific badge
(define-read-only (has-badge (user principal) (badge-id uint))
  (default-to false (map-get? user-badges {user: user, badge-id: badge-id}))
)

;; Get challenge details
(define-read-only (get-challenge (challenge-id uint))
  (map-get? active-challenges challenge-id)
)

;; Get user challenge progress
(define-read-only (get-user-challenge-progress (user principal) (challenge-id uint))
  (ok (get-challenge-progress user challenge-id))
)

;; Get redemption option details
(define-read-only (get-redemption-option (option-id uint))
  (map-get? redemption-options option-id)
)

;; Get total stats
(define-read-only (get-token-stats)
  {
    total-issued: (var-get total-tokens-issued),
    total-redeemed: (var-get total-tokens-redeemed)
  }
)

;; =========================================
;; Public Functions
;; =========================================

;; Create a new challenge
(define-public (create-challenge 
    (name (string-ascii 50)) 
    (description (string-ascii 200)) 
    (action-type uint) 
    (required-count uint) 
    (reward-amount uint) 
    (duration uint)
  )
  (let (
    (challenge-id (var-get next-challenge-id))
    (start-block block-height)
    (end-block (+ block-height duration))
  )
    ;; Only contract owner can create challenges
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Create the challenge
    (map-set active-challenges challenge-id
      {
        name: name,
        description: description,
        action-type: action-type,
        required-count: required-count,
        reward-amount: reward-amount,
        start-block: start-block,
        end-block: end-block,
        is-active: true
      }
    )
    
    ;; Increment challenge ID counter
    (var-set next-challenge-id (+ challenge-id u1))
    
    (ok challenge-id)
  )
)

;; Deactivate a challenge
(define-public (deactivate-challenge (challenge-id uint))
  (let (
    (challenge (map-get? active-challenges challenge-id))
  )
    ;; Only contract owner can deactivate challenges
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some challenge) ERR-INVALID-ACTION)
    
    ;; Set challenge to inactive
    (map-set active-challenges challenge-id
      (merge (unwrap-panic challenge) {is-active: false})
    )
    
    (ok true)
  )
)

;; Create a redemption option
(define-public (create-redemption-option 
    (name (string-ascii 50)) 
    (description (string-ascii 200)) 
    (token-cost uint)
  )
  (let (
    (option-id (var-get next-redemption-id))
  )
    ;; Only contract owner can create redemption options
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> token-cost u0) ERR-INVALID-AMOUNT)
    
    ;; Create the redemption option
    (map-set redemption-options option-id
      {
        name: name,
        description: description,
        token-cost: token-cost,
        available: true
      }
    )
    
    ;; Increment redemption option ID counter
    (var-set next-redemption-id (+ option-id u1))
    
    (ok option-id)
  )
)

;; Update redemption option availability
(define-public (update-redemption-availability (option-id uint) (available bool))
  (let (
    (option (map-get? redemption-options option-id))
  )
    ;; Only contract owner can update redemption options
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some option) ERR-INVALID-REDEMPTION)
    
    ;; Update availability
    (map-set redemption-options option-id
      (merge (unwrap-panic option) {available: available})
    )
    
    (ok true)
  )
)

;; Redeem tokens for a reward
(define-public (redeem-tokens (option-id uint))
  (let (
    (user tx-sender)
    (option (map-get? redemption-options option-id))
    (user-balance (get-balance user))
  )
    (asserts! (is-some option) ERR-INVALID-REDEMPTION)
    (asserts! (get available (unwrap-panic option)) ERR-INVALID-REDEMPTION)
    
    (let (
      (token-cost (get token-cost (unwrap-panic option)))
    )
      (asserts! (>= user-balance token-cost) ERR-INSUFFICIENT-TOKENS)
      
      ;; Subtract tokens from user balance
      (map-set user-balances user (- user-balance token-cost))
      
      ;; Update total redeemed tokens
      (var-set total-tokens-redeemed (+ (var-get total-tokens-redeemed) token-cost))
      
      (ok token-cost)
    )
  )
)