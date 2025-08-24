;; user-registry
;; Manages user accounts and profile information for the EcoForge platform
;; This contract serves as the foundation for user identity in the ecosystem,
;; enabling personalized carbon tracking and targeted sustainability recommendations.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-ALREADY-EXISTS (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PRIVACY-SETTING (err u103))
(define-constant ERR-INVALID-USERNAME (err u104))
(define-constant ERR-INVALID-REGION (err u105))

;; Privacy setting constants
(define-constant PRIVACY-PUBLIC u1)
(define-constant PRIVACY-PRIVATE u2)
(define-constant PRIVACY-FRIENDS-ONLY u3)

;; Data structures
(define-map users 
  { user-id: principal }
  {
    username: (string-utf8 50),
    region: (string-utf8 30),
    sustainability-goal: (string-utf8 200),
    privacy-setting: uint,
    joined-at: uint,
    last-updated: uint
  }
)

(define-map username-index
  { username: (string-utf8 50) }
  { user-id: principal }
)

;; Admin controls
(define-data-var contract-owner principal tx-sender)

;; Private functions
(define-private (is-valid-privacy-setting (setting uint))
  (or
    (is-eq setting PRIVACY-PUBLIC)
    (is-eq setting PRIVACY-PRIVATE)
    (is-eq setting PRIVACY-FRIENDS-ONLY)
  )
)

(define-private (check-user-exists (user-id principal))
  (match (map-get? users { user-id: user-id })
    user true
    false
  )
)

(define-private (check-username-available (username (string-utf8 50)))
  (is-none (map-get? username-index { username: username }))
)

(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Read-only functions
(define-read-only (get-user (user-id principal))
  (match (map-get? users { user-id: user-id })
    user (ok user)
    (err ERR-USER-NOT-FOUND)
  )
)

(define-read-only (is-registered (user-id principal))
  (ok (check-user-exists user-id))
)

;; Public functions
(define-public (register-user 
    (username (string-utf8 50))
    (region (string-utf8 30))
    (sustainability-goal (string-utf8 200))
    (privacy-setting uint)
  )
  (let 
    (
      (user-id tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Validate inputs and user doesn't already exist
    (asserts! (not (check-user-exists user-id)) ERR-USER-ALREADY-EXISTS)
    (asserts! (> (len username) u0) ERR-INVALID-USERNAME)
    (asserts! (> (len region) u0) ERR-INVALID-REGION)
    (asserts! (is-valid-privacy-setting privacy-setting) ERR-INVALID-PRIVACY-SETTING)
    (asserts! (check-username-available username) ERR-USER-ALREADY-EXISTS)
    
    ;; Store user data
    (map-set users
      { user-id: user-id }
      {
        username: username,
        region: region,
        sustainability-goal: sustainability-goal,
        privacy-setting: privacy-setting,
        joined-at: current-time,
        last-updated: current-time
      }
    )
    
    ;; Create username index
    (map-set username-index
      { username: username }
      { user-id: user-id }
    )
    
    (ok true)
  )
)

(define-public (update-profile
    (username (string-utf8 50))
    (region (string-utf8 30))
    (sustainability-goal (string-utf8 200))
  )
  (let 
    (
      (user-id tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (existing-user (unwrap! (map-get? users { user-id: user-id }) ERR-USER-NOT-FOUND))
      (current-username (get username existing-user))
    )
    ;; Validate inputs
    (asserts! (> (len username) u0) ERR-INVALID-USERNAME)
    (asserts! (> (len region) u0) ERR-INVALID-REGION)
    
    ;; Check if username is changed and available
    (if (not (is-eq current-username username))
      (begin
        (asserts! (check-username-available username) ERR-USER-ALREADY-EXISTS)
        ;; Delete old username index
        (map-delete username-index { username: current-username })
        ;; Create new username index
        (map-set username-index
          { username: username }
          { user-id: user-id }
        )
      )
      true
    )
    
    ;; Update user data
    (map-set users
      { user-id: user-id }
      {
        username: username,
        region: region,
        sustainability-goal: sustainability-goal,
        privacy-setting: (get privacy-setting existing-user),
        joined-at: (get joined-at existing-user),
        last-updated: current-time
      }
    )
    
    (ok true)
  )
)

(define-public (update-privacy-setting (privacy-setting uint))
  (let 
    (
      (user-id tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (existing-user (unwrap! (map-get? users { user-id: user-id }) ERR-USER-NOT-FOUND))
    )
    ;; Validate privacy setting
    (asserts! (is-valid-privacy-setting privacy-setting) ERR-INVALID-PRIVACY-SETTING)
    
    ;; Update user data
    (map-set users
      { user-id: user-id }
      {
        username: (get username existing-user),
        region: (get region existing-user),
        sustainability-goal: (get sustainability-goal existing-user),
        privacy-setting: privacy-setting,
        joined-at: (get joined-at existing-user),
        last-updated: current-time
      }
    )
    
    (ok true)
  )
)

(define-public (delete-account)
  (let 
    (
      (user-id tx-sender)
      (existing-user (unwrap! (map-get? users { user-id: user-id }) ERR-USER-NOT-FOUND))
    )
    ;; Delete username index
    (map-delete username-index { username: (get username existing-user) })
    
    ;; Delete user data
    (map-delete users { user-id: user-id })
    
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)