;; verification-system
;; 
;; This contract implements a multi-level verification system to ensure the integrity of
;; reported sustainable actions. It supports different verification methods including
;; self-attestation, photo/video evidence, community validation, and integration with
;; third-party IoT devices or services. Verified activities receive a confidence score
;; that influences reward calculations. The verification process is transparent yet
;; privacy-preserving, maintaining trust in the platform while protecting user data.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-ACTIVITY-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-VERIFICATION-TYPE (err u1002))
(define-constant ERR-ALREADY-VERIFIED (err u1003))
(define-constant ERR-INVALID-CONFIDENCE-SCORE (err u1004))
(define-constant ERR-VALIDATOR-NOT-REGISTERED (err u1005))
(define-constant ERR-SELF-VERIFICATION-NOT-ALLOWED (err u1006))
(define-constant ERR-VERIFICATION-EXPIRED (err u1007))
(define-constant ERR-INSUFFICIENT-VALIDATORS (err u1008))
(define-constant ERR-INVALID-EVIDENCE-HASH (err u1009))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Verification types
(define-constant VERIFICATION-TYPE-SELF-ATTESTATION u1)
(define-constant VERIFICATION-TYPE-PHOTO-EVIDENCE u2)
(define-constant VERIFICATION-TYPE-VIDEO-EVIDENCE u3)
(define-constant VERIFICATION-TYPE-COMMUNITY-VALIDATION u4)
(define-constant VERIFICATION-TYPE-IOT-DEVICE u5)
(define-constant VERIFICATION-TYPE-THIRD-PARTY-SERVICE u6)

;; Minimum number of validators required for community validation
(define-constant MIN-VALIDATORS u3)

;; Data structures
;; Activity map: Maps activity IDs to their details
(define-map activities
  { activity-id: uint }
  {
    reporter: principal,
    activity-type: uint,
    timestamp: uint,
    location: (optional (tuple (latitude int) (longitude int))),
    description: (string-utf8 500),
    status: uint,  ;; 0: pending, 1: verified, 2: rejected
    verification-type: uint,
    confidence-score: uint, ;; 0-100
    evidence-hash: (optional (buff 32))
  }
)

;; Verification requests
(define-map verification-requests
  { activity-id: uint }
  {
    verification-type: uint,
    submitted-at: uint,
    expires-at: uint,
    evidence-hash: (optional (buff 32)), 
    required-validators: uint,
    current-validators: uint
  }
)

;; Validators map: Maps principal to validator info
(define-map validators
  { validator: principal }
  {
    trust-score: uint, ;; 0-100
    validations-performed: uint,
    registration-time: uint,
    is-active: bool
  }
)

;; Validations map: Tracks individual validations for each activity
(define-map validations
  { activity-id: uint, validator: principal }
  {
    validation-time: uint,
    approved: bool,
    notes: (optional (string-utf8 200))
  }
)

;; Data variables
(define-data-var next-activity-id uint u1)
(define-data-var admin principal CONTRACT-OWNER)

;; Private functions
(define-private (is-authorized)
  (is-eq tx-sender (var-get admin))
)

(define-private (is-valid-verification-type (verification-type uint))
  (or
    (is-eq verification-type VERIFICATION-TYPE-SELF-ATTESTATION)
    (is-eq verification-type VERIFICATION-TYPE-PHOTO-EVIDENCE)
    (is-eq verification-type VERIFICATION-TYPE-VIDEO-EVIDENCE)
    (is-eq verification-type VERIFICATION-TYPE-COMMUNITY-VALIDATION)
    (is-eq verification-type VERIFICATION-TYPE-IOT-DEVICE)
    (is-eq verification-type VERIFICATION-TYPE-THIRD-PARTY-SERVICE)
  )
)

(define-private (is-valid-confidence-score (score uint))
  (<= score u100)
)

(define-private (is-registered-validator (validator principal))
  (default-to 
    false
    (get is-active (map-get? validators { validator: validator }))
  )
)

;; Read-only functions
(define-read-only (get-activity (activity-id uint))
  (ok (map-get? activities { activity-id: activity-id }))
)

(define-read-only (get-verification-request (activity-id uint))
  (ok (map-get? verification-requests { activity-id: activity-id }))
)

(define-read-only (get-validator-info (validator principal))
  (ok (map-get? validators { validator: validator }))
)

(define-read-only (get-validation (activity-id uint) (validator principal))
  (ok (map-get? validations { activity-id: activity-id, validator: validator }))
)

(define-read-only (is-verification-expired (activity-id uint))
  (match (map-get? verification-requests { activity-id: activity-id })
    request (> (unwrap-panic (get-block-info? time (- block-height u1))) (get expires-at request))
    false
  )
)

;; Public functions
;; Admin functions
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (ok (var-set admin new-admin))
  )
)

(define-public (register-validator (validator principal) (initial-trust-score uint))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-confidence-score initial-trust-score) ERR-INVALID-CONFIDENCE-SCORE)
    (ok (map-set validators 
      { validator: validator }
      {
        trust-score: initial-trust-score,
        validations-performed: u0,
        registration-time: (unwrap-panic (get-block-info? time (- block-height u1))),
        is-active: true
      }
    ))
  )
)

(define-public (deactivate-validator (validator principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (match (map-get? validators { validator: validator })
      validator-info
        (ok (map-set validators 
          { validator: validator }
          (merge validator-info { is-active: false })
        ))
      ERR-VALIDATOR-NOT-REGISTERED
    )
  )
)

;; User functions
;; Submit a new activity for verification
(define-public (submit-activity 
  (activity-type uint) 
  (description (string-utf8 500)) 
  (verification-type uint)
  (evidence-hash (optional (buff 32)))
  (location (optional (tuple (latitude int) (longitude int))))
)
  (let
    (
      (activity-id (var-get next-activity-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (required-validators (if (is-eq verification-type VERIFICATION-TYPE-COMMUNITY-VALIDATION) 
                              MIN-VALIDATORS
                              u0))
    )
    ;; Check that verification type is valid
    (asserts! (is-valid-verification-type verification-type) ERR-INVALID-VERIFICATION-TYPE)
    
    ;; Additional checks for specific verification types
    (asserts! (or 
                (not (is-eq verification-type VERIFICATION-TYPE-PHOTO-EVIDENCE))
                (not (is-eq verification-type VERIFICATION-TYPE-VIDEO-EVIDENCE))
                (is-some evidence-hash)
              ) 
              ERR-INVALID-EVIDENCE-HASH)
    
    ;; Create activity
    (map-set activities
      { activity-id: activity-id }
      {
        reporter: tx-sender,
        activity-type: activity-type,
        timestamp: current-time,
        location: location,
        description: description,
        status: u0, ;; pending
        verification-type: verification-type,
        confidence-score: u0, ;; will be calculated after verification
        evidence-hash: evidence-hash
      }
    )
    
    ;; Create verification request
    (map-set verification-requests
      { activity-id: activity-id }
      {
        verification-type: verification-type,
        submitted-at: current-time,
        expires-at: (+ current-time u604800), ;; Expires in 1 week
        evidence-hash: evidence-hash,
        required-validators: required-validators,
        current-validators: u0
      }
    )
    
    ;; If self-attestation, verify immediately
    (if (is-eq verification-type VERIFICATION-TYPE-SELF-ATTESTATION)
      (unwrap-panic (verify-self-attestation activity-id))
      true
    )
    
    ;; Increment activity ID counter
    (var-set next-activity-id (+ activity-id u1))
    
    (ok activity-id)
  )
)

;; Self-attestation verification
(define-private (verify-self-attestation (activity-id uint))
  (match (map-get? activities { activity-id: activity-id })
    activity (begin
      (asserts! (is-eq (get verification-type activity) VERIFICATION-TYPE-SELF-ATTESTATION) ERR-INVALID-VERIFICATION-TYPE)
      
      ;; Update activity with verification details
      (map-set activities
        { activity-id: activity-id }
        (merge activity {
          status: u1, ;; verified
          confidence-score: u30 ;; Self-attestation has a fixed confidence score
        })
      )
      (ok true)
    )
    ERR-ACTIVITY-NOT-FOUND
  )
)

;; Submit IoT or third-party verification
(define-public (submit-external-verification 
  (activity-id uint) 
  (verification-type uint) 
  (verification-data (buff 512))
)
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq verification-type VERIFICATION-TYPE-IOT-DEVICE)
                 (is-eq verification-type VERIFICATION-TYPE-THIRD-PARTY-SERVICE))
              ERR-INVALID-VERIFICATION-TYPE)
    
    (match (map-get? activities { activity-id: activity-id })
      activity (begin
        ;; Check that the activity is pending verification
        (asserts! (is-eq (get status activity) u0) ERR-ALREADY-VERIFIED)
        
        ;; Check that verification type matches
        (asserts! (is-eq (get verification-type activity) verification-type) 
                  ERR-INVALID-VERIFICATION-TYPE)
        
        ;; Update activity with verification details
        (map-set activities
          { activity-id: activity-id }
          (merge activity {
            status: u1, ;; verified
            confidence-score: (if (is-eq verification-type VERIFICATION-TYPE-IOT-DEVICE)
                                u90 ;; IoT device verification has higher confidence
                                u80) ;; Third-party service verification
          })
        )
        
        (ok true)
      )
      ERR-ACTIVITY-NOT-FOUND
    )
  )
)

;; Submit photo/video evidence verification result
(define-public (verify-media-evidence (activity-id uint) (approved bool))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    
    (match (map-get? activities { activity-id: activity-id })
      activity (begin
        ;; Check that the activity is pending verification
        (asserts! (is-eq (get status activity) u0) ERR-ALREADY-VERIFIED)
        
        ;; Check that verification type is photo or video
        (asserts! (or (is-eq (get verification-type activity) VERIFICATION-TYPE-PHOTO-EVIDENCE)
                      (is-eq (get verification-type activity) VERIFICATION-TYPE-VIDEO-EVIDENCE))
                  ERR-INVALID-VERIFICATION-TYPE)
        
        ;; Update activity with verification status
        (map-set activities
          { activity-id: activity-id }
          (merge activity {
            status: (if approved u1 u2), ;; 1: verified, 2: rejected
            confidence-score: (if approved
                               (if (is-eq (get verification-type activity) VERIFICATION-TYPE-PHOTO-EVIDENCE)
                                 u50 ;; Photo evidence
                                 u60) ;; Video evidence
                               u0)
          })
        )
        
        (ok true)
      )
      ERR-ACTIVITY-NOT-FOUND
    )
  )
)

;; Reject an activity verification
(define-public (reject-verification (activity-id uint) (reason (string-utf8 200)))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    
    (match (map-get? activities { activity-id: activity-id })
      activity (begin
        ;; Check that the activity is pending verification
        (asserts! (is-eq (get status activity) u0) ERR-ALREADY-VERIFIED)
        
        ;; Update activity with rejected status
        (map-set activities
          { activity-id: activity-id }
          (merge activity {
            status: u2, ;; rejected
            confidence-score: u0
          })
        )
        
        (ok true)
      )
      ERR-ACTIVITY-NOT-FOUND
    )
  )
)