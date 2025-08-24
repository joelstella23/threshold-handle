;; carbon-tracker.clar
;; This contract enables users to track their carbon footprint by recording various
;; activities that impact environmental sustainability. It calculates carbon emissions,
;; tracks historical data, and provides users with insights for reducing their impact.

;; =========================================
;; Constants and Error Codes
;; =========================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-ACTIVITY (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-GOAL-ALREADY-SET (err u103))
(define-constant ERR-NO-GOAL-EXISTS (err u104))
(define-constant ERR-INVALID-PARAMS (err u105))

;; Activity types
(define-constant ACTIVITY-TRANSPORT u1)
(define-constant ACTIVITY-ENERGY u2)
(define-constant ACTIVITY-DIET u3)
(define-constant ACTIVITY-CONSUMPTION u4)

;; Transport subtypes
(define-constant TRANSPORT-CAR u1)
(define-constant TRANSPORT-BUS u2)
(define-constant TRANSPORT-TRAIN u3)
(define-constant TRANSPORT-PLANE u4)
(define-constant TRANSPORT-BIKE u5)
(define-constant TRANSPORT-WALK u6)

;; Energy subtypes
(define-constant ENERGY-ELECTRICITY u1)
(define-constant ENERGY-GAS u2)
(define-constant ENERGY-HEATING-OIL u3)
(define-constant ENERGY-RENEWABLE u4)

;; Diet subtypes
(define-constant DIET-MEAT-BEEF u1)
(define-constant DIET-MEAT-OTHER u2)
(define-constant DIET-FISH u3)
(define-constant DIET-VEGETARIAN u4)
(define-constant DIET-VEGAN u5)

;; Consumption subtypes
(define-constant CONSUMPTION-CLOTHES u1)
(define-constant CONSUMPTION-ELECTRONICS u2)
(define-constant CONSUMPTION-HOUSEHOLD u3)
(define-constant CONSUMPTION-RECYCLED u4)

;; Carbon emission factors (in grams of CO2 equivalent per unit)
;; Transport: grams per kilometer
(define-constant CARBON-TRANSPORT-CAR u192)
(define-constant CARBON-TRANSPORT-BUS u105)
(define-constant CARBON-TRANSPORT-TRAIN u41)
(define-constant CARBON-TRANSPORT-PLANE u255)
(define-constant CARBON-TRANSPORT-BIKE u0)
(define-constant CARBON-TRANSPORT-WALK u0)

;; Energy: grams per kWh
(define-constant CARBON-ENERGY-ELECTRICITY u475)
(define-constant CARBON-ENERGY-GAS u198)
(define-constant CARBON-ENERGY-HEATING-OIL u266)
(define-constant CARBON-ENERGY-RENEWABLE u25)

;; Diet: grams per meal
(define-constant CARBON-DIET-MEAT-BEEF u6000)
(define-constant CARBON-DIET-MEAT-OTHER u1500)
(define-constant CARBON-DIET-FISH u1000)
(define-constant CARBON-DIET-VEGETARIAN u500)
(define-constant CARBON-DIET-VEGAN u300)

;; Consumption: grams per item
(define-constant CARBON-CONSUMPTION-CLOTHES u10000)
(define-constant CARBON-CONSUMPTION-ELECTRONICS u100000)
(define-constant CARBON-CONSUMPTION-HOUSEHOLD u5000)
(define-constant CARBON-CONSUMPTION-RECYCLED u1000)

;; Global averages (yearly in kilograms)
(define-constant GLOBAL-AVG-CARBON-FOOTPRINT u5000)
(define-constant NORTH-AMERICA-AVG-CARBON-FOOTPRINT u16000)
(define-constant EUROPE-AVG-CARBON-FOOTPRINT u7500)
(define-constant ASIA-AVG-CARBON-FOOTPRINT u4000)
(define-constant AFRICA-AVG-CARBON-FOOTPRINT u1000)

;; =========================================
;; Data Maps and Variables
;; =========================================

;; Stores the total carbon footprint for each user (in grams of CO2e)
(define-map user-carbon-totals 
  { user: principal }
  { total-emissions: uint }
)

;; Stores user activity records
(define-map activity-records
  { user: principal, timestamp: uint, activity-id: uint }
  { 
    activity-type: uint,
    activity-subtype: uint,
    amount: uint,
    carbon-value: uint
  }
)

;; Tracks user activity IDs (auto-incrementing)
(define-map user-activity-count
  { user: principal }
  { count: uint }
)

;; Stores monthly emissions for users
(define-map monthly-emissions
  { user: principal, year: uint, month: uint }
  { total: uint }
)

;; User reduction goals
(define-map user-goals
  { user: principal }
  { 
    target-emissions: uint,
    target-date: uint,
    created-at: uint,
    base-emissions: uint
  }
)

;; =========================================
;; Private Functions
;; =========================================

;; Get carbon factor for a given activity type and subtype
(define-private (get-carbon-factor (activity-type uint) (activity-subtype uint))
  (if (is-eq activity-type ACTIVITY-TRANSPORT)
    (if (is-eq activity-subtype TRANSPORT-CAR) 
      CARBON-TRANSPORT-CAR
      (if (is-eq activity-subtype TRANSPORT-BUS) 
        CARBON-TRANSPORT-BUS
        (if (is-eq activity-subtype TRANSPORT-TRAIN) 
          CARBON-TRANSPORT-TRAIN
          (if (is-eq activity-subtype TRANSPORT-PLANE) 
            CARBON-TRANSPORT-PLANE
            (if (is-eq activity-subtype TRANSPORT-BIKE) 
              CARBON-TRANSPORT-BIKE
              (if (is-eq activity-subtype TRANSPORT-WALK) 
                CARBON-TRANSPORT-WALK
                u0))))))
    (if (is-eq activity-type ACTIVITY-ENERGY)
      (if (is-eq activity-subtype ENERGY-ELECTRICITY) 
        CARBON-ENERGY-ELECTRICITY
        (if (is-eq activity-subtype ENERGY-GAS) 
          CARBON-ENERGY-GAS
          (if (is-eq activity-subtype ENERGY-HEATING-OIL) 
            CARBON-ENERGY-HEATING-OIL
            (if (is-eq activity-subtype ENERGY-RENEWABLE) 
              CARBON-ENERGY-RENEWABLE
              u0))))
      (if (is-eq activity-type ACTIVITY-DIET)
        (if (is-eq activity-subtype DIET-MEAT-BEEF) 
          CARBON-DIET-MEAT-BEEF
          (if (is-eq activity-subtype DIET-MEAT-OTHER) 
            CARBON-DIET-MEAT-OTHER
            (if (is-eq activity-subtype DIET-FISH) 
              CARBON-DIET-FISH
              (if (is-eq activity-subtype DIET-VEGETARIAN) 
                CARBON-DIET-VEGETARIAN
                (if (is-eq activity-subtype DIET-VEGAN) 
                  CARBON-DIET-VEGAN
                  u0)))))
        (if (is-eq activity-type ACTIVITY-CONSUMPTION)
          (if (is-eq activity-subtype CONSUMPTION-CLOTHES) 
            CARBON-CONSUMPTION-CLOTHES
            (if (is-eq activity-subtype CONSUMPTION-ELECTRONICS) 
              CARBON-CONSUMPTION-ELECTRONICS
              (if (is-eq activity-subtype CONSUMPTION-HOUSEHOLD) 
                CARBON-CONSUMPTION-HOUSEHOLD
                (if (is-eq activity-subtype CONSUMPTION-RECYCLED) 
                  CARBON-CONSUMPTION-RECYCLED
                  u0))))
          u0))))
)

;; Calculate carbon value based on activity type, subtype and amount
(define-private (calculate-carbon-value (activity-type uint) (activity-subtype uint) (amount uint))
  (let ((carbon-factor (get-carbon-factor activity-type activity-subtype)))
    (* carbon-factor amount)
  )
)

;; Get next activity ID for a user
(define-private (get-next-activity-id (user principal))
  (let ((current-count (default-to { count: u0 } (map-get? user-activity-count { user: user }))))
    (let ((next-id (+ (get count current-count) u1)))
      (map-set user-activity-count { user: user } { count: next-id })
      next-id
    )
  )
)

;; Update user's total carbon emissions
(define-private (update-user-carbon-total (user principal) (emission-value uint))
  (let ((current-total (default-to { total-emissions: u0 } (map-get? user-carbon-totals { user: user }))))
    (map-set user-carbon-totals 
      { user: user } 
      { total-emissions: (+ (get total-emissions current-total) emission-value) }
    )
  )
)

;; Check if activity type and subtype are valid
(define-private (is-valid-activity (activity-type uint) (activity-subtype uint))
  (and 
    (or 
      (is-eq activity-type ACTIVITY-TRANSPORT)
      (is-eq activity-type ACTIVITY-ENERGY)
      (is-eq activity-type ACTIVITY-DIET)
      (is-eq activity-type ACTIVITY-CONSUMPTION)
    )
    (> (get-carbon-factor activity-type activity-subtype) u0)
  )
)

;; =========================================
;; Read-Only Functions
;; =========================================

;; Get user's total carbon emissions
(define-read-only (get-user-carbon-total (user principal))
  (default-to { total-emissions: u0 } (map-get? user-carbon-totals { user: user }))
)

;; Get activity details by ID
(define-read-only (get-activity (user principal) (activity-id uint))
  (map-get? activity-records { user: user, timestamp: u0, activity-id: activity-id })
)

;; Get user's monthly emissions for a specific year and month
(define-read-only (get-monthly-emissions (user principal) (year uint) (month uint))
  (default-to { total: u0 } (map-get? monthly-emissions { user: user, year: year, month: month }))
)

;; Get user's carbon reduction goal
(define-read-only (get-user-goal (user principal))
  (map-get? user-goals { user: user })
)

;; Calculate progress towards goal (0-100%)
(define-read-only (calculate-goal-progress (user principal))
  (let ((goal (map-get? user-goals { user: user })))
    (if (is-none goal)
      (ok u0)
      (let (
        (unwrapped-goal (unwrap-panic goal))
        (current-emissions (get total-emissions (default-to { total-emissions: u0 } 
                                              (map-get? user-carbon-totals { user: user }))))
        (base-emissions (get base-emissions unwrapped-goal))
        (target-emissions (get target-emissions unwrapped-goal))
      )
        (if (>= base-emissions current-emissions)
          (let (
            (reduction (- base-emissions current-emissions))
            (needed-reduction (- base-emissions target-emissions))
          )
            (if (> needed-reduction u0)
              (let ((progress-percent (/ (* reduction u100) needed-reduction)))
                (if (> progress-percent u100)
                  (ok u100)
                  (ok progress-percent)))
              (ok u100)
            )
          )
          (ok u0)
        )
      )
    )
  )
)

;; Compare user's footprint to global/regional averages
(define-read-only (compare-to-average (user principal) (region uint))
  (let (
    (user-total (get total-emissions (default-to { total-emissions: u0 } 
                                    (map-get? user-carbon-totals { user: user }))))
    (annual-co2-kg (/ user-total u1000))
    (comparison-value
      (if (is-eq region u1) 
        NORTH-AMERICA-AVG-CARBON-FOOTPRINT
        (if (is-eq region u2) 
          EUROPE-AVG-CARBON-FOOTPRINT
          (if (is-eq region u3) 
            ASIA-AVG-CARBON-FOOTPRINT
            (if (is-eq region u4) 
              AFRICA-AVG-CARBON-FOOTPRINT
              GLOBAL-AVG-CARBON-FOOTPRINT))))
    )
  )
    (ok { 
      user-annual-kg: annual-co2-kg, 
      average-annual-kg: comparison-value,
      percentage-of-average: (if (> comparison-value u0)
                                (/ (* annual-co2-kg u100) comparison-value)
                                u0)
    })
  )
)

;; =========================================
;; Public Functions
;; =========================================

;; Delete a user's carbon reduction goal
(define-public (delete-carbon-goal)
  (let ((user tx-sender))
    (asserts! (is-some (map-get? user-goals { user: user })) ERR-NO-GOAL-EXISTS)
    
    (map-delete user-goals { user: user })
    
    (ok true)
  )
)