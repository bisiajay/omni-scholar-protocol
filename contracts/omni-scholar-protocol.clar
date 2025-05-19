;; Omni Scholar - Universal learning exchange framework.
;;
;; A decentralized knowledge-sharing ecosystem where participants can exchange wisdom credits, offer guidance sessions, and build reputation through a transparent, token-secured 


;; ========== SYSTEM ARCHITECTURE ==========


;; ===== COLLECTIVE LEARNING STRUCTURES =====
(define-map collective-sessions uint {facilitator: principal, participants: (list 10 principal), duration: uint, contribution: uint, status: (string-ascii 20)})
(define-data-var session-identifier uint u0) ;; Counter for collective sessions

;; ===== REPUTATION FRAMEWORK =====
(define-map provider-assessments {guide: principal, assessor: principal} uint) ;; Individual assessments
(define-map guide-reputation principal {cumulative-rating: uint, assessment-count: uint}) ;; Aggregated reputation

;; ===== PARTICIPANT DATA REPOSITORIES =====
(define-map wisdom-credit-balance principal uint) ;; Participant's wisdom credit balance
(define-map token-reserves principal uint) ;; Participant's token balance in protocol
(define-map offered-wisdom {provider: principal} {units: uint, valuation: uint}) ;; Available wisdom offerings

;; ===== VERIFICATION AND QUALITY ASSURANCE =====
(define-map verified-wisdom-providers principal bool) ;; Verified status of wisdom providers
(define-map premium-wisdom-offerings {provider: principal} {units: uint, valuation: uint, verified: bool}) ;; Premium offerings
;; ===== CORE ECONOMIC PARAMETERS =====
(define-data-var credit-acquisition-rate uint u10)  ;; Base cost for acquiring wisdom credits
(define-data-var participant-wisdom-ceiling uint u100) ;; Maximum wisdom credits a participant can hold
(define-data-var ecosystem-commission-rate uint u10)  ;; Platform transaction fee percentage
(define-data-var collective-wisdom-repository uint u0) ;; Total available wisdom in ecosystem
(define-data-var wisdom-repository-threshold uint u1000) ;; Maximum capacity of wisdom repository

;; ===== GOVERNANCE AND ERROR HANDLING =====
(define-constant governance-steward tx-sender) ;; Protocol administrator address
(define-constant error-governance-restricted (err u200)) ;; Only governance can perform operation
(define-constant error-insufficient-wisdom (err u201)) ;; Not enough wisdom credits available
(define-constant error-invalid-wisdom-parameters (err u202)) ;; Invalid wisdom credit parameters
(define-constant error-invalid-compensation (err u203)) ;; Invalid price or rate specified
(define-constant error-repository-capacity-exceeded (err u204)) ;; Wisdom repository at capacity
(define-constant error-access-violation (err u205)) ;; Unauthorized access attempt
(define-constant error-individual-capacity-reached (err u206)) ;; User at maximum capacity
(define-constant error-zero-quantity (err u207)) ;; Zero quantity not allowed
(define-constant error-excessive-commission (err u208)) ;; Commission too high
(define-constant error-threshold-minimum (err u209)) ;; Threshold cannot be zero
(define-constant error-repository-reduction (err u210)) ;; Cannot reduce repository capacity
(define-constant error-verification-required (err u211)) ;; User not verified for operation
(define-constant error-assessment-below-minimum (err u212)) ;; Rating below minimum threshold
(define-constant error-assessment-above-maximum (err u213)) ;; Rating exceeds maximum threshold
(define-constant error-incentive-below-minimum (err u214)) ;; Discount below minimum threshold
(define-constant error-incentive-above-maximum (err u215)) ;; Discount exceeds maximum threshold


;; ===== INCENTIVE STRUCTURES =====
(define-map wisdom-packages {provider: principal} {units: uint, valuation: uint, incentive-rate: uint}) ;; Discount packages

;; ========== UTILITY FUNCTIONS ==========

(define-private (update-wisdom-repository (units-change int))
  (let (
    (current-repository (var-get collective-wisdom-repository))
    (updated-repository (if (< units-change 0)
                     ;; If removing units, ensure we don't go below zero
                     (if (>= current-repository (to-uint (- 0 units-change)))
                         (- current-repository (to-uint (- 0 units-change)))
                         u0)
                     ;; If adding units
                     (+ current-repository (to-uint units-change))))
  )
    ;; Ensure we don't exceed the maximum capacity
    (asserts! (<= updated-repository (var-get wisdom-repository-threshold)) error-repository-capacity-exceeded)
    ;; Update the repository size
    (var-set collective-wisdom-repository updated-repository)
    (ok true)))

(define-private (determine-ecosystem-fee (transaction-value uint))
  (let ((fee-rate (var-get ecosystem-commission-rate)))
    (/ (* transaction-value fee-rate) u100)))

;; ========== CORE PROTOCOL FUNCTIONS ==========

;; Register new wisdom credits to participant's account
(define-public (acquire-wisdom-credits (units uint))
  (let (
    (participant tx-sender)
    (current-credits (default-to u0 (map-get? wisdom-credit-balance participant)))
    (max-allowed (var-get participant-wisdom-ceiling))
    (acquisition-cost (* units (var-get credit-acquisition-rate)))
    (participant-funds (default-to u0 (map-get? token-reserves participant)))
  )
    ;; Validate the input
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (<= (+ current-credits units) max-allowed) error-individual-capacity-reached)
    (asserts! (>= participant-funds acquisition-cost) error-insufficient-wisdom)

    ;; Update participant's wisdom and token balances
    (map-set wisdom-credit-balance participant (+ current-credits units))
    (map-set token-reserves participant (- participant-funds acquisition-cost))

    ;; Add funds to the governance's balance
    (map-set token-reserves governance-steward (+ (default-to u0 (map-get? token-reserves governance-steward)) acquisition-cost))

    (ok true)))

;; Make wisdom credits available for others to acquire
(define-public (share-wisdom (units uint) (valuation uint))
  (let (
    (current-credits (default-to u0 (map-get? wisdom-credit-balance tx-sender)))
    (currently-shared (get units (default-to {units: u0, valuation: u0} (map-get? offered-wisdom {provider: tx-sender}))))
    (total-shared (+ units currently-shared))
  )
    ;; Validate the input
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (> valuation u0) error-invalid-compensation)
    (asserts! (>= current-credits total-shared) error-insufficient-wisdom)

    ;; Update the global wisdom repository
    (try! (update-wisdom-repository (to-int units)))

    ;; Update the available wisdom map
    (map-set offered-wisdom {provider: tx-sender} {units: total-shared, valuation: valuation})

    (ok true)))

;; Acquire wisdom from another participant
(define-public (exchange-wisdom (provider principal) (units uint))
  (let (
    (offering (default-to {units: u0, valuation: u0} (map-get? offered-wisdom {provider: provider})))
    (exchange-value (* units (get valuation offering)))
    (ecosystem-fee (determine-ecosystem-fee exchange-value))
    (total-cost (+ exchange-value ecosystem-fee))
    (provider-credits (default-to u0 (map-get? wisdom-credit-balance provider)))
    (seeker-funds (default-to u0 (map-get? token-reserves tx-sender)))
    (provider-funds (default-to u0 (map-get? token-reserves provider)))
  )
    ;; Verify conditions
    (asserts! (not (is-eq tx-sender provider)) error-access-violation)
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (>= (get units offering) units) error-insufficient-wisdom)
    (asserts! (>= provider-credits units) error-insufficient-wisdom)
    (asserts! (>= seeker-funds total-cost) error-insufficient-wisdom)

    ;; Update provider's wisdom credit balance and available offerings
    (map-set wisdom-credit-balance provider (- provider-credits units))
    (map-set offered-wisdom {provider: provider} 
             {units: (- (get units offering) units), valuation: (get valuation offering)})

    ;; Update token balances
    (map-set token-reserves tx-sender (- seeker-funds total-cost))
    (map-set token-reserves provider (+ provider-funds exchange-value))
    (map-set wisdom-credit-balance tx-sender (+ (default-to u0 (map-get? wisdom-credit-balance tx-sender)) units))

    ;; Add commission to governance balance
    (map-set token-reserves governance-steward (+ (default-to u0 (map-get? token-reserves governance-steward)) ecosystem-fee))

    (ok true)))

;; Offer certified premium wisdom (requires verification)
(define-public (share-verified-wisdom (units uint) (valuation uint))
  (let (
    (current-credits (default-to u0 (map-get? wisdom-credit-balance tx-sender)))
    (is-verified (default-to false (map-get? verified-wisdom-providers tx-sender)))
    (currently-shared (get units (default-to {units: u0, valuation: u0} (map-get? offered-wisdom {provider: tx-sender}))))
    (total-shared (+ units currently-shared))
  )
    ;; Validate the input
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (> valuation u0) error-invalid-compensation)
    (asserts! is-verified error-verification-required)
    (asserts! (>= current-credits total-shared) error-insufficient-wisdom)

    ;; Update the global wisdom repository
    (try! (update-wisdom-repository (to-int units)))

    ;; Update regular wisdom offerings
    (map-set offered-wisdom {provider: tx-sender} {units: total-shared, valuation: valuation})

    ;; Update premium wisdom offerings
    (map-set premium-wisdom-offerings {provider: tx-sender} {units: units, valuation: valuation, verified: true})

    (ok true)))

;; Create a bundled package of wisdom credits at an incentive rate
(define-public (create-wisdom-package (units uint) (valuation uint) (incentive-rate uint))
  (let (
    (current-credits (default-to u0 (map-get? wisdom-credit-balance tx-sender)))
    (currently-shared (get units (default-to {units: u0, valuation: u0} (map-get? offered-wisdom {provider: tx-sender}))))
    (current-package (default-to {units: u0, valuation: u0, incentive-rate: u0} (map-get? wisdom-packages {provider: tx-sender})))
    (total-shared (+ units currently-shared))
    (total-packaged-units (+ units (get units current-package)))
  )
    ;; Validate the input
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (> valuation u0) error-invalid-compensation)
    (asserts! (> incentive-rate u0) error-incentive-below-minimum)
    (asserts! (<= incentive-rate u50) error-incentive-above-maximum)
    (asserts! (>= current-credits total-shared) error-insufficient-wisdom)

    ;; Update the global wisdom repository
    (try! (update-wisdom-repository (to-int units)))

    ;; Update wisdom availability
    (map-set offered-wisdom {provider: tx-sender} {units: total-shared, valuation: valuation})

    ;; Create or update the package offering
    (map-set wisdom-packages {provider: tx-sender} {
      units: total-packaged-units, 
      valuation: valuation, 
      incentive-rate: incentive-rate
    })

    (ok true)))

;; Initialize a collective wisdom session
(define-public (establish-collective-session (participants (list 10 principal)) (duration uint) (contribution uint))
  (let (
    (current-credits (default-to u0 (map-get? wisdom-credit-balance tx-sender)))
    (session-id (var-get session-identifier))
    (participant-count (len participants))
    (total-session-duration (* duration participant-count))
  )
    ;; Validate the input
    (asserts! (> duration u0) error-invalid-wisdom-parameters)
    (asserts! (> contribution u0) error-invalid-compensation)
    (asserts! (>= current-credits total-session-duration) error-insufficient-wisdom)

    ;; Update the wisdom repository
    (try! (update-wisdom-repository (to-int total-session-duration)))

    ;; Update facilitator's wisdom credit balance
    (map-set wisdom-credit-balance tx-sender (- current-credits total-session-duration))

    ;; Increment the session identifier
    (var-set session-identifier (+ session-id u1))

    (ok session-id)))

;; Assess a provider after acquiring wisdom
(define-public (assess-wisdom-provider (provider principal) (rating uint))
  (let (
    (provider-metrics (default-to {cumulative-rating: u0, assessment-count: u0} (map-get? guide-reputation provider)))
    (current-total (get cumulative-rating provider-metrics))
    (current-count (get assessment-count provider-metrics))
    (new-total (+ current-total rating))
    (new-count (+ current-count u1))
  )
    ;; Validate the input
    (asserts! (not (is-eq tx-sender provider)) error-access-violation)
    (asserts! (>= rating u1) error-assessment-below-minimum)
    (asserts! (<= rating u5) error-assessment-above-maximum)

    ;; Update the provider's rating data
    (map-set provider-assessments {guide: provider, assessor: tx-sender} rating)
    (map-set guide-reputation provider {cumulative-rating: new-total, assessment-count: new-count})

    (ok true)))

;; Deposit tokens into the protocol
(define-public (contribute-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? token-reserves tx-sender)))
    (new-balance (+ current-balance amount))
  )
    ;; Validate the input
    (asserts! (> amount u0) error-zero-quantity)

    ;; Transfer tokens from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update participant's token balance in the protocol
    (map-set token-reserves tx-sender new-balance)

    (ok true)))

;; Withdraw tokens from the protocol
(define-public (retrieve-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? token-reserves tx-sender)))
    (contract-balance (as-contract (stx-get-balance tx-sender)))
  )
    ;; Validate the input
    (asserts! (> amount u0) error-zero-quantity)
    (asserts! (>= current-balance amount) error-insufficient-wisdom)
    (asserts! (>= contract-balance amount) error-insufficient-wisdom)

    ;; Transfer tokens from contract to participant
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update participant's token balance in the protocol
    (map-set token-reserves tx-sender (- current-balance amount))

    (ok true)))

;; Reclaim shared wisdom that hasn't been acquired
(define-public (reclaim-shared-wisdom (units uint))
  (let (
    (offering (default-to {units: u0, valuation: u0} (map-get? offered-wisdom {provider: tx-sender})))
    (available-units (get units offering))
    (participant-credits (default-to u0 (map-get? wisdom-credit-balance tx-sender)))
  )
    ;; Validate the input
    (asserts! (> units u0) error-invalid-wisdom-parameters)
    (asserts! (>= available-units units) error-insufficient-wisdom)

    ;; Update the participant's shared wisdom
    (map-set offered-wisdom {provider: tx-sender} {
      units: (- available-units units),
      valuation: (get valuation offering)
    })

    ;; Update participant's wisdom credit balance
    (map-set wisdom-credit-balance tx-sender participant-credits)

    ;; Handle premium offerings if applicable
    (if (is-some (map-get? premium-wisdom-offerings {provider: tx-sender}))
        (let (
          (premium-offering (unwrap-panic (map-get? premium-wisdom-offerings {provider: tx-sender})))
          (premium-units (get units premium-offering))
        )
          (if (>= premium-units units)
              (map-set premium-wisdom-offerings {provider: tx-sender} {
                units: (- premium-units units),
                valuation: (get valuation premium-offering),
                verified: (get verified premium-offering)
              })
              (map-delete premium-wisdom-offerings {provider: tx-sender})
          )
        )
        true
    )

    (ok true)))

;; Update protocol configuration (governance only)
(define-public (reconfigure-protocol-parameters (new-credit-rate uint) 
                                             (new-commission-rate uint) 
                                             (new-participant-ceiling uint) 
                                             (new-repository-threshold uint))
  (begin
    ;; Verify governance privileges
    (asserts! (is-eq tx-sender governance-steward) error-governance-restricted)

    ;; Validate the input
    (asserts! (> new-credit-rate u0) error-invalid-compensation)
    (asserts! (<= new-commission-rate u30) error-excessive-commission)
    (asserts! (> new-participant-ceiling u0) error-threshold-minimum)
    (asserts! (>= new-repository-threshold (var-get collective-wisdom-repository)) error-repository-reduction)

    ;; Update the protocol configuration
    (var-set credit-acquisition-rate new-credit-rate)
    (var-set ecosystem-commission-rate new-commission-rate)
    (var-set participant-wisdom-ceiling new-participant-ceiling)
    (var-set wisdom-repository-threshold new-repository-threshold)

    (ok true)))

