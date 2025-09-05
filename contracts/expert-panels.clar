;; title: Expert Panel System
;; version: 1.0.0
;; summary: Specialized review panels for high-impact scientific papers
;; description: Enables formation of expert panels for thorough evaluation of complex research

;; constants
(define-constant err-not-found (err u401))
(define-constant err-unauthorized (err u402))
(define-constant err-panel-full (err u403))
(define-constant err-already-member (err u404))
(define-constant err-insufficient-expertise (err u405))
(define-constant err-panel-not-ready (err u406))
(define-constant err-consensus-not-reached (err u407))
(define-constant err-invitation-expired (err u408))

(define-constant min-expert-reputation u200) ;; Higher bar for panel experts
(define-constant max-panel-size u7)
(define-constant min-panel-size u3)
(define-constant panel-invitation-period u2016) ;; ~2 weeks
(define-constant consensus-threshold u70) ;; 70% agreement needed
(define-constant panel-review-bonus u25) ;; Extra reputation for panel reviews

;; data vars
(define-data-var next-panel-id uint u1)

;; data maps
(define-map expert-panels
  { panel-id: uint }
  {
    paper-id: uint,
    organizer: principal,
    specialty-area: (string-ascii 50),
    panel-size: uint,
    status: (string-ascii 20), ;; forming, active, completed
    creation-block: uint,
    completion-deadline: uint,
    consensus-score: (optional uint),
    panel-review-id: (optional uint)
  }
)

(define-map panel-members
  { panel-id: uint, expert: principal }
  {
    expertise-level: uint, ;; 1-5 scale
    individual-score: (optional uint),
    review-submitted: bool,
    joined-block: uint,
    contribution-weight: uint
  }
)

(define-map panel-invitations
  { panel-id: uint, invitee: principal }
  {
    invited-by: principal,
    invitation-block: uint,
    expires-at: uint,
    expertise-required: uint,
    status: (string-ascii 15)
  }
)

(define-map panel-consensus
  { panel-id: uint }
  {
    total-reviews: uint,
    weighted-score-sum: uint,
    consensus-reached: bool,
    final-recommendation: (optional (string-ascii 20)), ;; accept, reject, revise
    deliberation-notes: (optional (string-ascii 400))
  }
)

;; public functions

(define-public (create-expert-panel 
  (paper-id uint) 
  (specialty-area (string-ascii 50)) 
  (target-size uint) 
  (completion-deadline uint))
  (let
    (
      (panel-id (var-get next-panel-id))
      (paper (contract-call? .Peercred get-paper paper-id))
      (organizer-rep (contract-call? .Peercred get-user-reputation tx-sender))
    )
    ;; Validate paper exists and organizer has sufficient reputation
    (asserts! (is-some paper) err-not-found)
    (asserts! (>= organizer-rep min-expert-reputation) err-insufficient-expertise)
    (asserts! (and (>= target-size min-panel-size) (<= target-size max-panel-size)) err-panel-not-ready)
    (asserts! (> completion-deadline stacks-block-height) err-unauthorized)
    
    (map-set expert-panels
      { panel-id: panel-id }
      {
        paper-id: paper-id,
        organizer: tx-sender,
        specialty-area: specialty-area,
        panel-size: u1, ;; Start with organizer
        status: "forming",
        creation-block: stacks-block-height,
        completion-deadline: completion-deadline,
        consensus-score: none,
        panel-review-id: none
      }
    )
    
    ;; Add organizer as first panel member
    (map-set panel-members
      { panel-id: panel-id, expert: tx-sender }
      {
        expertise-level: u5, ;; Organizer gets max expertise level
        individual-score: none,
        review-submitted: false,
        joined-block: stacks-block-height,
        contribution-weight: u20 ;; Base weight for organizer
      }
    )
    
    (var-set next-panel-id (+ panel-id u1))
    (ok panel-id)
  )
)

(define-public (invite-panel-expert (panel-id uint) (expert principal) (required-expertise uint))
  (let
    (
      (panel (unwrap! (map-get? expert-panels { panel-id: panel-id }) err-not-found))
      (expert-rep (contract-call? .Peercred get-user-reputation expert))
    )
    ;; Only organizer or existing panel members can invite
    (asserts! (or (is-eq tx-sender (get organizer panel))
                  (is-some (map-get? panel-members { panel-id: panel-id, expert: tx-sender }))) err-unauthorized)
    (asserts! (is-eq (get status panel) "forming") err-panel-not-ready)
    (asserts! (< (get panel-size panel) max-panel-size) err-panel-full)
    (asserts! (>= expert-rep min-expert-reputation) err-insufficient-expertise)
    (asserts! (is-none (map-get? panel-members { panel-id: panel-id, expert: expert })) err-already-member)
    
    (map-set panel-invitations
      { panel-id: panel-id, invitee: expert }
      {
        invited-by: tx-sender,
        invitation-block: stacks-block-height,
        expires-at: (+ stacks-block-height panel-invitation-period),
        expertise-required: required-expertise,
        status: "pending"
      }
    )
    
    (ok true)
  )
)

(define-public (join-expert-panel (panel-id uint) (expertise-level uint))
  (let
    (
      (panel (unwrap! (map-get? expert-panels { panel-id: panel-id }) err-not-found))
      (invitation (unwrap! (map-get? panel-invitations { panel-id: panel-id, invitee: tx-sender }) err-not-found))
      (expert-rep (contract-call? .Peercred get-user-reputation tx-sender))
    )
    ;; Validate invitation and expertise
    (asserts! (is-eq (get status invitation) "pending") err-invitation-expired)
    (asserts! (< stacks-block-height (get expires-at invitation)) err-invitation-expired)
    (asserts! (>= expertise-level (get expertise-required invitation)) err-insufficient-expertise)
    (asserts! (and (>= expertise-level u1) (<= expertise-level u5)) err-insufficient-expertise)
    (asserts! (>= expert-rep min-expert-reputation) err-insufficient-expertise)
    (asserts! (< (get panel-size panel) max-panel-size) err-panel-full)
    
    ;; Accept invitation
    (map-set panel-invitations
      { panel-id: panel-id, invitee: tx-sender }
      (merge invitation { status: "accepted" })
    )
    
    ;; Add to panel
    (map-set panel-members
      { panel-id: panel-id, expert: tx-sender }
      {
        expertise-level: expertise-level,
        individual-score: none,
        review-submitted: false,
        joined-block: stacks-block-height,
        contribution-weight: (* expertise-level u10) ;; Weight based on expertise
      }
    )
    
    ;; Update panel size
    (map-set expert-panels
      { panel-id: panel-id }
      (merge panel { panel-size: (+ (get panel-size panel) u1) })
    )
    
    (ok true)
  )
)

(define-public (submit-panel-review (panel-id uint) (individual-score uint) (review-notes (string-ascii 300)))
  (let
    (
      (panel (unwrap! (map-get? expert-panels { panel-id: panel-id }) err-not-found))
      (member (unwrap! (map-get? panel-members { panel-id: panel-id, expert: tx-sender }) err-unauthorized))
    )
    ;; Validate panel status and member eligibility
    (asserts! (is-eq (get status panel) "active") err-panel-not-ready)
    (asserts! (not (get review-submitted member)) err-already-member)
    (asserts! (and (>= individual-score u1) (<= individual-score u10)) (err u105))
    (asserts! (< stacks-block-height (get completion-deadline panel)) err-invitation-expired)
    
    ;; Record individual review
    (map-set panel-members
      { panel-id: panel-id, expert: tx-sender }
      (merge member {
        individual-score: (some individual-score),
        review-submitted: true
      })
    )
    
    ;; Update consensus tracking
    (let
      (
        (consensus-data (default-to { total-reviews: u0, weighted-score-sum: u0, consensus-reached: false, 
                                     final-recommendation: none, deliberation-notes: none }
                                   (map-get? panel-consensus { panel-id: panel-id })))
        (weighted-score (* individual-score (get contribution-weight member)))
        (new-total (+ (get total-reviews consensus-data) u1))
        (new-weighted-sum (+ (get weighted-score-sum consensus-data) weighted-score))
      )
      (map-set panel-consensus
        { panel-id: panel-id }
        (merge consensus-data {
          total-reviews: new-total,
          weighted-score-sum: new-weighted-sum
        })
      )
      
      ;; Check if consensus can be calculated
      (try! (if (>= new-total min-panel-size)
               (calculate-panel-consensus panel-id)
               (ok true)
             ))
    )
    
    ;; Award enhanced reputation for panel participation
    (let ((current-rep (contract-call? .Peercred get-user-reputation tx-sender)))
      (try! (contract-call? .Peercred initialize-reputation tx-sender (+ current-rep panel-review-bonus)))
      (ok true)
    )
  )
)

(define-public (finalize-panel-review (panel-id uint) (recommendation (string-ascii 20)) (notes (string-ascii 400)))
  (let
    (
      (panel (unwrap! (map-get? expert-panels { panel-id: panel-id }) err-not-found))
      (consensus-data (unwrap! (map-get? panel-consensus { panel-id: panel-id }) err-not-found))
    )
    ;; Only organizer can finalize
    (asserts! (is-eq tx-sender (get organizer panel)) err-unauthorized)
    (asserts! (get consensus-reached consensus-data) err-consensus-not-reached)
    
    ;; Update panel status
    (map-set expert-panels
      { panel-id: panel-id }
      (merge panel { status: "completed" })
    )
    
    ;; Record final recommendation
    (map-set panel-consensus
      { panel-id: panel-id }
      (merge consensus-data {
        final-recommendation: (some recommendation),
        deliberation-notes: (some notes)
      })
    )
    
    (ok true)
  )
)

;; read-only functions

(define-read-only (get-expert-panel (panel-id uint))
  (map-get? expert-panels { panel-id: panel-id })
)

(define-read-only (get-panel-member (panel-id uint) (expert principal))
  (map-get? panel-members { panel-id: panel-id, expert: expert })
)

(define-read-only (get-panel-invitation (panel-id uint) (invitee principal))
  (map-get? panel-invitations { panel-id: panel-id, invitee: invitee })
)

(define-read-only (get-panel-consensus (panel-id uint))
  (map-get? panel-consensus { panel-id: panel-id })
)

(define-read-only (get-next-panel-id)
  (var-get next-panel-id)
)

(define-read-only (calculate-panel-consensus-score (panel-id uint))
  (let
    (
      (consensus-data (map-get? panel-consensus { panel-id: panel-id }))
    )
    (match consensus-data
      data (if (> (get total-reviews data) u0)
             (ok (/ (get weighted-score-sum data) (get total-reviews data)))
             (ok u0))
      err-not-found)
  )
)

;; private functions

(define-private (calculate-panel-consensus (panel-id uint))
  (let
    (
      (consensus-data (unwrap! (map-get? panel-consensus { panel-id: panel-id }) err-not-found))
      (panel (unwrap! (map-get? expert-panels { panel-id: panel-id }) err-not-found))
      (total-reviews (get total-reviews consensus-data))
    )
    ;; Check if minimum reviews received
    (if (>= total-reviews min-panel-size)
      (let
        (
          (consensus-score (/ (get weighted-score-sum consensus-data) total-reviews))
          (consensus-reached (>= (/ (* total-reviews u100) (get panel-size panel)) consensus-threshold))
        )
        ;; Update panel with consensus
        (map-set expert-panels
          { panel-id: panel-id }
          (merge panel { 
            status: (if consensus-reached "active" "forming"),
            consensus-score: (some consensus-score)
          })
        )
        
        (map-set panel-consensus
          { panel-id: panel-id }
          (merge consensus-data { consensus-reached: consensus-reached })
        )
        
        (ok consensus-reached)
      )
      (ok false)
    )
  )
)
